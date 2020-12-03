(ns ^{:doc "Tests the Data Pool"
      :author "Paula Gearon"}
    asami.durable.pool-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as s]
            [asami.durable.common :refer [close find-object find-id write! at
                                          get-object]]  ;; TODO remove
            [asami.durable.pool :refer [create-pool id-offset-long]]

            [asami.durable.tree :as tree :refer [get-child left node-seq]]
            [asami.durable.block.block-api :refer [get-long get-id get-bytes]]
            [asami.durable.encoder :refer [to-bytes]]
            [asami.durable.decoder :refer [type-info]])
  #?(:clj (:import [java.io File])))

(defn recurse-delete
  [s]
  #?(:clj
     (letfn [(remove [f]
               (when (.isDirectory f)
                 (doseq [file (into [] (.listFiles f))]
                   (remove file)))
               (.delete f))]
       (remove (File. s)))))

(deftest test-creation
  (let [pool (create-pool "empty-test")]
    (close pool)
    (recurse-delete "empty-test")))

(deftest test-encapsulate
  (let [pool (create-pool "empty-test2")
        [a pool] (write! pool "one")
        [b pool] (write! pool "two")
        [c pool] (write! pool "three")
        [d pool] (write! pool "four")
        [e pool] (write! pool "five")
        [f pool] (write! pool "six")
        [g pool] (write! pool :seven)
        [h pool] (write! pool :eight)
        [i pool] (write! pool :nine)
        [j pool] (write! pool :ten)]
    (is (= "one" (find-object pool a)))
    (is (= "two" (find-object pool b)))
    (is (= "three" (find-object pool c)))
    (is (= "four" (find-object pool d)))
    (is (= "five" (find-object pool e)))
    (is (= "six" (find-object pool f)))
    (is (= :seven (find-object pool g)))
    (is (= :eight (find-object pool h)))
    (is (= :nine (find-object pool i)))
    (is (= :ten (find-object pool j)))
    (close pool)
    (recurse-delete "empty-test2")))

(defn find-node
  [{index :index} s]
  (let [[header body] (to-bytes s)]
    (tree/find-node index [^byte (type-info (aget header 0)) header body s])))

(defn first-node
  [{{root :root :as index} :index}]
  (loop [n root]
    (if-let [nn (get-child n index left)]
      (recur nn)
      n)))

(deftest test-storage
  (let [pool (create-pool "pool-test")
        data ["abcdefgh"
              ".......one"
              ".......two"
              ".......three hundred and twenty-five"
              ".......four hundred and thirty-six"
              ".......five hundred and forty-seven"
              ".......six hundred and fifty-eight"
              :seven-hundred-and-one
              :eight-hundred-and-two
              :nine-hundred-and-three
              :ten-hundred-and-four]
        [ids pool] (reduce (fn [[ids p] d]
                             (let [[id p'] (write! p d)]
                               [(conj ids id) p']))
                           [[] pool] data)
        root (:root-id pool)
        [ids2 pool] (reduce (fn [[ids p] d]
                              (let [[id p'] (write! p d)]
                                [(conj ids id) p']))
                            [[] pool] data)]
    (is (= ids ids2))
    (is (= root (:root-id pool)))
    (doseq [[id value] (map vector ids data)]
      (is (= value (find-object pool id))))

    (doseq [[id value] (map vector ids data)]
      (is (= id (find-id pool value)) (str "data: " value)))
    (close pool)

    (let [pool2 (create-pool "pool-test" root)]
      (doseq [[id value] (map vector ids data)]
        (is (= value (find-object pool2 id))))

      (doseq [[id value] (map vector ids data)]
        (is (= id (find-id pool2 value)) (str "data: " value)))))

  (recurse-delete "pool-test"))

(defn load-strings!
  "Loads words into the pool. Returns a pair of the IDs for the words (in order) and the new pool"
  [words pool]
  (reduce (fn [[ids p] w]
            (let [[id p'] (write! p w)]
              [(conj ids id) p']))
          [[] pool]
          words))


(deftest test-words
  (let [book          (slurp "test/resources/pride_and_prejudice.txt") 
        words         (s/split book #"\s")
        pool          (create-pool "book2")
        [coded bpool] (load-strings! words pool)
        root          (:root-id bpool)
        g             (find-id bpool "Gutenberg")
        output-words  (map #(find-object bpool %) coded)]
    (is (= "Gutenberg" (find-object bpool g)))
    (is (= words output-words))

    (close bpool)

    (let [new-pool (create-pool "book2" root)
          g2 (find-id new-pool "Gutenberg")
          output-words2 (map #(find-object new-pool %) coded)
          [coded2 bpool2] (load-strings! words new-pool)]
      (is (= g g2))
      (is (= words output-words2))
      ;; the following should not have changed, since the same words were loaded
      (is (= coded coded2))
      (is (= root (:root-id bpool2)))
      (close bpool2)))
  (recurse-delete "book2"))


(comment
  (let [_             (prn "============== asami ==============")
        book          (slurp "test/resources/pride_and_prejudice.txt")
        words         (set (s/split book #"\s"))
        _             (prn "creating pool for " (count words))
        pool          (time (create-pool "book2"))
        _             (prn "loading data")
        [coded bpool] (time (load-strings! words pool))
        _             (prn "loaded data")
        root          (:root-id bpool)
                                        ;g             (find-id bpool "Gutenberg")
        _             (prn "reading all data")
        output-words  (time (doall (map #(find-object bpool %) coded)))
        _             (prn "read all data" (count output-words))]
    (close bpool)
    (recurse-delete "book2")
    :done
    )


  (require '[hitchhiker.tree.utils.async :as ha :include-macros true]
           '[hitchhiker.tree.bootstrap.konserve :as kons]
           '[hitchhiker.tree :as core]
           '[hitchhiker.tree.messaging :as msg]
           '[konserve.filestore :refer [new-fs-store delete-store]]
           '[konserve.cache :as kc]
           '[clojure.core.async :refer [promise-chan] :as async])


  (let [_             (prn "============== hh-tree ==============")
        book          (slurp "test/resources/pride_and_prejudice.txt")
        words         (set (s/split book #"\s"))
        _             (prn "creating store for " (count words) (count (set words)))
        folder "/tmp/async-hitchhiker-tree-test"
        _ (delete-store folder)
        store (kons/add-hitchhiker-tree-handlers
               (kc/ensure-cache (async/<!! (new-fs-store folder :config {:fsync true}))))
        backend (kons/->KonserveBackend store)
        _             (prn "loading data")
        datahike-config (core/->Config 17 300 (- 300 17))
        faster-config (core/->Config 30 900 (- 900 30))
        flushed       (ha/<?? (core/flush-tree
                               (time (reduce (fn [t i]
                                               (ha/<?? (msg/insert t i i)))
                                             (ha/<?? (core/b-tree faster-config))
                                             words))
                               backend))
        _             (prn "loaded data")
        root-key (kons/get-root-key (:tree flushed))
        ;; reload
        tree (ha/<? (kons/create-tree-from-root-key store root-key))
        _             (prn "reading all data")
        output-words  (time (doall (msg/lookup-fwd-iter tree "")))
        _             (prn "read all data" (count output-words))]
    :done
    )


  (let [folder "/tmp/async-hitchhiker-tree-test"
             _ (delete-store folder)
             store (kons/add-hitchhiker-tree-handlers
                    (kc/ensure-cache (async/<!! (new-fs-store folder :config {:fsync true}))))
             backend (kons/->KonserveBackend store)
             flushed (ha/<?? (core/flush-tree
                              (time (reduce (fn [t i]
                                              (ha/<?? (msg/insert t i i)))
                                            (ha/<?? (core/b-tree (core/->Config 1 3 (- 3 1))))
                                            (range 1 11)))
                              backend))
             root-key (kons/get-root-key (:tree flushed))
             tree (ha/<?? (kons/create-tree-from-root-key store root-key))]
         (is (= (ha/<?? (msg/lookup tree -10)) nil))
         (is (= (ha/<?? (msg/lookup tree 100)) nil))
         (dotimes [i 10]
           (is (= (ha/<?? (msg/lookup tree (inc i))) (inc i))))
         (is (= (map first (msg/lookup-fwd-iter tree 4)) (range 4 11)))
         (is (= (map first (msg/lookup-fwd-iter tree 0)) (range 1 11)))
         (let [deleted (ha/<?? (core/flush-tree (ha/<?? (msg/delete tree 3)) backend))
               root-key (kons/get-root-key (:tree deleted))
               tree (ha/<?? (kons/create-tree-from-root-key store root-key))]
           (is (= (ha/<?? (msg/lookup tree 2)) 2))
           (is (= (ha/<?? (msg/lookup tree 3)) nil))
           (is (= (ha/<?? (msg/lookup tree 4)) 4)))
         (delete-store folder)) 


  ;; REPL output
  ;; "============== asami =============="
  ;; "creating pool for " 13779
  ;; "Elapsed time: 19.256731 msecs"
  ;; "loading data"
  ;; "Elapsed time: 14618.803674 msecs"
  ;; "loaded data"
  ;; "reading all data"
  ;; "Elapsed time: 1189.145622 msecs"
  ;; "read all data" 13779
  ;; "============== hh-tree =============="
  ;; "creating store for " 13779 13779
  ;; "loading data"
  ;; "Elapsed time: 155.655257 msecs"
  ;; "loaded data"
  ;; "reading all data"
  ;; "Elapsed time: 72.265827 msecs"
  ;; "read all data" 13779


  )
