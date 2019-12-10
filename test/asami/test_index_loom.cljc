(ns asami.test-index-loom
  "Tests basic Loom functions"
  (:require [loom.graph :as loom :refer [nodes edges has-node? has-edge?
                                         transpose]]
            [asami.index-loom :as aloom :refer [graph]]
            #?(:clj  [schema.core :as s]
               :cljs [schema.core :as s :include-macros true])
            #?(:clj  [clojure.test :refer [is are use-fixtures testing]]
               :cljs [clojure.test :refer-macros [is are run-tests use-fixtures testing]])
            #?(:clj  [schema.test :as st :refer [deftest]]
               :cljs [schema.test :as st :refer-macros [deftest]])))

(deftest build-graph-test
  (let [g1 (graph [1 2] [1 3] [2 3] 4)
        g2 (graph {1 [2 3] 2 [3] 4 []})
        g3 (graph g1)
        g4 (graph g3 (graph [5 6]) [7 8] 9)
        g5 (graph)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (nodes g1))
           #{[1 2] [1 3] [2 3]} (set (edges g1))
           (set (nodes g2)) (set (nodes g1))
           (set (edges g2)) (set (edges g1))
           (set (nodes g3)) (set (nodes g1))
           (set (nodes g3)) (set (nodes g1))
           #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
           #{[1 2] [1 3] [2 3] [5 6] [7 8]} (set (edges g4))
             #{} (set (nodes g5))
             #{} (set (edges g5))
             true (has-edge? g1 1 2)
             false (has-node? g1 5)
             false (has-edge? g1 4 1)))))

(deftest simple-digraph-test
  (let [g1 (graph [1 2] [1 3] [2 3] 4)
        g2 (graph {1 [2 3] 2 [3] 4 []})
        g3 (graph g1)
        g4 (graph g3 (graph [5 6]) [7 8] 9)
        g5 (graph)
        g6 (transpose g1)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (nodes g1))
           #{1 2 3 4} (set (nodes g6))
           #{[1 2] [1 3] [2 3]} (set (edges g1))
           #{[2 1] [3 1] [3 2]} (set (edges g6))
           (set (nodes g2)) (set (nodes g1))
           (set (edges g2)) (set (edges g1))
           (set (nodes g3)) (set (nodes g1))
           (set (nodes g3)) (set (nodes g1))
           #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
           #{[1 2] [1 3] [2 3] [5 6] [7 8]} (set (edges g4))
           #{} (set (nodes g5))
           #{} (set (edges g5))
           true (has-node? g1 4)
           true (has-edge? g1 1 2)
           false (has-node? g1 5)
           false (has-edge? g1 2 1)))))

#?(:cljs (run-tests))
