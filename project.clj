(defproject org.clojars.quoll/asami "1.2.6"
  :description "An in memory graph store for Clojure and ClojureScript"
  :url "http://github.com/threatgrid/asami"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.773"]
                 [prismatic/schema "1.1.12"] 
                 [org.clojure/core.cache "0.8.2"]
                 [org.clojars.quoll/zuko "0.3.2"]
                 [tailrecursion/cljs-priority-map "1.2.1"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [io.replikativ/hitchhiker-tree "0.1.9"]
                 [io.replikativ/konserve "0.6.0-alpha1"]
                 ]
  :plugins [[lein-cljsbuild "1.1.7"]
            [cider/cider-nrepl "0.24.0"]]
  :cljsbuild {
    :builds {
      :dev
      {:source-paths ["src"]
       :compiler {
         :output-to "out/asami/core.js"
         :optimizations :simple
         :pretty-print true}}
      :test
      {:source-paths ["src" "test"]
       :compiler {
         :output-to "out/asami/test_memory.js"
         :optimizations :simple
         :pretty-print true}}
      }
    :test-commands {
      "unit" ["node" "out/asami/test_memory.js"]}
    })
