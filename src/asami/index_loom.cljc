(ns ^{:doc "An in-memory graph implementation with full indexing."
      :author "Paula Gearon"}
    asami.index-loom
  (:require [asami.graph :as gr :refer [graph-add]]
            [clojure.set :as set]
            [loom.graph :as loom :refer [nodes edges has-node? successors*
                                         out-degree out-edges]]
            #?(:clj  [asami.index :as index]
               :cljs [asami.index :as index :refer [GraphIndexed]])
            #?(:clj  [schema.core :as s]
               :cljs [schema.core :as s :include-macros true]))
  #?(:clj (:import [asami.index GraphIndexed])))

(extend-type GraphIndexed
  loom/Graph
  (nodes [{:keys [spo osp] :as graph}]
    (into (set (keys spo)) (keys osp)))

  (edges [{:keys [osp] :as graph}]
    (let [edge-pairs (for [o (keys osp) s (keys (osp o))]
                       (if (get-in osp [s o]) [[s o]] [[s o] [o s]]))]
      (apply concat edge-pairs)))

  (has-node? [{:keys [spo osp] :as graph} node]
    (boolean (or (spo node) (osp node))))

  (has-edge? [{:keys [osp] :as graph} n1 n2]
    (boolean (get-in osp [n2 n1])))

  (successors* [{:keys [spo] :as graph} node]
    (apply set/union (vals (spo node))))

  (out-degree [{:keys [spo] :as graph} node]
    ;; drops duplicates for different predicates!
    (count (apply set/union (vals (spo node)))))

  (out-edges [{:keys [spo] :as graph} node]
    "Returns all the outgoing edges of node"
    (for [o (apply set/union (vals (spo node)))] [node o])))

(defn add-edge
  [g [s o :as edge]]
  (graph-add g s :to o))

(defn add-edges
  [g & edges]
  (reduce add-edge g edges))

(defn add-edges*
  [g edges]
  (apply add-edges g edges))

(defn build-graph
  "Builds up a graph (i.e. adds edges and nodes) from any combination of
  other graphs, adjacency maps, edges, or nodes. Modified form of the
  equivalent function in Loom."
  [g & inits]
  (letfn [(build [g init]
            (cond
             ;; graph
             (satisfies? loom/Graph init)
             (add-edges* g (edges init))
             ;; adacency map
             (map? init)
             (add-edges* g (for [[n nbrs] init nbr nbrs] [n nbr]))
             ;; edge
             (sequential? init) (add-edges g init)
             ;; node - ignore
             :else g))]
    (reduce build g inits)))

(defn graph
  "Creates an index graph with a set of edges. All edges are unlabelled."
  [& inits]
  (apply build-graph index/empty-graph inits))
