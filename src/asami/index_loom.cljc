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

(defn nodes* [{:keys [spo osp] :as graph}]
  (into (set (keys spo)) (keys osp)))

(defn edges* [{:keys [osp] :as graph}]
  (let [edge-pairs (for [o (keys osp) s (keys (osp o))]
                     (if (get-in osp [s o]) [[s o]] [[s o] [o s]]))]
    (apply concat edge-pairs)))

(defn has-node?* [{:keys [spo osp] :as graph} node]
  (boolean (or (spo node) (osp node))))

(defn has-edge?* [{:keys [osp] :as graph} n1 n2]
  (boolean (get-in osp [n2 n1])))

(defn successors** [{:keys [spo] :as graph} node]
  (apply set/union (vals (spo node))))

(defn out-degree* [{:keys [spo] :as graph} node]
  ;; drops duplicates for different predicates!
  (count (apply set/union (vals (spo node)))))

(defn out-edges*
  [{:keys [spo] :as graph} node]
  (for [o (apply set/union (vals (spo node)))] [node o]))


#?(:clj
   (extend-type GraphIndexed
     loom/Graph
     (nodes [g] (nodes* g))
     (edges [g] (edges* g))
     (has-node? [g node] (has-node?* g node))
     (has-edge? [g n1 n2] (has-edge?* g n1 n2))
     (successors* [g node] (successors** g node))
     (out-degree [g node] (out-degree* g node))
     (out-edges [g node] (out-edges* g node))))

#?(:cljs
   (extend-type index/GraphIndexed
     loom/Graph
     (nodes [g] (nodes* g))
     (edges [g] (edges* g))
     (has-node? [g node] (has-node?* g node))
     (has-edge? [g n1 n2] (has-edge?* g n1 n2))
     (successors* [g node] (successors** g node))
     (out-degree [g node] (out-degree* g node))
     (out-edges [g node] (out-edges* g node))))

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
