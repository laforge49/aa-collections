(ns aa-collections.immutable.map-iterator
  (:require [aa-collections.immutable.map-node :refer :all])
  (:import (clojure.lang Counted IMapEntry)
           (java.util Iterator)
           (aa_collections.immutable.imap_node IMapNode)
           (aa_collections.immutable MapSequence)))

(deftype map-iterator [^IMapNode node
                       ^{:volatile-mutable true IMapEntry true} lst
                       ^{:volatile-mutable true int true} cnt]
  Iterator
  (hasNext [this]
    (> cnt 0))
  (next [this]
    (if (nil? lst)
      (set! lst (first-t2 node))
      (set! lst (.next-t2 node (.getKey lst))))
    (set! cnt (- cnt 1))
    lst)

  Counted
  (count [this] cnt))

(defn new-map-iterator [node]
  (->map-iterator node nil (.-cnt node)))

(defn ^MapSequence new-map-seq [^IMapNode node]
  (MapSequence/create (->map-iterator node nil (.-cnt node))))
