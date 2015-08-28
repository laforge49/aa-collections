(ns aa-collections.immutable.map-iterator
  (:require [aa-collections.immutable.map-node :refer :all])
  (:import (clojure.lang Counted IMapEntry)
           (java.util Iterator)
           (aa_collections.immutable.imap_node IMapNode)
           (aa_collections.immutable MapSequence)))

(deftype map-entry-iterator [^IMapNode node
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

(defn ^map-entry-iterator new-map-entry-iterator [^IMapNode node]
  (->map-entry-iterator node nil (.-cnt node)))

(defn ^MapSequence new-map-entry-seq [^IMapNode node]
  (MapSequence/create (new-map-entry-iterator node) identity))

(defn key-of [^IMapEntry e] (.getKey e))

(defn ^MapSequence new-map-key-seq [^IMapNode node]
  (MapSequence/create (new-map-entry-iterator node) key-of))

(defn value-of [^IMapEntry e] (.getValue e))

(defn ^MapSequence new-map-value-seq [^IMapNode node]
  (MapSequence/create (new-map-entry-iterator node) value-of))
