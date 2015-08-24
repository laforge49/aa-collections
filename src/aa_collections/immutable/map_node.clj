(ns aa-collections.immutable.map-node
  (:import (clojure.lang RT)))

(deftype MapNode [t2 level left right cnt comparator nada])

(defn emty? [x]
  (or (nil? x) (zero? (.-level x))))

(defn emty [this]
  (if (emty? this)
    this
    (.-nada this)))

(defn cmpr [this x]
  (.compare (.-comparator this) x (.getKey (.-t2 this))))

(defn cntr [this]
  (if (emty? this)
    0
    (.-cnt this)))

(defn right-node [this]
  (if (emty? (.-right this))
    (emty this)
    (.-right this)))

(defn- left-node [this]
  (if (emty? (.-left this))
    (emty this)
    (.-left this)))

(defn emty-node
  ([] (emty-node RT/DEFAULT_COMPARATOR))
  ([comparator] (->MapNode nil 0 nil nil 0 comparator nil)))

(defn new-node
  [this t2 level left right cnt]
  (->MapNode t2 level left right cnt (.-comparator this) (emty this)))
