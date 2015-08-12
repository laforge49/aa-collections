(ns aa-collections.immutable-set
  (:import (clojure.lang RT)))

(declare ->AASet)

(defn nada? [x] (or (nil? x) (zero? (.-level x))))

(defn aa-empty-set
  ([] (aa-empty-set RT/DEFAULT_COMPARATOR))
  ([comparator] (->AASet nil 0 nil nil comparator nil)))

(defn- irevise
  [this & args]
  (let [m (apply array-map args)]
    (->AASet
      (get m :value (.-value this))
      (get m :level (.-level this))
      (get m :left (.-left this))
      (get m :right (.-right this))
      (.-comparator this)
      (.-nada this))))

(defn- iskew
  [this]
  (cond
    (nada? this)
    this
    (nada? (.-left this))
    this
    (= (.-level (.-left this)) (.-level this))
    (let [l (.-left this)]
      (.revise l :right (.revise this :left (.-right l))))
    :else
    this))

(defn- isplit
  [this]
  (cond
    (nada? this)
    this
    (or (nada? (.-right this)) (nada? (.-right (.-right this))))
    this
    (= (.-level this) (.-level (.-right (.-right this))))
    (let [r (.-right this)]
      (.revise r :left (.revise this :right (.-left r))))
    :else
    this))

(defprotocol AASetInternal
  (revise [this & args])
  (skew [this])
  (split [this])
  )

(deftype AASet [value level left right comparator nada]
  AASetInternal
  (revise [this & args] (irevise this args))
  (skew [this] (iskew this))
  (split [this] (isplit this))

  clojure.lang.IPersistentSet
  (seq [_] nil)
  (count [_] 0)
  (cons [_ o] nil)
  (empty [this]
    (if (zero? level)
      this
      nada))
  (equiv [_ o] false)
  (disjoin [_ key] nil)
  (contains [_ key] false)
  (get [_ key] nil)

  clojure.lang.Reversible
  (rseq [_] nil)
  )
