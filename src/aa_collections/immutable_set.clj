(ns aa-collections.immutable-set
  (:import (clojure.lang RT)))

(defn nada? [x] (or (nil? x) (zero? (.-level x))))

(defprotocol AASetInternal
  (skew [this])
  (split [this])
  )

(declare iskew isplit)

(deftype AASet [level left right value comparator nada]
  AASetInternal
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

(defn aa-empty-set
  ([] (aa-empty-set RT/DEFAULT_COMPARATOR))
  ([comparator] (->AASet 0 nil nil nil comparator nil)))

(defn- iskew
  [this]
  (cond
    (nada? this)
    this
    (nada? (.-left this))
    this
    (= (.-level (.-left this)) (.-level this))
    (let [l (.-left this)]
      (->AASet
        (.-level l)
        (.-left l)
        (->AASet
          (.-level this)
          (.-right l)
          (.-right this)
          (.-value this)
          (.-comparator this)
          (.-nada this))
        (.-value l)
        (.-comparator l)
        (.-nada l)))
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
      (->AASet
        (.-level r)
        (->AASet
          (.-level this)
          (.-left this)
          (.-left r)
          (.-value this)
          (.-comparator this)
          (.-nada this))
        (.-right r)
        (.-value r)
        (.-comparator r)
        (.-nada r)))
    :else
    this))
