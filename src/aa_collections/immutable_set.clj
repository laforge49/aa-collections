(ns aa-collections.immutable-set
  (:import (clojure.lang RT)))

(defprotocol AASetInternal
  (left-node [this])
  (right-node [this])
  (new-node [this value level left right])
  (revise [this & args])
  (skew [this])
  (split [this])
  )

(declare ->AASet)

(defn nada? [x] (or (nil? x) (zero? (.-level x))))

(defn aa-empty-set
  ([] (aa-empty-set RT/DEFAULT_COMPARATOR))
  ([comparator] (->AASet nil 0 nil nil comparator nil)))

(defn- inew-node
  [this value level left right]
  (->AASet value level left right (.-comparator this) (empty this)))

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
      (.revise r :level (+ (.-level r) 1) :left (.revise this :right (.-left r))))
    :else
    this))

(defn- iinsert
  [this x]
  (if (nada? this)
    (new-node this x 1 nil nil)
    (let [c (.compare (.-comparator this) x (.-value this))]
      (.split (.skeq (cond
        (< c 0)
        (.revise this :left (iinsert (left-node this) x))
        (> c 0)
        (.revise this :right (iinsert (right-node this) x)))))))
  )

(deftype AASet [value level left right comparator nada]
  AASetInternal
  (left-node [this]
    (if (nada? left)
      (empty this)
      left))
  (right-node [this]
    (if (nada? right)
      (empty this)
      right))
  (new-node [this val lvl l r] (inew-node this val lvl l r))
  (revise [this & args] (irevise this args))
  (skew [this] (iskew this))
  (split [this] (isplit this))

  clojure.lang.IPersistentSet
  (seq [_] nil)
  (count [_] 0)
  (cons [this x] (iinsert this x))
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
