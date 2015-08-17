(ns aa-collections.immutable-set
  (:import (clojure.lang RT Counted ISeq Sequential IPersistentCollection Seqable)
           (java.util List)))

(defprotocol IAASetNode
  (sfirst [this])
  (slast [this])
  (snext [this x])
  (sget [this x])
  (left-node [this])
  (right-node [this])
  (new-node [this value level left right cnt])
  (revise [this & args])
  (skew [this])
  (split [this])
  (insert [this x])
  (emty [this])
  (to-str [this])
  )

(declare ->AASetNode)

(defn nada? [x]
  (or (nil? x) (zero? (.-level x))))

(defn aa-empty-set-node
  ([] (aa-empty-set-node RT/DEFAULT_COMPARATOR))
  ([comparator] (->AASetNode nil 0 nil nil 0 comparator nil)))

(defn- inew-node
  [this value level left right cnt]
  (->AASetNode value level left right cnt (.-comparator this) (emty this)))

(defn cntr [this]
  (if (nada? this)
    0
    (.-cnt this)))

(defn- irevise
  [this & args]
  (let [m (apply array-map args)
        l (get m :left (.-left this))
        r (get m :right (.-right this))
        c (+ 1 (cntr l) (cntr r))]
    (->AASetNode
      (get m :value (.-value this))
      (get m :level (.-level this))
      l
      r
      c
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
      (irevise l :right (irevise this :left (.-right l))))
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
      (irevise r
               :level (+ (.-level r) 1)
               :left (irevise this :right (.-left r))))
    :else
    this))

(defn- iinsert
  [this x]
  (if (nada? this)
    (new-node this x 1 nil nil 1)
    (let [c (.compare (.-comparator this) x (.-value this))]
      (isplit (iskew (cond
                       (< c 0)
                       (let [ol (left-node this)
                             l (iinsert ol x)]
                         (if (identical? ol l)
                           this
                           (irevise this :left l)))
                       (> c 0)
                       (let [or (right-node this)
                             r (insert or x)]
                         (if (identical? or r)
                           this
                           (irevise this :right r))))))))
  )

(deftype AASetNode [value level left right cnt comparator nada]
  IAASetNode
  (sfirst [this]
    (cond
      (nada? this) nil
      (nada? left) value
      :else (sfirst left)))
  (slast [this]
    (cond
      (nada? this) nil
      (nada? right) value
      :else (slast right)))
  (snext [this x]
    (if (nada? this)
      nil
      (let [c (.compare comparator x value)]
        (cond
          (zero? c) (sfirst (right-node this))
          (> c 0) (snext (right-node this) x)
          :else (let [n (snext (left-node this) x)]
                  (if (nil? n)
                    value
                    n))))))
  (sget [this x]
    (if (nada? this)
      nil
      (let [c (.compare comparator x value)]
        (cond
        (zero? c) x
        (> c 0) (sget (right-node this) x)
        :else (sget (left-node this) x)))))
  (left-node [this]
    (if (nada? left)
      (emty this)
      left))
  (right-node [this]
    (if (nada? right)
      (emty this)
      right))
  (new-node [this val lvl l r c] (inew-node this val lvl l r c))
  (revise [this & args] (irevise this args))
  (skew [this] (iskew this))
  (split [this] (isplit this))
  (insert [this x] (iinsert this x))
  (emty [this]
    (if (nada? this)
      this
      (.-nada this)))
  (to-str [this]
    (if (nada? this)
      ""
      (str (to-str (left-node this)) " " value " " (to-str (right-node this)))))

  Counted
  (count [this] cnt)
  )

(declare ->AASetSeq)

(deftype AASetSeq [node last cnt]
  ISeq
  (first [this]
    (if (nil? last)
      (sfirst node)
      (snext node last)))
  (next [this]
    (let [m (.more this)]
      (if (nil? (first m))
        nil
        m)))
  (more [this]
    (let [f (first this)]
      (if (nil? f)
        this
        (->AASetSeq node f (- cnt 1)))))
  (cons [this x]
    (RT/cons x this))
  (count [this]
    cnt)
  (empty [this]
    (if (nada? node)
      node
      (.-nada node)))
  (equiv [this o]
    (if (identical? this o)
      true)
    (if (not (or (instance? Sequential o) (instance? List o)))
      false)
    (loop [m (seq o)
          n this]
      (let [fm (first m)
            fn (first n)]
        (cond
          (not= fm fn) false
          (nil? fm) true
          :else (recur (.more m) (.more n))))))
  (seq [this] this)

  Counted

  Sequential
  )

(declare ->AASet)

(deftype AASet [node]

  clojure.lang.IPersistentSet
  (seq [_]
    (->AASetSeq node nil (count node)))
  (count [_]
    (count node))
  (cons [this x]
    (let [n (.insert node x)]
      (if (identical? n node)
        this
        (->AASet n))))
  (empty [this] (->AASet (.empty node)))
  (equiv [_ o] false)
  (disjoin [_ key] nil)
  (contains [_ key] (.sget node key))
  (get [_ key] (.sget node key))

  Counted
  )
