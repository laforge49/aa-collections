(ns aa-collections.immutable-set
  (:import (clojure.lang RT Counted ISeq Sequential IPersistentCollection Seqable IteratorSeq)
           (java.util List Iterator)
           (aa_collections AASetSequence)))

(declare ->AASetNode)

(defn- nada? [x]
  (or (nil? x) (zero? (.-level x))))

(defn- aa-empty-set-node
  ([] (aa-empty-set-node RT/DEFAULT_COMPARATOR))
  ([comparator] (->AASetNode nil 0 nil nil 0 comparator nil)))

(defn- iemty [this]
       (if (nada? this)
         this
         (.-nada this)))

(defn- left-node [this]
           (if (nada? (.-left this))
             (iemty this)
             (.-left this)))

(defn- right-node [this]
            (if (nada? (.-right this))
              (iemty this)
              (.-right this)))

(defn- inew-node
  [this value level left right cnt]
  (->AASetNode value level left right cnt (.-comparator this) (iemty this)))

(defn- cntr [this]
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
    (inew-node this x 1 nil nil 1)
    (let [c (.compare (.-comparator this) x (.-value this))]
      (isplit (iskew (cond
                       (< c 0)
                       (let [ol (left-node this)
                             l (iinsert ol x)]
                         (if (identical? ol l)
                           this
                           (irevise this :left l)))
                       (> c 0)
                       (let [oldr (right-node this)
                             r (iinsert oldr x)]
                         (if (identical? oldr r)
                           this
                           (irevise this :right r))))))))
  )

(defn- ifirst [this]
  (cond
    (nada? this) nil
    (nada? (.-left this)) (.-value this)
    :else (recur (.-left this))))

(defn- ilast [this]
  (cond
    (nada? this) nil
    (nada? (.-right this)) (.-value this)
    :else (recur (.-right this))))

(defn- inext [this x]
       (if (nada? this)
         nil
         (let [c (.compare (.-comparator this) x (.-value this))]
           (cond
             (zero? c) (ifirst (right-node this))
             (> c 0) (inext (right-node this) x)
             :else (let [n (inext (left-node this) x)]
                     (if (nil? n)
                       (.-value this)
                       n))))))

(defn- iget [this x]
      (if (nada? this)
        nil
        (let [c (.compare (.-comparator this) x (.-value this))]
          (cond
            (zero? c) x
            (> c 0) (iget (right-node this) x)
            :else (iget (left-node this) x)))))

(defn- decrease-level [this]
  (let [should-be (min (.-level (left-node this))
                       (+ (.-level (right-node this) 1)))]
    (if (>= should-be (.-level this))
      this
      (let [rn (right-node this)
            rn (if (>= should-be (.-level (right-node this)))
                       rn
                       (irevise rn :level should-be))]
        (irevise this :right rn :level should-be)))))

(deftype AASetNode [value level left right cnt comparator nada])

(deftype AASetIterator [node ^:volatile-mutable lst ^:volatile-mutable cnt]
  Iterator
  (hasNext [this]
    (> cnt 0))
  (next [this]
    (if (nil? lst)
      (set! lst (ifirst node))
      (set! lst (inext node lst)))
    (set! cnt (- cnt 1))
    lst)

  Counted
  (count [this] cnt)
  )

(defn new-set-iseq [node]
  ;(iterator-seq (->AASetIterator node nil (.-cnt node)))
  ;(IteratorSeq/create (->AASetIterator node nil (.-cnt node)))
  (AASetSequence/create (->AASetIterator node nil (.-cnt node)))
  )

(declare ->AASet)

(deftype AASet [node]
  clojure.lang.IPersistentSet
  (seq [_]
    (if (nada? node)
      ()
      ;(->AASetSeq node nil (.-cnt node))
      (new-set-iseq node)
      ))
  (count [_]
    (count node))
  (cons [this x]
    (let [n (iinsert node x)]
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

(defn aa-empty-set
  ([] (aa-empty-set RT/DEFAULT_COMPARATOR))
  ([comparator]
   (->AASet (->AASetNode nil 0 nil nil 0 comparator nil))))
