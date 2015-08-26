(ns aa-collections.immutable.map-node
  (:import (clojure.lang RT IMapEntry Counted)
           (java.util Comparator)
           (aa_collections.immutable.imap_node IMapNode)))

(declare ->MapNode)

(^IMapNode defn emty? [x]
  (or (nil? x) (zero? (.-level x))))

(^IMapNode defn emty-node
  ([] (emty-node RT/DEFAULT_COMPARATOR))
  ([^Comparator comparator] (->MapNode nil 0 nil nil 0 comparator nil)))

(^IMapEntry defn first-t2 [^IMapNode this]
  (cond
    (emty? this) nil
    (emty? (.-left this)) (.-t2 this)
    :else (recur (.-left this))))

(^IMapEntry defn last-t2 [^IMapNode this]
  (cond
    (emty? this) nil
    (emty? (.-right this)) (.-t2 this)
    :else (recur (.-right this))))

(deftype MapNode [^IMapEntry t2 ^int level ^IMapNode left ^IMapNode right ^int cnt ^Comparator comparator ^IMapNode nada]

  Counted

  (count [this]
    (if (emty? this)
      0
      cnt))

  IMapNode

  (emty [this]
    (if (emty? this)
      this
      nada))

  (cmpr [this x]
    (.compare comparator x (.getKey t2)))

  (right-node [this]
    (if (emty? right)
      (.emty this)
      right))

  (left-node [this]
    (if (emty? (.-left this))
      (.emty this)
      (.-left this)))

  (new-node [this t2 level left right cnt]
    (->MapNode t2 level left right cnt (.-comparator this) (.emty this)))

  (revise [this & args]
    (let [m (apply array-map args)
          t-2 (get m :t2 t2)
          lev (get m :level level)
          l (get m :left (.left-node this))
          r (get m :right (.right-node this))
          c (+ 1 (.count l) (.count r))]
      (if (or (not= (.getKey t-2) (.getkey t2))
              (not= (.getValue t-2) (.getValue t2))
              (not= lev level)
              (not= l (.left-node this))
              (not= r (.right-node this)))
        (.new-node this t-2 lev l r c)
        this)))

  (skew
    [this]
    (cond
      (emty? this)
      this
      (emty? left)
      this
      (= (.-level left) level)
      (let [l left]
        (.revise l :right (.revise this :left (.-right l))))
      :else
      this))

  (split [this]
    (cond
      (emty? this)
      this
      (or (emty? right) (emty? (.-right right)))
      this
      (= level (.-level (.-right right)))
      (.revise right
               :level (+ 1 (.-level right))
               :left (.revise this :right (.-left right)))
      :else
      this))

  (insert [this t-2]
    (if (emty? this)
      (.new-node this t2 1 nil nil 1)
      (let [c (.cmpr this (.getKey t-2))]
        (.split (.skew (cond
                         (< c 0)
                         (let [oldl (.left-node this)
                               l (.insert oldl t-2)]
                           (.revise this :left l))
                         (> c 0)
                         (let [oldr (.right-node this)
                               r (.insert oldr t-2)]
                           (.revise this :right r))
                         :else
                         (.revise this :t2 t-2)))))))

  (predecessor-t2 [this]
    (last-t2 (.left-node this)))

  (successor-t2 [this]
    (first-t2 (.right-node this)))

  (next-t2 [this x]
    (if (emty? this)
      nil
      (let [c (.cmpr this x)]
        (cond
          (zero? c) (.successor-t2 this)
          (> c 0) (.next-t2 (.right-node this) x)
          :else (if (emty? (.left-node this))
                  t2
                  (.next-t2 (.left-node this) x))))))

  (get-t2 [this x]
    (if (emty? this)
      nil
      (let [c (.cmpr this x)]
        (cond
          (zero? c) t2
          (> c 0) (.get-t2 (.right-node this) x)
          :else (.get-t2 (.left-node this) x)))))

  (decrease-level [this]
    (let [should-be (+ 1 (min (.-level (.left-node this))
                              (.-level (.right-node this))))]
      (if (>= should-be level)
        this
        (let [rn (.right-node this)
              rn (if (>= should-be (.-level (.right-node this)))
                   rn
                   (.revise rn :level should-be))]
          (.revise this :right rn :level should-be)))))

  (delete [this x]
    (if (emty? this)
      this
      (let [c (.cmpr this x)]
        (if (and (= c 0) (= 1 level))
          (.emty this)
          (let [t (cond
                    (> c 0)
                    (.revise this :right (.delete (.right-node this) x))
                    (< c 0)
                    (.revise this :left (.delete (.left-node this) x))
                    :else
                    (if (emty? (.left-node this))
                      (let [s (.successor-t2 this)]
                        (.revise this :t2 s :right (.delete (.right-node this) s)))
                      (let [p (.predecessor-t2 this)]
                        (.revise this :t2 p :left (.delete (.left-node this) p)))))
                t (.decrease-level t)
                t (.skew t)
                t (.revise t :right (.skew (.right-node t)))
                r (.right-node t)
                t (if (emty? r)
                    t
                    (.revise t :right (.revise r :right (.skew (.right-node r)))))
                t (.split t)
                t (.revise t :right (.split (.right-node t)))]
            t)))))
  )
