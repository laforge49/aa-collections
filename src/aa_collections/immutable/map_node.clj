(ns aa-collections.immutable.map-node
  (:import (clojure.lang RT)))

(defprotocol IMapNode
  (emty [this])
  (cmpr [this x])
  (cntr [this])
  (right-node [this])
  (left-node [this])
  (new-node [this t2 level left right cnt])
  (revise [this & args])
  (skew [this])
  (split [this])
  (insert [this t2]))

(declare ->MapNode)

(defn emty? [x]
  (or (nil? x) (zero? (.-level x))))

(defn emty-node
  ([] (emty-node RT/DEFAULT_COMPARATOR))
  ([comparator] (->MapNode nil 0 nil nil 0 comparator nil)))

(deftype MapNode [t2 level left right cnt comparator nada]

  IMapNode

  (emty [this]
    (if (emty? this)
      this
      nada))

  (cmpr [this x]
    (.compare comparator x (.getKey (t2 this))))

  (cntr [this]
    (if (emty? this)
      0
      cnt))

  (right-node [this]
    (if (emty? right)
      (.emty this)
      right))

  (left-node [this]
    (if (emty? (.-left this))
      (emty this)
      (.-left this)))

  (new-node [this t2 level left right cnt]
    (->MapNode t2 level left right cnt (.-comparator this) (.emty this)))

  (revise [this & args]
    (let [m (apply array-map args)
          v (get m :t2 t2)
          lev (get m :level level)
          l (get m :left (.left-node this))
          r (get m :right (.right-node this))
          c (+ 1 (.cntr l) (.cntr r))]
      (if (or (not= v t2)
              (not= lev level)
              (not= l (.left-node this))
              (not= r (.right-node this)))
        (.new-node this v lev l r c)
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

  (insert [this t2]
    (if (emty? this)
      (.new-node this t2 1 nil nil 1)
      (let [c (.cmpr this t2)]
        (.split (.skew (cond
                         (< c 0)
                         (let [oldl (.left-node this)
                               l (.insert oldl t2)]
                           (.revise this :left l))
                         (> c 0)
                         (let [oldr (.right-node this)
                               r (.insert oldr t2)]
                           (.revise this :right r))))))))
  )
