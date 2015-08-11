(ns aa-collections.immutable-set
  (:import (clojure.lang RT)))

(defn isNil [x] (or (nil? x) (zero? (.-level x))))

(defprotocol AASetInternal
  (nixnay [this])
  )

(deftype AASet [level left right value comparator nada]
  AASetInternal
  (nixnay [this]
    (if (isNil this)
      this
      nada))
  clojure.lang.IPersistentSet
  (seq [_] nil)
  (count [_] 0)
  (cons [_ o] nil)
  (empty [_] nil)
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
