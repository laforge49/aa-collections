(ns aa-collections.immutable-set
  (:import (clojure.lang RT)))

(defn isNil [x] (or (nil? x) (zero? (.-level x))))

(defprotocol AAImmutableSetInternal
  )

(deftype AAImmutableSet [level left right value comparator]
  AAImmutableSetInternal
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
