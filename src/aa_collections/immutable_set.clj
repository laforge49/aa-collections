(ns aa-collections.immutable-set)

(deftype AAImmutableSet [level left right value]
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
