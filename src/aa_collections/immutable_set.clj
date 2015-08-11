(ns aa-collections.immutable-set
  (:import (clojure.lang RT)))

(defn isNada [x] (or (nil? x) (zero? (.-level x))))

(deftype AASet [level left right value comparator nada]
  clojure.lang.IPersistentSet
  (seq [_] nil)
  (count [_] 0)
  (cons [_ o] nil)
  (empty [this]
    (if (isNada this)
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
