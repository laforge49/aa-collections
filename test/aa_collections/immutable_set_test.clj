(ns aa-collections.immutable-set-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable-set :refer :all])
  (:import (clojure.lang Reversible IPersistentSet IPersistentCollection RT)))

(def x (->AAImmutableSet 0 nil nil nil RT/DEFAULT_COMPARATOR))

(println (instance? Reversible x))
(println (instance? IPersistentSet x))
(println (instance? IPersistentCollection x))
(println (isNil x))
(println (isNil nil))
(println (count x))
(println (seq x))
(println (.-level x))