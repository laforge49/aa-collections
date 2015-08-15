(ns aa-collections.immutable-set-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable-set :refer :all])
  (:import (clojure.lang Reversible IPersistentSet IPersistentCollection)))

(def x (aa-empty-set-node))

(println (nada? x))
(println (nada? nil))
(println (count x))
(println (to-str x))
(def x1 (insert x 1))
(println (to-str x1))
(def x13 (insert x1 3))
(println (to-str x13))
(def x123 (insert x13 2))
(println (to-str x123))
(def x1235 (insert x123 5))
(println (to-str x1235))
(def x12345 (insert x1235 4))
(println (to-str x12345))
(println (sfirst x12345))
(println (snext x12345 1))
(println (snext x12345 2))
(println (snext x12345 3))
(println (snext x12345 4))
(println (snext x12345 5))
(def s0 (->AASetSeq x12345 nil (count x12345)))
(println (first s0))
(def s1 (.more s0))
(println (first s1))
(def s2 (.more s1))
(println (first s2))
(def s3 (.more s2))
(println (first s3))
(def s4 (.more s3))
(println (first s4))
(def s5 (.more s4))
(println (first s5))
(println (next s4))
(println (.count s0))
(println (.count s1))
(println (.count s4))
(println (.count s5))
(println (sorted-set 1 2 3 4 5))
(println (= s0 (sorted-set 1 2 3 4 5)))
(println s0)
