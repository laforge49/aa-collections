(ns aa-collections.immutable-set-test
  (:require [clojure.test :refer :all]
            [aa-collections.immutable-set :refer :all]))

(def a0 (aa-empty-set))
(def c0 #{})
(println"a0:" a0 c0)
(def b0 (seq a0))
(def d0 (seq c0))
(println "b0:" b0 d0)
(println "first b0:" (first b0) (first d0))
(println "rest b0:" (rest b0) (rest d0))
(println "next b0:" (next b0) (next d0))
(def a1 (conj a0 1))
(def a5 (conj a0 1 2 3 4 5))
(println "a5:" a5)
(println "seq a5:" (seq a5))
(println (.getClass a5))
(let [aa (.seq a5)]
  (println (.getClass aa))
  (println (.count aa)))

(println a0 1 (.disjoin a0 1))
(println a1 0 (.disjoin a1 0))
(println a1 1 (.disjoin a1 1))
(println a5 1 (.disjoin a5 1))
(println a5 2 (.disjoin a5 2))
(println a5 3 (.disjoin a5 3))
(println a5 4 (.disjoin a5 4))
(println a5 5 (.disjoin a5 5))
