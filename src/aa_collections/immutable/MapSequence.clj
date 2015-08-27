(ns aa-collections.immutable.MapSequence
  (:gen-class
    :main false
    :extends clojure.lang.ASeq
    :implements [clojure.lang.Counted]
    :constructors {[java.util.Iterator]
                   []
                   [clojure.lang.IPersistentMap Object]
                   [clojure.lang.IPersistentMap]}
    :init init
    :state state
    :methods [^:static [create [java.util.Iterator] Object]]
    :exposes-methods {count superCount})
  (:import (aa_collections.immutable MapSequence)))

(defn -create [iter]
  (if (.hasNext iter)
    (new MapSequence iter)
    nil))

(deftype seq-state [iter val rst])

(defn -init
  ([iter]
   (let [s (->seq-state iter (atom nil) (atom nil))]
     (reset! (.-val s) s)
     (reset! (.-rst s) s)
     [[] s]))
  ([meta s]
   [[meta] s])
  )

(defn -withMeta [this meta] (new MapSequence meta (.-state this)))

(defn -first [this]
  (let [s (.-state this)
        v (.-val s)]
    (if (= s @v)
      (swap! v #(if (= s %) (.next (.-iter s)))))
    @(.-val s)))

(defn -next [this]
  (let [s (.-state this)
        r (.-rst s)]
    (when (= s @r)
      (-first this)
      (swap! r #(if (= s %) (-create (.-iter s)))))
    @(.-rst s)))

(defn -count [this]
  (let [iter (.iter (.-state this))]
    (if (counted? iter)
      (.count iter)
      (.superCount this))))
