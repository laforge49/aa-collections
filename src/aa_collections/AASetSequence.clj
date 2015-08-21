(ns aa-collections.AASetSequence
  (:gen-class
    :main false
    :extends clojure.lang.ASeq
    :constructors {[java.util.Iterator]
                   []
                   [clojure.lang.IPersistentMap Object]
                   [clojure.lang.IPersistentMap]}
    :init init
    :state state
    :methods [^:static [create [java.util.Iterator] Object]])
  (:import (aa_collections AASetSequence)))

(defn -create [iter]
  (if (.hasNext iter)
    (new AASetSequence iter)
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

(defn -withMeta [this meta] (new AASetSequence meta (.-state this)))

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
