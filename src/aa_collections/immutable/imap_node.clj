(ns aa-collections.immutable.imap-node
  (:import (clojure.lang IMapEntry)))

(defprotocol IMapNode
  (^IMapNode emty [this])
  (^int cmpr [this x])
  (^IMapNode right-node [this])
  (^IMapNode left-node [this])
  (^IMapNode new-node [this ^IMapEntry t2 ^int level ^IMapNode left ^IMapNode right ^int cnt])
  (^IMapNode revise [this & args])
  (^IMapNode skew [this])
  (^IMapNode split [this])
  (^IMapNode insert [this ^IMapEntry t-2])
  (^IMapEntry predecessor-t2 [this])
  (^IMapEntry successor-t2 [this])
  (^IMapEntry next-t2 [this x])
  (^IMapEntry get-t2 [this x])
  (^IMapNode decrease-level [this]))
