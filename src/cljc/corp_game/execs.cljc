(ns corp-game.execs
  "Data model for individual corporations"
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]

            [corp-game.helpers :as h]
            [corp-game.corporations :as corps]
            )
  (:gen-class)
  )

(s/def ::exec-name ::h/non-empty-name)
(s/def ::id ::h/id)
(s/def ::cash ::h/cash)
(s/def ::assets ::h/assets)

;; A list of all corporations this exec has investments in
(s/def ::invested-corps (s/and set? (s/coll-of ::id)))
(s/def ::exec (s/keys :req [::id ::exec-name ::cash ::assets ::invested-corps]))
(s/def ::execs (s/map-of ::id ::exec))

(defn create-exec
  "Creates a new executive"
  [id exec-name cash]
  {:pre [(s/assert ::id id)
         (s/assert ::exec-name exec-name)
         (s/assert ::cash cash)]
   :post [(s/assert ::exec %)]}
  {::id id
   ::exec-name exec-name
   ::cash cash
   ::assets 0
   ::invested-corps #{}
   }
  )

(defn create-and-add-executive
  "Creates an executive, adds it to the map of execs"
  [execs id exec-name cash]
  {:pre [(s/assert ::execs execs)
         (s/assert ::id id)
         ]
   ;; Other pre asserts performed in create-exec
   :post [(s/assert ::execs %)]}
  (assoc execs id (create-exec id exec-name cash)))

(defn modify-account
  "Modifies an executive's cash reserves by the amount required"
  [execs id amount]
  {:pre [(s/assert ::execs execs)
         (s/assert ::id id)
         (s/assert ::cash amount)
         ;; Ensure the exec exists.
         (get-in execs [id ::cash])
         ]
   :post [(s/assert ::execs %)]}
  (update-in execs [id ::cash] #(+ % amount))
  )

(defn add-investment
  "Modifies an executive's investment list to show they have invested in a corp"
  [execs id corp-id]
  {:pre [(s/assert ::execs execs)
         (s/assert ::id id)
         (s/assert ::id corp-id)
         ;; Ensure the exec exists.
         (get-in execs id)
         ]
   :post [(s/assert ::execs execs)]}
  (update-in execs [id ::invested-corps] conj corp-id)
  )

(defn remove-investment
  "Modifies and executive's investment list to show they are no longer invested in a corp.
  Does nothing if the exec's list doesn't have the specified corp"
  [execs id corp-id]
  {:pre [(s/assert ::execs execs)
         (s/assert ::id id)
         (s/assert ::id corp-id)
         ;; Ensure the exec exists.
         (get-in execs id)
         ]
   :post [(s/assert ::execs execs)]}
  (update-in execs [id ::invested-corps] disj corp-id)
  )

