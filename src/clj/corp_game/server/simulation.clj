(ns corp-game.server.simulation
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]

            [corp-game.helpers :as h]
            [corp-game.corporations :as corps]
            [corp-game.execs :as execs]
            )
  )

;; Contains the last used id for each keyword
(def ^:private ids (ref {}
                        :validator #(s/valid? (s/map-of keyword? ::h/id) %)
                        )
  )
(defn get-next-id
  "Returns the next id to use in a sequence"
  [kw]
  {:pre [(s/assert keyword? kw)]
   :post [(s/assert ::h/id %)]}
  (dosync
    (-> ids
        (alter update-in
               [kw]
               (fn [v] (if v (inc v) 0))
               )
        ;; Get the value that was just updated in the map
        (kw)
        )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORP ONLY FUNCTION WRAPPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private corps (ref {}
                          :validator #(s/assert ::corps/corps %)
                          )
  )
(defn add-new-corporation
  "Generates and adds a new corporation. Returns the new corporation"
  [corp-name cash corporation-field region-id]
  (dosync
    (let [id (get-next-id ::corps/id)]
      (->
        (alter corps corps/create-and-add-corporation id corp-name cash corporation-field region-id)
        (get id)
        )
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGION ONLY FUNCTION WRAPPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private regions (ref {}
                            :validator #(s/assert ::h/regions %)
                            )
  )
(defn add-new-region
  "Generates and adds a new region to regions. Returns the new region"
  [region-name total-revenue]
  (dosync
    (let [id (get-next-id ::h/region-id)]
      (->
        (alter regions h/generate-and-add-region id region-name total-revenue)
        (get id)
        )
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXEC ONLY FUNCTION WRAPPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private execs (ref {}
                          :validator #(s/assert ::execs/execs %)
                          )
  )
(defn exec-add-new
  "Generates and adds a new executive. Returns the new executive"
  [exec-name cash]
  {:post [(s/assert ::execs/exec %)]}
  (dosync
    (let [id (get-next-id ::execs/id)]
      (->
        (alter execs execs/create-and-add-executive id exec-name cash)
        (get id)
        )
      )
    )
  )
(defn exec-alter-account
  "Alters an executive account by the amount specified. Returns the ::execs/execs map"
  [exec-id amount]
  (dosync
    (alter execs execs/modify-account exec-id amount)
    )
  )
(defn exec-add-investment
  "Alters executive account to show an investment in the specified corp. Returns the map"
  [exec-id corp-id]
  (dosync
    (alter execs execs/add-investment exec-id corp-id)
    )
  )
(defn exec-remove-investment
  "Alters executive account to show an investment in the specified corp. Returns the map"
  [exec-id corp-id]
  (dosync
    (alter execs execs/remove-investment exec-id corp-id)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMBINED FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
