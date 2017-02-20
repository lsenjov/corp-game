(ns corp-game.helpers
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]
            )
  )

(s/def ::non-neg-num (s/and number? (complement neg?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHARED SPECS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard identifier
(s/def ::id integer?)
;; Standard name type
(s/def ::non-empty-name (s/and string? #(< 0 (count %))))
;; Cash is all liquid assets (including debt).
(s/def ::cash number?)
;; Assets represents non-liquid assets capable of making cash in the market
;; Assets are always non-negative
(s/def ::assets (s/and number? #(<= 0 %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regions have an id, name, and total revenue.
(s/def ::total-revenue (s/and ::cash #(<= 0 %)))
(s/def ::region-name ::non-empty-name)
(s/def ::region-id ::id)
(s/def ::region (s/keys :req [::region-id ::region-name ::total-revenue]))
(s/def ::regions (s/map-of ::region-id ::region))

(defn generate-region
  "Generates a new region"
  [id region-name total-revenue]
  {:pre [(s/assert ::region-id id)
         (s/assert ::region-name region-name)
         (s/assert ::total-revenue total-revenue)]
   :post [(s/assert ::region %)]}
  {::region-id id
   ::region-name region-name
   ::total-revenue total-revenue}
  )
(defn generate-and-add-region
  "As generate-region, but adds it to a ::regions map"
  [regions id region-name total-revenue]
  {:pre [(s/assert ::regions regions)
         (s/assert ::region-id id)
         (s/assert ::region-name region-name)
         (s/assert ::total-revenue total-revenue)]
   :post [(s/assert ::regions %)]}
  (assoc regions id (generate-region id region-name total-revenue))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHER HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sqrt
  "Gets the square root of a number"
  [n]
  {:pre [(s/assert ::non-neg-num n)]
   :post [(s/assert ::non-neg-num %)]
   }
  (Math/sqrt n)
  )

