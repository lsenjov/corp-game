(ns corp-game.helpers
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]
            )
  )

(def ^:final settings
  (->
    {
     ;; Starting corp productivity
     :default-productivity 0.2
     ;; Default dividend payout. Pays this amount of total cash to shareholders daily.
     :default-dividends (/ 1 1e4)
     ;; How fast productivity changes. If set to 1, will be equal weighting to current productivity, per day
     ;; At 1/1000, takes 60 days to rise from 0.2 to ~0.85 when productivity-upkeep is set to 1
     :productivity-change (/ 1 1e3)
     ;; How much cash as a percentage of assets goes to EACH upkeep category, paid daily
     ;; If the ratio is 0.01, and the company has $100 assets, each day the company will pay $1 to each upkeep category.
     :upkeep-ratio 0.01
     ;; Companies have a minimum upkeep amount for each category. No matter how small a company gets, it will still bleed a little bit of cash per day.
     ;; This is also the minimum speed a corporation expands each day
     :upkeep-minimum 100
     ;; Possible fields for corporations and their total revenue shares.
     ;; Revenue is proportional to each other. If a field has 2 shares out of the 20, in each region its total revenue will be 10% of the region
     :corporation-fields {:media {::title "Media"
                                  ::desc "Media and Video entertainment"
                                  ::revenue-share 1}
                          :security {::title "Security"
                                     ::revenue-share 1}
                          }
     }
    ((fn [m] (assoc m :total-revenue-shares (->> m :corporation-fields vals (map ::revenue-share) (reduce +)))))
    ((fn [m] (assoc m :all-fields (->> m :corporation-fields keys))))
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
;; Some length of time to pass
(s/def ::days (s/and number? pos?))

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
(defn get-field-share
  "Given a region and a corporation field, returns the total revenue to be had"
  [region corporation-field]
  {:pre [(s/assert ::region region)]
   :post [(s/assert ::cash %)]}
  ;; Get the proportion of this field to the total shares
  (-> (/ (get-in settings [:corporation-fields corporation-field ::revenue-share])
         (:total-revenue-shares settings))
      ;; Multiply this fraction by the region revenue
      (* (::total-revenue region))
      )
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

