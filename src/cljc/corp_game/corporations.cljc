(ns corp-game.corporations
  "Data model for individual corporations"
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]

            [corp-game.helpers :as h]
            )
  (:gen-class)
  )

(def ^:final settings
  h/settings
  )

(s/def ::id ::h/id)
(s/def ::corp-name ::h/non-empty-name)
(s/def ::desc (s/nilable (s/and string? #(< 0 (count %)))))
(s/def ::days (s/and number? #(< 0 %)))
;; Assets represents non-liquid assets capable of making cash in the market
(s/def ::assets ::h/assets)
;; Cash is all liquid assets (including debt). Cash is turned into assets over time, and revenue goes straight here
;; If at any time a corp's sum of cash and assets is negative, the corp is shut down
(s/def ::cash ::h/cash)

;; Productivity multiplier is a factor greater than 0
;; Starts low. Will stabilise at productivity-upkeep
;; Total shares in the market is assets * productivity multiplier, and can up to this amount from a market (If a company's assets * productivity is 100, it can only earn a maximum of $100 per day. Generally this will be much less due to competition though.
;; Note that security, chances of good events, and other factors are based off productivity. Depending on the corp, dumping lots of money into productivity may be a good thing
(s/def ::productivity (s/and number? #(< 0 %)))

;; A company requires maintenance, for both assets and productivity. These numbers start at 1 (just upkeep), and may be modified by the owners
;; If assets is greater than 1, it will grow as a proportion of its assets (with a minimum per day, in the case of businesses with no assets). If less than 1, asset totals will slowly (or sometimes quickly) degrade as upkeep is unpaid.
(s/def ::assets-upkeep (s/and number? #(<= 0 %)))
(s/def ::productivity-upkeep ::assets-upkeep)
;; Possible corporation fields, derived from settings
(s/def ::corporation-field (s/and keyword?
                                  (-> settings :corporation-fields keys set)
                                  )
  )
;; Operating theatre of this corporation
(s/def ::region-id ::h/id)
(s/def ::corp
  (s/keys :req [::id ::corp-name ::assets ::cash ::productivity ::assets-upkeep ::productivity-upkeep ::region-id]))

;; Map of corporations in a region
(s/def ::corps
  (s/map-of ::id ::corporation))

(defn create-corporation
  "Creates a basic corporation map"
  ([id corp-name cash corporation-field region-id]
   {:pre [(s/assert ::corp-name corp-name)
          (s/assert ::cash cash)
          (s/assert ::id id)
          (s/assert ::corporation-field corporation-field)
          (s/assert ::region-id region-id)
          ]
    :post [(s/assert ::corp %)]
    }
   {::id id
    ::corp-name corp-name
    ::cash cash
    ::assets 0
    ::productivity (:default-productivity settings)
    ::assets-upkeep 1
    ::productivity-upkeep 1
    ::corporation-field corporation-field
    ::region-id region-id
    }
   )
  )
(s/fdef create-corporation
        :args (s/cat :id ::id :corp-name ::corp-name :cash ::cash)
        :ret ::corporation
        )
(defn create-and-add-corporation
  "As create-corporation, but adds to a corps map"
  [corps id corp-name cash corporation-field region-id]
  {:pre [(s/assert ::corps corps)]
   :post [(s/assert ::corps %)]}
  (assoc corps id (create-corporation id corp-name cash corporation-field region-id))
  )

(defn tick-corporation-upkeep
  "Ticks a corporation, by default 1 day.
  Does not include adding revenue to the corporation's coffers"
  ([{::keys [cash assets productivity assets-upkeep productivity-upkeep] :as corporation}
    days]
   {:pre [(s/assert ::corp corporation)
          (s/assert ::days days)]
    :post [(s/assert ::corp %)]
    }
   (log/trace "tick-corporation-upkeep:" corporation days)
   (let [;; The amount that needs to be expended per upkeep category per day
         upkeep-min (* days (* (:upkeep-ratio settings) assets))
         ;; The actual cash that needs to be spent, only noticable in small corps
         upkeep-base (max upkeep-min
                          (* days
                             (:upkeep-minimum settings)))
         ;; The cash that will be spent on assets
         assets-upkeep-amount (* upkeep-base assets-upkeep)
         ;; The cash to be spent on productivity
         productivity-upkeep-amount (* upkeep-base productivity-upkeep)
         ;; Being good is *hard*. It costs exponentially more to be very good.
         ;; Alternatively, it costs much less if you just want to be "good enough", which is why we get the square root.
         weighted-productivity-change (h/sqrt (* (:productivity-change settings) days productivity-upkeep))
         ]
     (log/trace "tick-corporation-upkeep."
                "min:" upkeep-min
                "base:" upkeep-base
                "assets:" assets-upkeep-amount
                "productivity:" productivity-upkeep-amount)
     (-> corporation
         ;; Remove cash paid
         (update-in [::cash] #(- % assets-upkeep-amount productivity-upkeep-amount))
         ;; Upkeep assets
         (update-in [::assets] #(+ %
                                   ;; We get the square root here. If you're wanting to expand, it's expensive.
                                   ;; Faster you want to expand, the more costly it is
                                   (* assets-upkeep-amount (h/sqrt assets-upkeep))
                                   (- upkeep-min))
                    )
         ;; Upkeep productivity
         (update-in [::productivity] #(/ ;; Adding current productivity amount
                                         (+ %
                                            ;; To a weighted desired productivity amount
                                            weighted-productivity-change)
                                         ;; And dividing by a single share
                                         (+ 1
                                            ;; And the weighted amount once more
                                            weighted-productivity-change)
                                         )
                    )
         )
     )
   )
  ([corporation]
   (tick-corporation-upkeep corporation 1))
  )
(defn tick-corporation-upkeep-all
  "Ticks corporation upkeep for all corporations"
  [corps days]
  {:pre [(s/assert ::corps corps)
         (s/assert ::h/days days)]
   :post [(s/assert ::corps %)]}
  (->> corps
       (map (fn [[k v]] [k (tick-corporation-upkeep v days)]))
       (reduce merge {})
       )
  )

(defn calc-corp-value
  "Returns a revenue valuation of the total corporation.
 Does not weight for recently bought stocks or current cash on hand"
  [{::keys [cash productivity assets] :as corporation}]
  {:pre [(s/assert ::corp corporation)]
   :post [(s/assert ::cash %)]}
  (* assets productivity)
  )

(defn calc-corp-value-by-id
  "As calc-corp-value, but runs on a map of corps"
  [corps id]
  {:pre [(s/assert ::corps corps)
         (s/assert ::id id)]
   :post [(s/assert ::cash %)]}
  (-> corps (get id) calc-corp-value)
  )

(defn corp-modify-cash
  "Modifies the cash amount of a corporation"
  [corp amount]
  {:pre [(s/assert ::corp corp)
         (s/assert ::cash amount)]
   :post [(s/assert ::corp corp)]}
  (update-in corp [::cash] #(+ amount %)))
(defn corp-modify-cash-by-id
  "Given a corps map and an id, updates the amount of cash in that corporation"
  [corps id amount]
  {:pre [(s/assert ::corps corps)
         (s/assert ::id id)
         (s/assert ::cash amount)
         (get corps id) ;; Ensure the corp exists
         ]
   :post [(s/assert ::corps corps)]}
  (update-in corps [id] corp-modify-cash amount))
(defn corp-modify-cash-by-factor
  "As corp-modify-cash, but amount is multiplying the size of the corporation by the factor"
  [corp factor]
  {:pre [(s/assert ::corp corp)
         (s/assert number? factor)
         (pos? factor)
         ]
   :post [(s/assert ::corp %)]}
  (corp-modify-cash corp (* factor (calc-corp-value corp))))
(defn corp-modify-cash-by-factor-by-id
  "As corp-modify-cash-by-factor, but in a corps map"
  [corps id factor]
  {:pre [(s/assert ::corps corps)
         (s/assert ::id id)
         (s/assert number? factor)
         (pos? factor)
         ]
   :post [(s/assert ::corps %)]}
  (update-in corps [id] corp-modify-cash-by-factor factor))

(defn tick-revenue-by-region-and-field
  "Given a region, corporation field, map of corporations, and a time passed,
  will calculate the total revenue per dollar, and assign revenue to those corporations"
  [corps {::h/keys [region-id] :as region} corporation-field days]
  {:pre [(s/assert ::corps corps)
         (s/assert ::h/region region)
         (s/assert ::corporation-field corporation-field)
         (s/assert ::h/days days)]
   :post [(s/assert ::corps %)]}
  (let [filtered-corps ;; A map of all corporations in that region and in that field
        (->> corps
             (filter (fn [[_ v]]
                       ;; Ensure it's the same region
                       (and (= region-id (::region-id v))
                            ;; And in the correct field
                            (= corporation-field (::corporation-field v))
                            )
                       )
                     )
             (reduce merge {})
             )
        ;; Total revenue for this field
        field-revenue (h/get-field-share region corporation-field)
        ;; Total size of all corporations
        total-corps-size (->> filtered-corps vals (map calc-corp-value) (reduce +) (max 1))
        ;; How much to multiply a corporation's total value by, max of 1
        revenue-factor (* days (min 1 (/ field-revenue total-corps-size)))
        ]
    (log/trace "tick-revenue-by-region-and-field."
               "region-id" region-id
               "field" corporation-field
               "field-revenue" field-revenue
               "total-crps-size" total-corps-size
               "revenue-factor" revenue-factor)
    (as-> filtered-corps v
         (keys v)
         (map (fn [k] #(corp-modify-cash-by-factor-by-id % k revenue-factor)) v)
         (apply comp v)
         (v corps)
         )
    )
  )
(defn tick-revenue-by-region
  "As tick-revenue-by-region-and-field, but for all fields in a region"
  [corps region days]
  {:pre [(s/assert ::corps corps)
         (s/assert ::h/region region)
         (s/assert ::h/days days)]
   :post [(s/assert ::corps %)]}
  (as-> settings v
    (get v :all-fields)
    (map (fn [field] (fn [cs] (tick-revenue-by-region-and-field cs region field days)))
         v)
    (apply comp v)
    (v corps)
    )
  )
(defn tick-revenue
  "Ticks revenue for all corporations in all regions"
  [corps regions days]
  {:pre [(s/assert ::corps corps)
         (s/assert ::h/regions regions)
         (s/assert ::h/days days)]
   :post [(s/assert ::corps corps)]}
  (as-> regions v
    (vals v)
    (map (fn [region] (fn [cs] (tick-revenue-by-region cs region days))) v)
    (apply comp v)
    (v corps)
    )
  )
