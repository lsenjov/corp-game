(ns corp-game.corporations
  "Data model for individual corporations"
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]

            [corp-game.helpers :as h]
            )
  (:gen-class)
  )

(def ^:final settings
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
(s/def ::corporation
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
    :post [(s/assert ::corporation %)]
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

(defn ^:private tick-corporation-upkeep
  "Ticks a corporation, by default 1 day.
  Does not include adding revenue to the corporation's coffers"
  ([{::keys [cash assets productivity assets-upkeep productivity-upkeep] :as corporation}
    days]
   {:pre [(s/assert ::corporation corporation)
          (s/assert ::days days)]
    :post [(s/assert ::corporation %)]
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

(defn calc-corp-value
  "Returns a cash value of the total corporation, without weighting for recently bought stocks"
  [{::keys [cash productivity assets] :as corporation}]
  {:pre [(s/assert ::corporation corporation)]
   :post [(s/assert ::cash %)]}
  (* (+ cash assets) productivity)
  )

(defn calc-corp-value-by-id
  "As calc-corp-value, but runs on a map of corps"
  [corps id]
  {:pre [(s/assert ::corps corps)
         (s/assert ::id id)]
   :post [(s/assert ::cash %)]}
  (-> corps (get id) calc-corp-value)
  )
