(ns filipesilva.inst
  (:refer-clojure :exclude [+ - next])
  (:require [clojure.string :as str])
  #?(:clj (:import [java.util Date]
                    [java.time ZoneOffset ZonedDateTime YearMonth])))

;; ---------------------------------------------------------------------------
;; Platform layer (private)
;; ---------------------------------------------------------------------------

(defn- from-ms [ms]
  #?(:clj  (Date. (long ms))
     :cljs (js/Date. ms)))

(defn- from-string [s]
  #?(:clj  (clojure.instant/read-instant-date s)
     :cljs (let [;; #inst treats missing tz as UTC; JS treats time-only strings as local.
                 ;; Append Z to strings with time but no offset to match #inst behavior.
                 s (if (and (str/includes? s "T")
                            (not (re-find #"[Zz]$|[+\-]\d{2}:\d{2}$" s)))
                     (str s "Z")
                     s)]
             (js/Date. s))))

(defn- now-ms []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

#?(:clj
   (defn- date->zdt ^ZonedDateTime [^Date d ^ZoneOffset offset]
     (-> d .toInstant (.atZone offset))))

#?(:clj
   (defn- zdt->date ^Date [^ZonedDateTime zdt]
     (Date/from (.toInstant zdt))))

#?(:cljs
   (defn- offset-mins->ms [offset-mins]
     (* offset-mins 60 1000)))

(defn- add-platform [inst n unit]
  #?(:clj
     (let [zdt (date->zdt inst ZoneOffset/UTC)]
       (zdt->date
        (case unit
          :millis  (.plusNanos zdt (* (long n) 1000000))
          :seconds (.plusSeconds zdt (long n))
          :minutes (.plusMinutes zdt (long n))
          :hours   (.plusHours zdt (long n))
          :days    (.plusDays zdt (long n))
          :weeks   (.plusWeeks zdt (long n))
          :months  (.plusMonths zdt (long n))
          :years   (.plusYears zdt (long n)))))
     :cljs
     (let [ms (inst-ms inst)
           d  (js/Date. ms)
           ;; Clamp day after month/year change to match java.time behavior.
           ;; JS overflows (Jan 31 + 1mo = Mar 3), java.time clamps (= Feb 28).
           clamp-day (fn [d orig-day]
                       (when-not (= (.getUTCDate d) orig-day)
                         (.setUTCDate d 0)) ; day 0 = last day of prev month
                       d)]
       (case unit
         :millis  (js/Date. (clojure.core/+ ms n))
         :seconds (js/Date. (clojure.core/+ ms (* n 1000)))
         :minutes (js/Date. (clojure.core/+ ms (* n 60000)))
         :hours   (js/Date. (clojure.core/+ ms (* n 3600000)))
         :days    (do (.setUTCDate d (clojure.core/+ (.getUTCDate d) n)) d)
         :weeks   (do (.setUTCDate d (clojure.core/+ (.getUTCDate d) (* n 7))) d)
         :months  (let [day (.getUTCDate d)]
                    (.setUTCMonth d (clojure.core/+ (.getUTCMonth d) n))
                    (clamp-day d day))
         :years   (let [day (.getUTCDate d)]
                    (.setUTCFullYear d (clojure.core/+ (.getUTCFullYear d) n))
                    (clamp-day d day))))))

;; Cron helpers: get/set date components in a given UTC offset

(defn- components
  "Returns {:year :month :day :hour :minute :dow} in offset-local time.
   month is 1-based, dow is 0=Sunday."
  [inst offset-mins]
  #?(:clj
     (let [offset (ZoneOffset/ofTotalSeconds (* offset-mins 60))
           zdt    (date->zdt inst offset)]
       {:year   (.getYear zdt)
        :month  (.getMonthValue zdt)
        :day    (.getDayOfMonth zdt)
        :hour   (.getHour zdt)
        :minute (.getMinute zdt)
        :dow    (mod (.getValue (.getDayOfWeek zdt)) 7)})
     :cljs
     (let [ms (clojure.core/+ (inst-ms inst) (offset-mins->ms offset-mins))
           d  (js/Date. ms)]
       {:year   (.getUTCFullYear d)
        :month  (clojure.core/+ (.getUTCMonth d) 1)
        :day    (.getUTCDate d)
        :hour   (.getUTCHours d)
        :minute (.getUTCMinutes d)
        :dow    (.getUTCDay d)})))

(defn- from-components
  "Builds an inst from components expressed in offset-local time."
  [{:keys [year month day hour minute]} offset-mins]
  #?(:clj
     (let [offset (ZoneOffset/ofTotalSeconds (* offset-mins 60))
           zdt    (ZonedDateTime/of (int year) (int month) (int day)
                                    (int hour) (int minute) 0 0 offset)]
       (zdt->date zdt))
     :cljs
     (let [ms (js/Date.UTC year (clojure.core/- month 1) day hour minute 0 0)]
       (js/Date. (clojure.core/- ms (offset-mins->ms offset-mins))))))

(defn- dim
  "Days in month for given year and 1-based month."
  [year month]
  #?(:clj  (.lengthOfMonth (YearMonth/of (int year) (int month)))
     :cljs (.getUTCDate (js/Date. (js/Date.UTC year month 0)))))

;; ---------------------------------------------------------------------------
;; Cron parsing (pure)
;; ---------------------------------------------------------------------------

(defn- parse-field
  "Parses a single cron field string into a sorted set of ints."
  [s min-val max-val]
  (let [expand (fn [lo hi step] (range lo (inc hi) step))
        parse-atom
        (fn [part]
          (cond
            (= part "*")
            (expand min-val max-val 1)

            (re-matches #"\*/(\d+)" part)
            (let [[_ step] (re-matches #"\*/(\d+)" part)]
              (expand min-val max-val (parse-long step)))

            (re-matches #"(\d+)-(\d+)/(\d+)" part)
            (let [[_ lo hi step] (re-matches #"(\d+)-(\d+)/(\d+)" part)]
              (expand (parse-long lo) (parse-long hi) (parse-long step)))

            (re-matches #"(\d+)-(\d+)" part)
            (let [[_ lo hi] (re-matches #"(\d+)-(\d+)" part)]
              (expand (parse-long lo) (parse-long hi) 1))

            :else
            [(parse-long part)]))]
    (into (sorted-set) (mapcat parse-atom (str/split s #",")))))

(defn- parse-cron
  "Parses a 5-field cron string."
  [cron-str]
  (let [fields (str/split (str/trim cron-str) #"\s+")
        _      (assert (= 5 (count fields)) (str "cron must have 5 fields: " cron-str))
        [mi h dom mon dow] fields]
    {:minutes   (parse-field mi  0 59)
     :hours     (parse-field h   0 23)
     :days      (parse-field dom 1 31)
     :months    (parse-field mon 1 12)
     :weekdays  (parse-field dow 0 6)
     :dom-star? (= dom "*")
     :dow-star? (= dow "*")}))

;; ---------------------------------------------------------------------------
;; Cron search
;; ---------------------------------------------------------------------------

(defn- next-in-set
  "Smallest value in sorted-set >= val, or nil."
  [ss val]
  (first (subseq ss >= val)))

(defn- prev-in-set
  "Largest value in sorted-set <= val, or nil."
  [ss val]
  (first (rsubseq ss <= val)))

(defn- valid-day?
  "Checks if day d is valid for the cron spec in the given month."
  [cron c d]
  (let [{:keys [days weekdays dom-star? dow-star?]} cron
        max-d (dim (:year c) (:month c))]
    (when (<= d max-d)
      (let [test-c    (assoc c :day d :hour 0 :minute 0)
            test-inst (from-components test-c 0)
            dw        (:dow (components test-inst 0))]
        (cond
          (and dom-star? dow-star?) true
          dom-star?                 (contains? weekdays dw)
          dow-star?                 (contains? days d)
          :else                     (or (contains? days d)
                                        (contains? weekdays dw)))))))

(defn- find-next
  "Finds next matching time from components. Returns components or nil."
  [cron comps year-limit]
  (let [{:keys [minutes hours]} cron
        max-year (clojure.core/+ (:year comps) year-limit)]
    (loop [{:keys [year month day hour minute] :as c} comps]
      (when (<= year max-year)
        (if-let [m (next-in-set (:months cron) month)]
          (let [c (if (> m month) (assoc c :month m :day 1 :hour 0 :minute 0) c)
                {:keys [month day]} c
                max-d (dim (:year c) (:month c))
                next-day (loop [d day]
                           (when (<= d max-d)
                             (if (valid-day? cron c d) d (recur (inc d)))))]
            (if next-day
              (let [c (if (> next-day (:day c))
                        (assoc c :day next-day :hour 0 :minute 0)
                        (assoc c :day next-day))]
                (if-let [h (next-in-set hours (:hour c))]
                  (let [c (if (> h (:hour c)) (assoc c :hour h :minute 0) c)]
                    (if-let [mi (next-in-set minutes (:minute c))]
                      (assoc c :minute mi)
                      ;; minute wrapped — try next hour
                      (if-let [nh (next-in-set hours (inc (:hour c)))]
                        (recur (assoc c :hour nh :minute 0))
                        ;; hour wrapped — next day
                        (recur (assoc c :day (inc (:day c)) :hour 0 :minute 0)))))
                  ;; hour wrapped — next day
                  (recur (assoc c :day (inc (:day c)) :hour 0 :minute 0))))
              ;; day wrapped — next month
              (if-let [nm (next-in-set (:months cron) (inc month))]
                (recur (assoc c :month nm :day 1 :hour 0 :minute 0))
                (recur (assoc c :year (inc year) :month 1 :day 1 :hour 0 :minute 0)))))
          ;; month wrapped — next year
          (recur (assoc c :year (inc year) :month 1 :day 1 :hour 0 :minute 0)))))))

(defn- find-prev
  "Finds previous matching time from components. Returns components or nil."
  [cron comps year-limit]
  (let [{:keys [minutes hours]} cron
        min-year (clojure.core/- (:year comps) year-limit)]
    (loop [{:keys [year month day hour minute] :as c} comps]
      (when (>= year min-year)
        (if-let [m (prev-in-set (:months cron) month)]
          (let [c (if (< m month)
                    (let [max-d (dim year m)]
                      (assoc c :month m :day max-d :hour 23 :minute 59))
                    c)
                {:keys [month day]} c
                prev-day (loop [d day]
                           (when (>= d 1)
                             (if (valid-day? cron c d) d (recur (dec d)))))]
            (if prev-day
              (let [c (if (< prev-day (:day c))
                        (assoc c :day prev-day :hour 23 :minute 59)
                        (assoc c :day prev-day))]
                (if-let [h (prev-in-set hours (:hour c))]
                  (let [c (if (< h (:hour c)) (assoc c :hour h :minute 59) c)]
                    (if-let [mi (prev-in-set minutes (:minute c))]
                      (assoc c :minute mi)
                      ;; minute wrapped — try prev hour
                      (if-let [ph (prev-in-set hours (dec (:hour c)))]
                        (recur (assoc c :hour ph :minute 59))
                        ;; hour wrapped — prev day
                        (recur (assoc c :day (dec (:day c)) :hour 23 :minute 59)))))
                  ;; hour wrapped — prev day
                  (recur (assoc c :day (dec (:day c)) :hour 23 :minute 59))))
              ;; day wrapped — prev month
              (if-let [pm (prev-in-set (:months cron) (dec month))]
                (let [max-d (dim year pm)]
                  (recur (assoc c :month pm :day max-d :hour 23 :minute 59)))
                (recur (assoc c :year (dec year) :month 12 :day 31 :hour 23 :minute 59)))))
          ;; month wrapped — prev year
          (recur (assoc c :year (dec year) :month 12 :day 31 :hour 23 :minute 59)))))))

;; ---------------------------------------------------------------------------
;; Timezone offset parsing
;; ---------------------------------------------------------------------------

(defn- parse-offset
  "Parses '+05:30' or '-08:00' into minutes from UTC."
  [tz]
  (let [[_ sign h m] (re-matches #"([+-])(\d{2}):(\d{2})" tz)
        total (clojure.core/+ (* (parse-long h) 60) (parse-long m))]
    (if (= sign "-") (clojure.core/- total) total)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn inst
  "Creates an #inst. No args = now. String = parse. Number = ms. inst? = copy."
  ([] (from-ms (now-ms)))
  ([x]
   (cond
     (string? x) (from-string x)
     (number? x) (from-ms x)
     :else       (from-ms (inst-ms x)))))

(defn +
  "Adds n units to inst. Units: :millis :seconds :minutes :hours :days :weeks :months :years"
  [inst n unit]
  (add-platform inst n unit))

(defn -
  "Subtracts n units from inst."
  [inst n unit]
  (+ inst (clojure.core/- n) unit))

(defn next
  "Returns the next #inst matching the cron expression after the given inst.
   Optional tz is an offset string like '-08:00' (default UTC)."
  ([inst cron-str]
   (next inst cron-str "+00:00"))
  ([inst cron-str tz]
   (let [cron   (parse-cron cron-str)
         offset (parse-offset tz)
         comps  (components inst offset)
         ;; Start from the next minute
         start  (let [c (update comps :minute inc)]
                  (if (>= (:minute c) 60)
                    (let [c (assoc c :minute 0 :hour (inc (:hour c)))]
                      (if (>= (:hour c) 24)
                        (assoc c :hour 0 :day (inc (:day c)))
                        c))
                    c))
         result (find-next cron start 4)]
     (when result
       (from-components result offset)))))

(defn previous
  "Returns the previous #inst matching the cron expression before the given inst.
   Optional tz is an offset string like '-08:00' (default UTC)."
  ([inst cron-str]
   (previous inst cron-str "+00:00"))
  ([inst cron-str tz]
   (let [cron   (parse-cron cron-str)
         offset (parse-offset tz)
         comps  (components inst offset)
         ;; Start from the previous minute
         start  (let [c (update comps :minute dec)]
                  (if (neg? (:minute c))
                    (let [c (assoc c :minute 59 :hour (dec (:hour c)))]
                      (if (neg? (:hour c))
                        (assoc c :hour 23 :day (dec (:day c)))
                        c))
                    c))
         result (find-prev cron start 4)]
     (when result
       (from-components result offset)))))
