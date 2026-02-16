(ns filipesilva.inst
  (:refer-clojure :exclude [+ - next str])
  (:require [clojure.string :as str]
            #?(:clj  [clojure.instant]
               :cljs [cljs.tagged-literals]))
  #?(:clj (:import [java.util Date]
                    [java.time ZoneId ZoneOffset ZonedDateTime YearMonth])))

;; ---------------------------------------------------------------------------
;; Platform layer
;; ---------------------------------------------------------------------------

(defn- from-ms [ms]
  #?(:clj  (Date. (long ms))
     :cljs (js/Date. ms)))

(defn- from-string [s]
  #?(:clj  (clojure.instant/read-instant-date s)
     :cljs (cljs.tagged-literals/read-inst s)))

(defn- now-ms []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

#?(:clj
   (defn- date->zdt ^ZonedDateTime [^Date d ^ZoneId zone]
     (-> d .toInstant (.atZone zone))))

#?(:clj
   (defn- zdt->date ^Date [^ZonedDateTime zdt]
     (Date/from (.toInstant zdt))))

#?(:cljs
   (defn- offset-mins->ms [offset-mins]
     (* offset-mins 60 1000)))

(def ^:private utc
  #?(:clj ZoneOffset/UTC :cljs 0))

(defn- parse-zone
  "Parses a tz-or-offset string. On JVM returns a ZoneId; on JS returns
   offset-mins (number) for offsets or a timezone string for timezones."
  [tz-or-offset]
  #?(:clj  (ZoneId/of tz-or-offset)
     :cljs (if-let [[_ sign h m] (re-matches #"([+-])(\d{2}):(\d{2})" tz-or-offset)]
             (let [total (clojure.core/+ (* (parse-long h) 60) (parse-long m))]
               (if (= sign "-") (clojure.core/- total) total))
             tz-or-offset)))

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
  "Returns {:year :month :day :hour :minute :dow} in zone-local time.
   month is 1-based, dow is 0=Sunday.
   zone: JVM = ZoneId, JS = offset-mins (number) or timezone string."
  [inst zone]
  #?(:clj
     (let [zdt (date->zdt inst zone)]
       {:year   (.getYear zdt)
        :month  (.getMonthValue zdt)
        :day    (.getDayOfMonth zdt)
        :hour   (.getHour zdt)
        :minute (.getMinute zdt)
        :dow    (mod (.getValue (.getDayOfWeek zdt)) 7)})
     :cljs
     (if (number? zone)
       (let [ms (clojure.core/+ (inst-ms inst) (offset-mins->ms zone))
             d  (js/Date. ms)]
         {:year   (.getUTCFullYear d)
          :month  (clojure.core/+ (.getUTCMonth d) 1)
          :day    (.getUTCDate d)
          :hour   (.getUTCHours d)
          :minute (.getUTCMinutes d)
          :dow    (.getUTCDay d)})
       (let [fmt (js/Intl.DateTimeFormat. "en-US"
                   #js {:timeZone zone :hourCycle "h23"
                        :weekday "short" :year "numeric" :month "numeric"
                        :day "numeric" :hour "numeric" :minute "numeric"})
             parts (.formatToParts fmt inst)
             vals  (reduce (fn [m p] (assoc m (.-type p) (.-value p))) {} parts)
             dow-str (get vals "weekday")
             dow-map {"Sun" 0 "Mon" 1 "Tue" 2 "Wed" 3 "Thu" 4 "Fri" 5 "Sat" 6}]
         {:year   (parse-long (get vals "year"))
          :month  (parse-long (get vals "month"))
          :day    (parse-long (get vals "day"))
          :hour   (parse-long (get vals "hour"))
          :minute (parse-long (get vals "minute"))
          :dow    (get dow-map dow-str)}))))

#?(:cljs
   (defn- tz-offset-mins
     "Returns the UTC offset in minutes for a timezone at a given instant."
     [tz-str ms]
     (let [c (components (js/Date. ms) tz-str)
           local-ms (js/Date.UTC (:year c) (clojure.core/- (:month c) 1)
                                 (:day c) (:hour c) (:minute c) 0 0)]
       (js/Math.round (/ (clojure.core/- local-ms ms) 60000)))))

(defn- from-components
  "Builds an inst from components expressed in zone-local time.
   zone: JVM = ZoneId, JS = offset-mins (number) or timezone string."
  [{:keys [year month day hour minute]} zone]
  #?(:clj
     (let [zdt (ZonedDateTime/of (int year) (int month) (int day)
                                  (int hour) (int minute) 0 0 zone)]
       (zdt->date zdt))
     :cljs
     (if (number? zone)
       (let [ms (js/Date.UTC year (clojure.core/- month 1) day hour minute 0 0)]
         (js/Date. (clojure.core/- ms (offset-mins->ms zone))))
       ;; Timezone string: approximate-then-verify
       (let [approx-ms (js/Date.UTC year (clojure.core/- month 1) day hour minute 0 0)
             offset1   (tz-offset-mins zone approx-ms)
             adjusted  (clojure.core/- approx-ms (* offset1 60000))
             offset2   (tz-offset-mins zone adjusted)
             result-ms (clojure.core/- approx-ms (* offset2 60000))
             ;; If the offset at the result differs from offset2, the requested
             ;; local time falls in a DST gap. Use offset1 to match java.time's
             ;; behavior of shifting forward through the gap.
             offset3   (tz-offset-mins zone result-ms)]
         (js/Date. (if (= offset2 offset3) result-ms adjusted))))))

(defn- dim
  "Days in month for given year and 1-based month."
  [year month]
  #?(:clj  (.lengthOfMonth (YearMonth/of (int year) (int month)))
     :cljs (.getUTCDate (js/Date. (js/Date.UTC year month 0)))))

;; ---------------------------------------------------------------------------
;; Cron parsing
;; ---------------------------------------------------------------------------

(defn- parse-field
  "Parses a single cron field string into a sorted set of ints."
  [s min-val max-val]
  (let [expand (fn [lo hi step] (range lo (inc hi) step))
        parse-atom
        (fn [part]
          (if (= part "*")
            (expand min-val max-val 1)
            (let [[_ a b c] (re-matches #"(\*|\d+)(?:-(\d+))?(?:/(\d+))?" part)]
              (cond
                c (expand (if (= a "*") min-val (parse-long a))
                          (if b (parse-long b) max-val)
                          (parse-long c))
                b (expand (parse-long a) (parse-long b) 1)
                :else [(parse-long a)]))))]
    (into (sorted-set) (mapcat parse-atom (str/split s #",")))))

(defn- parse-cron
  "Parses a 5-field cron string."
  [cron-str]
  (let [fields (str/split (str/trim cron-str) #"\s+")
        _      (when-not (= 5 (count fields))
                 (throw (ex-info (clojure.core/str "cron must have 5 fields: " cron-str)
                                 {:cron cron-str :fields fields})))
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
;;
;; Descends through fields (month → day → hour → minute), seeking the next/prev
;; valid value at each level. When a field has no valid value, it bumps the
;; parent and restarts from that level.
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
            test-inst (from-components test-c utc)
            dw        (:dow (components test-inst utc))]
        (cond
          (and dom-star? dow-star?) true
          dom-star?                 (contains? weekdays dw)
          dow-star?                 (contains? days d)
          :else                     (or (contains? days d)
                                        (contains? weekdays dw)))))))

(defn- find-next
  "Finds next matching time from components. Returns components or nil."
  [cron comps year-limit]
  (let [{:keys [minutes hours months]} cron
        max-year (clojure.core/+ (:year comps) year-limit)]
    (loop [{:keys [year month day hour] :as c} comps
           field :month]
      (when (<= year max-year)
        (case field
          :month
          (if-let [m (next-in-set months month)]
            (recur (if (> m month) (assoc c :month m :day 1 :hour 0 :minute 0) c)
                   :day)
            (recur (assoc c :year (inc year) :month 1 :day 1 :hour 0 :minute 0)
                   :month))

          :day
          (let [max-d (dim year month)
                d (loop [d day]
                    (when (<= d max-d)
                      (if (valid-day? cron c d) d (recur (inc d)))))]
            (if d
              (recur (if (> d day) (assoc c :day d :hour 0 :minute 0) (assoc c :day d))
                     :hour)
              (recur (assoc c :month (inc month) :day 1 :hour 0 :minute 0)
                     :month)))

          :hour
          (if-let [h (next-in-set hours hour)]
            (recur (if (> h hour) (assoc c :hour h :minute 0) c)
                   :minute)
            (recur (assoc c :day (inc day) :hour 0 :minute 0)
                   :day))

          :minute
          (if-let [mi (next-in-set minutes (:minute c))]
            (assoc c :minute mi)
            (recur (assoc c :hour (inc hour) :minute 0)
                   :hour)))))))

(defn- find-prev
  "Finds previous matching time from components. Returns components or nil."
  [cron comps year-limit]
  (let [{:keys [minutes hours months]} cron
        min-year (clojure.core/- (:year comps) year-limit)]
    (loop [{:keys [year month day hour] :as c} comps
           field :month]
      (when (>= year min-year)
        (case field
          :month
          (if-let [m (prev-in-set months month)]
            (recur (if (< m month)
                     (let [max-d (dim year m)]
                       (assoc c :month m :day max-d :hour 23 :minute 59))
                     c)
                   :day)
            (recur (assoc c :year (dec year) :month 12 :day 31 :hour 23 :minute 59)
                   :month))

          :day
          (let [d (loop [d day]
                    (when (>= d 1)
                      (if (valid-day? cron c d) d (recur (dec d)))))]
            (if d
              (recur (if (< d day) (assoc c :day d :hour 23 :minute 59) (assoc c :day d))
                     :hour)
              (recur (assoc c :month (dec month) :day 31 :hour 23 :minute 59)
                     :month)))

          :hour
          (if-let [h (prev-in-set hours hour)]
            (recur (if (< h hour) (assoc c :hour h :minute 59) c)
                   :minute)
            (recur (assoc c :day (dec day) :hour 23 :minute 59)
                   :day))

          :minute
          (if-let [mi (prev-in-set minutes (:minute c))]
            (assoc c :minute mi)
            (recur (assoc c :hour (dec hour) :minute 59)
                   :hour)))))))

(defn- find-cron [inst cron-str tz-or-offset direction]
  (let [cron   (parse-cron cron-str)
        zone   (parse-zone tz-or-offset)
        [step find-fn] (case direction :next [1 find-next] :previous [-1 find-prev])
        start  (components (add-platform inst step :minutes) zone)
        result (find-fn cron start 4)]
    (when result
      (from-components result zone))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn inst
  "Creates an #inst. No args = now. String = parse. Number = ms. inst? = copy.
  With tz-or-offset, interprets the local time in that zone/offset.
  tz-or-offset: timezone like \"America/Chicago\" or offset like \"-06:00\"."
  ([] (from-ms (now-ms)))
  ([x]
   (cond
     (string? x) (from-string x)
     (number? x) (from-ms x)
     :else       (from-ms (inst-ms x))))
  ([x tz-or-offset]
   (let [i    (inst x)
         zone (parse-zone tz-or-offset)
         c    (components i utc)]
     (from-components c zone))))

(defn- format-offset
  "Formats offset minutes as '+HH:MM' or '-HH:MM'."
  [offset-mins]
  (let [sign  (if (neg? offset-mins) "-" "+")
        abs   #?(:clj (Math/abs ^long offset-mins) :cljs (js/Math.abs offset-mins))
        h     (quot abs 60)
        m     (rem abs 60)]
    (clojure.core/str sign
      #?(:clj  (clojure.core/format "%02d:%02d" h m)
         :cljs (clojure.core/str
                 (when (< h 10) "0") h ":"
                 (when (< m 10) "0") m)))))

(defn str
  "Returns the inst as an ISO-8601 string that round-trips through inst.
  With tz-or-offset, shows the time in that zone/offset."
  ([inst]
   #?(:clj  (let [s (.toString (.toInstant ^Date inst))]
              ;; Instant.toString omits .000 when millis are zero; normalize to
              ;; always include them for consistency with JS toISOString.
              (if (= (count s) 20) ;; "yyyy-MM-ddTHH:mm:ssZ"
                (clojure.core/str (subs s 0 19) ".000Z")
                s))
      :cljs (.toISOString inst)))
  ([inst tz-or-offset]
   (let [zone (parse-zone tz-or-offset)]
     #?(:clj
        (let [zdt  (date->zdt inst zone)
              secs (.getTotalSeconds (.getOffset zdt))]
          (clojure.core/format "%04d-%02d-%02dT%02d:%02d:%02d.%03d%s"
            (.getYear zdt) (.getMonthValue zdt) (.getDayOfMonth zdt)
            (.getHour zdt) (.getMinute zdt) (.getSecond zdt)
            (quot (.getNano zdt) 1000000)
            (if (zero? secs) "Z" (format-offset (quot secs 60)))))
        :cljs
        (let [ms          (inst-ms inst)
              offset-mins (if (number? zone) zone (tz-offset-mins zone ms))
              shifted     (js/Date. (clojure.core/+ ms (* offset-mins 60000)))
              iso         (.toISOString shifted)]
          ;; Replace trailing Z with offset string
          (clojure.core/str (subs iso 0 23) (if (zero? offset-mins) "Z"
                                               (format-offset offset-mins))))))))

(defn tzs
  "Returns a sorted seq of supported timezone strings, e.g. \"America/Chicago\"."
  []
  (sort #?(:clj  (ZoneId/getAvailableZoneIds)
           :cljs (js/Intl.supportedValuesOf "timeZone"))))

(defn +
  "Adds n units to inst. Units: :millis :seconds :minutes :hours :days :weeks :months :years"
  [inst n unit]
  (add-platform inst n unit))

(defn -
  "Subtracts n units from inst. Units: :millis :seconds :minutes :hours :days :weeks :months :years"
  [inst n unit]
  (+ inst (clojure.core/- n) unit))

(defn next
  "Returns the next #inst matching the cron expression after the given inst.
  Optional tz-or-offset: timezone like \"America/Chicago\" or offset like \"-08:00\"
  (default UTC).

  Cron reference:
  * * * * *
  | | | | |
  | | | | day of the week (0–6) (Sunday to Saturday)
  | | | month (1–12)
  | | day of the month (1–31)
  | hour (0–23)
  minute (0–59)

  * any value
  , value list separator
  - range of values
  / step values"
  ([inst cron-str]
   (next inst cron-str "+00:00"))
  ([inst cron-str tz-or-offset]
   (find-cron inst cron-str tz-or-offset :next)))

(defn previous
  "Returns the previous #inst matching the cron expression before the given inst.
  Optional tz-or-offset: timezone like \"America/Chicago\" or offset like \"-08:00\"
  (default UTC).

  Cron reference:
  * * * * *
  | | | | |
  | | | | day of the week (0–6) (Sunday to Saturday)
  | | | month (1–12)
  | | day of the month (1–31)
  | hour (0–23)
  minute (0–59)

  * any value
  , value list separator
  - range of values
  / step values"
  ([inst cron-str]
   (previous inst cron-str "+00:00"))
  ([inst cron-str tz-or-offset]
   (find-cron inst cron-str tz-or-offset :previous)))
