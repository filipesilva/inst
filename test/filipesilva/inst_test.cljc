(ns filipesilva.inst-test
  (:require [clojure.test :as t :refer [deftest is]]
            [filipesilva.inst :as inst]))

;; ---------------------------------------------------------------------------
;; inst creation
;; ---------------------------------------------------------------------------

(deftest inst-from-string
  (let [t (inst/inst "2018-03-28")]
    (is (inst? t))
    (is (= (inst-ms t) (inst-ms #inst "2018-03-28")))))

(deftest inst-from-string-with-time
  (let [t (inst/inst "2018-03-28T10:48:00.000")]
    (is (inst? t))
    (is (= (inst-ms t) (inst-ms #inst "2018-03-28T10:48:00.000")))))

(deftest inst-from-string-with-offset
  (let [t (inst/inst "2018-03-28T10:48:00.000-08:00")]
    (is (inst? t))
    (is (= (inst-ms t) (inst-ms #inst "2018-03-28T10:48:00.000-08:00")))))

(deftest inst-now
  (let [before (inst-ms (inst/inst))
        after  (inst-ms (inst/inst))]
    (is (inst? (inst/inst)))
    (is (<= before after))))

(deftest inst-from-inst
  (let [original #inst "2018-03-28"
        copy     (inst/inst original)]
    (is (inst? copy))
    (is (= (inst-ms original) (inst-ms copy)))))

(deftest inst-from-ms
  (let [t (inst/inst 0)]
    (is (inst? t))
    (is (= 0 (inst-ms t))))
  (let [t (inst/inst 1522195200000)]
    (is (= (inst-ms t) (inst-ms #inst "2018-03-28")))))


;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(deftest add-days
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/+ t 3 :days))
           (inst-ms #inst "2018-03-31")))))

(deftest subtract-days
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/- t 3 :days))
           (inst-ms #inst "2018-03-25")))))

(deftest add-millis
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/+ t 500 :millis))
           (+ (inst-ms #inst "2018-03-28") 500)))))

(deftest subtract-millis
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/- t 500 :millis))
           (- (inst-ms #inst "2018-03-28") 500)))))

(deftest add-seconds
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/+ t 30 :seconds))
           (+ (inst-ms #inst "2018-03-28") 30000)))))

(deftest add-minutes
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/+ t 90 :minutes))
           (+ (inst-ms #inst "2018-03-28") (* 90 60000))))))

(deftest add-hours
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/+ t 5 :hours))
           (+ (inst-ms #inst "2018-03-28") (* 5 3600000))))))

(deftest add-weeks
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/+ t 2 :weeks))
           (inst-ms #inst "2018-04-11")))))

(deftest add-months
  (let [t (inst/inst "2018-01-28")]
    (is (= (inst-ms (inst/+ t 1 :months))
           (inst-ms #inst "2018-02-28")))))

(deftest add-months-overflow
  (let [t (inst/inst "2018-01-31")]
    ;; Jan 31 + 1 month = Feb 28 (2018 is not a leap year)
    (is (= (inst-ms (inst/+ t 1 :months))
           (inst-ms #inst "2018-02-28")))))

(deftest add-years
  (let [t (inst/inst "2018-03-28")]
    (is (= (inst-ms (inst/+ t 2 :years))
           (inst-ms #inst "2020-03-28")))))

(deftest add-years-leap
  ;; Feb 29 2020 + 1 year = Feb 28 2021
  (let [t (inst/inst "2020-02-29")]
    (is (= (inst-ms (inst/+ t 1 :years))
           (inst-ms #inst "2021-02-28")))))

;; ---------------------------------------------------------------------------
;; Cron / next
;; ---------------------------------------------------------------------------

(deftest next-every-minute
  ;; At 10:48, next * * * * * should be 10:49
  (let [t    (inst/inst "2018-03-28T10:48:00.000")
        nxt  (inst/next t "* * * * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-03-28T10:49:00.000")))))

(deftest next-specific-minute
  ;; At 10:48, next 30 * * * * should be 11:30
  (let [t   (inst/inst "2018-03-28T10:48:00.000")
        nxt (inst/next t "30 * * * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-03-28T11:30:00.000")))))

(deftest next-specific-minute-before
  ;; At 10:20, next 30 * * * * should be 10:30
  (let [t   (inst/inst "2018-03-28T10:20:00.000")
        nxt (inst/next t "30 * * * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-03-28T10:30:00.000")))))

(deftest next-tuesday
  ;; 2018-03-28 is a Wednesday, next Tuesday is 2018-04-03
  (let [t   (inst/inst "2018-03-28T10:48:00.000")
        nxt (inst/next t "0 0 * * 2")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-04-03T00:00:00.000")))))

(deftest next-tuesday-with-tz
  ;; With -08:00 offset, "next Tuesday midnight local" should be
  ;; 2018-04-03T08:00:00.000Z (midnight -08:00 = 08:00 UTC)
  (let [t   (inst/inst "2018-03-28T10:48:00.000-08:00")
        nxt (inst/next t "0 0 * * 2" "-08:00")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-04-03T08:00:00.000")))))

(deftest next-specific-day-of-month
  ;; At March 28, next "0 0 15 * *" = April 15
  (let [t   (inst/inst "2018-03-28T10:00:00.000")
        nxt (inst/next t "0 0 15 * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-04-15T00:00:00.000")))))

(deftest next-specific-month
  ;; At March 28, next "0 0 1 6 *" = June 1
  (let [t   (inst/inst "2018-03-28T10:00:00.000")
        nxt (inst/next t "0 0 1 6 *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-06-01T00:00:00.000")))))

(deftest next-step-minutes
  ;; Every 15 minutes: "*/15 * * * *"
  ;; At 10:48, next should be 11:00
  (let [t   (inst/inst "2018-03-28T10:48:00.000")
        nxt (inst/next t "*/15 * * * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-03-28T11:00:00.000")))))

(deftest next-range-hours
  ;; "0 9-17 * * *" = on the hour, 9am-5pm
  ;; At 18:00, next should be 09:00 next day
  (let [t   (inst/inst "2018-03-28T18:00:00.000")
        nxt (inst/next t "0 9-17 * * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-03-29T09:00:00.000")))))

(deftest next-year-boundary
  ;; At Dec 31, next "0 0 1 1 *" should be Jan 1 next year
  (let [t   (inst/inst "2018-12-31T10:00:00.000")
        nxt (inst/next t "0 0 1 1 *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2019-01-01T00:00:00.000")))))

(deftest next-leap-year-feb-29
  ;; "0 0 29 2 *" = Feb 29, only matches leap years
  ;; From 2018, next Feb 29 is 2020
  (let [t   (inst/inst "2018-03-01T00:00:00.000")
        nxt (inst/next t "0 0 29 2 *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2020-02-29T00:00:00.000")))))

;; ---------------------------------------------------------------------------
;; Cron / previous
;; ---------------------------------------------------------------------------

(deftest previous-every-minute
  ;; At 10:48, previous * * * * * should be 10:47
  (let [t   (inst/inst "2018-03-28T10:48:00.000")
        prv (inst/previous t "* * * * *")]
    (is (= (inst-ms prv) (inst-ms #inst "2018-03-28T10:47:00.000")))))

(deftest previous-specific-minute
  ;; At 10:48, previous 30 * * * * should be 10:30
  (let [t   (inst/inst "2018-03-28T10:48:00.000")
        prv (inst/previous t "30 * * * *")]
    (is (= (inst-ms prv) (inst-ms #inst "2018-03-28T10:30:00.000")))))

(deftest previous-specific-minute-after
  ;; At 10:20, previous 30 * * * * should be 09:30
  (let [t   (inst/inst "2018-03-28T10:20:00.000")
        prv (inst/previous t "30 * * * *")]
    (is (= (inst-ms prv) (inst-ms #inst "2018-03-28T09:30:00.000")))))

(deftest previous-tuesday
  ;; 2018-03-28 is a Wednesday, previous Tuesday is 2018-03-27
  (let [t   (inst/inst "2018-03-28T10:48:00.000")
        prv (inst/previous t "0 0 * * 2")]
    (is (= (inst-ms prv) (inst-ms #inst "2018-03-27T00:00:00.000")))))

(deftest previous-tuesday-with-tz
  ;; With -08:00 offset, previous Tuesday midnight local
  ;; 2018-03-27T08:00:00.000Z (midnight -08:00 = 08:00 UTC)
  (let [t   (inst/inst "2018-03-28T10:48:00.000-08:00")
        prv (inst/previous t "0 0 * * 2" "-08:00")]
    (is (= (inst-ms prv) (inst-ms #inst "2018-03-27T08:00:00.000")))))

(deftest previous-specific-day-of-month
  ;; At March 28, previous "0 0 15 * *" = March 15
  (let [t   (inst/inst "2018-03-28T10:00:00.000")
        prv (inst/previous t "0 0 15 * *")]
    (is (= (inst-ms prv) (inst-ms #inst "2018-03-15T00:00:00.000")))))

(deftest previous-year-boundary
  ;; At Jan 2, previous "0 0 1 1 *" should be Jan 1 same year
  (let [t   (inst/inst "2019-01-02T10:00:00.000")
        prv (inst/previous t "0 0 1 1 *")]
    (is (= (inst-ms prv) (inst-ms #inst "2019-01-01T00:00:00.000")))))

(deftest previous-leap-year-feb-29
  ;; "0 0 29 2 *" = Feb 29, only matches leap years
  ;; From 2021, previous Feb 29 is 2020
  (let [t   (inst/inst "2021-03-01T00:00:00.000")
        prv (inst/previous t "0 0 29 2 *")]
    (is (= (inst-ms prv) (inst-ms #inst "2020-02-29T00:00:00.000")))))

;; ---------------------------------------------------------------------------
;; Cron / parse features
;; ---------------------------------------------------------------------------

(deftest next-comma-list
  ;; "0,30 * * * *" = on the hour and half hour
  ;; At 10:20, next should be 10:30
  (let [t   (inst/inst "2018-03-28T10:20:00.000")
        nxt (inst/next t "0,30 * * * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-03-28T10:30:00.000")))))

(deftest next-range-with-step
  ;; "0-30/10 * * * *" = minutes 0, 10, 20, 30
  ;; At 10:15, next should be 10:20
  (let [t   (inst/inst "2018-03-28T10:15:00.000")
        nxt (inst/next t "0-30/10 * * * *")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-03-28T10:20:00.000")))))

(deftest next-dom-and-dow-or
  ;; When both day-of-month and day-of-week are specified, either should match.
  ;; "0 0 15 * 2" = the 15th of the month OR any Tuesday
  ;; 2018-03-28 is a Wednesday. Next match: Tuesday April 3 or March 15 (past).
  ;; The next Tuesday is April 3, and the next 15th is April 15.
  ;; April 3 comes first.
  (let [t   (inst/inst "2018-03-28T10:00:00.000")
        nxt (inst/next t "0 0 15 * 2")]
    (is (= (inst-ms nxt) (inst-ms #inst "2018-04-03T00:00:00.000")))))

(deftest next-lazy-seq
  ;; same t as README
  (let [t      (inst/inst "2018-03-28T10:48:00.000-08:00")
        cron   "0 0 * * 2"
        tues   (->> t
                    (iterate #(inst/next % cron))
                    (take 4))]
    (is (= (mapv inst-ms tues)
           (mapv inst-ms [#inst "2018-03-28T18:48:00.000"
                          #inst "2018-04-03T00:00:00.000"
                          #inst "2018-04-10T00:00:00.000"
                          #inst "2018-04-17T00:00:00.000"])))))

(deftest next-impossible-cron-returns-nil
  ;; Feb 31 never exists, should return nil
  (let [t (inst/inst "2018-03-28T10:00:00.000")]
    (is (nil? (inst/next t "0 0 31 2 *")))))

#?(:cljs
   (do
     (defmethod t/report [:cljs.test/default :end-run-tests] [m]
       (when-not (t/successful? m)
         (.exit js/process 1)))
     (defn -main []
       (t/run-tests 'filipesilva.inst-test))))
