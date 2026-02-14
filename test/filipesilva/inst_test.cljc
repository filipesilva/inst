(ns filipesilva.inst-test
  (:require [clojure.test :as t :refer [deftest is]]
            [filipesilva.inst :as inst]))

;; ---------------------------------------------------------------------------
;; inst creation
;; ---------------------------------------------------------------------------

(deftest inst-from-string
  (is (= (inst-ms (inst/inst "2018-03-28"))
         (inst-ms #inst "2018-03-28")))
  (is (= (inst-ms (inst/inst "2018-03-28T10:48:00.000"))
         (inst-ms #inst "2018-03-28T10:48:00.000")))
  (is (= (inst-ms (inst/inst "2018-03-28T10:48:00.000-08:00"))
         (inst-ms #inst "2018-03-28T10:48:00.000-08:00"))))

(deftest inst-now
  (is (<= (inst-ms (inst/inst))
          (inst-ms (inst/inst)))))

(deftest inst-from-inst
  (let [original #inst "2018-03-28"]
    (is (= (inst-ms (inst/inst original))
           (inst-ms original)))))

(deftest inst-from-ms
  (is (= (inst-ms (inst/inst 0)) 0))
  (is (= (inst-ms (inst/inst 1522195200000))
         (inst-ms #inst "2018-03-28"))))

;; ---------------------------------------------------------------------------
;; str
;; ---------------------------------------------------------------------------

(deftest inst-str
  (t/are [input expected]
    (= (inst/str (inst/inst input)) expected)
    "2018-03-28"                    "2018-03-28T00:00:00.000Z"
    "2018-03-28T10:48:00.000"       "2018-03-28T10:48:00.000Z"
    "2018-03-28T10:48:00.500"       "2018-03-28T10:48:00.500Z"
    "2018-03-28T10:48:00.000-08:00" "2018-03-28T18:48:00.000Z")
  ;; round-trip
  (let [t (inst/inst "2018-03-28T10:48:00.500")]
    (is (= t (-> t inst/str inst/inst)))))

;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(deftest add
  (t/are [input n unit expected]
    (= (inst-ms (inst/+ (inst/inst input) n unit))
       (inst-ms (inst/inst expected)))
    "2018-03-28"  500 :millis  "2018-03-28T00:00:00.500"
    "2018-03-28"  30  :seconds "2018-03-28T00:00:30.000"
    "2018-03-28"  90  :minutes "2018-03-28T01:30:00.000"
    "2018-03-28"  5   :hours   "2018-03-28T05:00:00.000"
    "2018-03-28"  3   :days    "2018-03-31"
    "2018-03-28"  2   :weeks   "2018-04-11"
    "2018-01-28"  1   :months  "2018-02-28"
    "2018-01-31"  1   :months  "2018-02-28"   ;; overflow clamps
    "2018-03-28"  2   :years   "2020-03-28"
    "2020-02-29"  1   :years   "2021-02-28")) ;; leap year clamp

(deftest subtract
  (t/are [input n unit expected]
    (= (inst-ms (inst/- (inst/inst input) n unit))
       (inst-ms (inst/inst expected)))
    "2018-03-28"  500 :millis  "2018-03-27T23:59:59.500"
    "2018-03-28"  3   :days    "2018-03-25"))

;; ---------------------------------------------------------------------------
;; Cron / next & previous
;; ---------------------------------------------------------------------------

(deftest cron-next
  (t/are [input cron expected]
    (= (inst-ms (inst/next (inst/inst input) cron))
       (inst-ms (inst/inst expected)))
    ;; every minute: at 10:48, next is 10:49
    "2018-03-28T10:48:00.000"  "* * * * *"       "2018-03-28T10:49:00.000"
    ;; specific minute: at 10:48, next :30 is 11:30
    "2018-03-28T10:48:00.000"  "30 * * * *"      "2018-03-28T11:30:00.000"
    ;; specific minute before: at 10:20, next :30 is 10:30
    "2018-03-28T10:20:00.000"  "30 * * * *"      "2018-03-28T10:30:00.000"
    ;; tuesday: 2018-03-28 is Wed, next Tue is Apr 3
    "2018-03-28T10:48:00.000"  "0 0 * * 2"       "2018-04-03T00:00:00.000"
    ;; day of month: at Mar 28, next 15th is Apr 15
    "2018-03-28T10:00:00.000"  "0 0 15 * *"      "2018-04-15T00:00:00.000"
    ;; specific month: at Mar 28, next Jun 1
    "2018-03-28T10:00:00.000"  "0 0 1 6 *"       "2018-06-01T00:00:00.000"
    ;; step: */15 at 10:48, next is 11:00
    "2018-03-28T10:48:00.000"  "*/15 * * * *"    "2018-03-28T11:00:00.000"
    ;; range: 9-17 at 18:00, next is 09:00 next day
    "2018-03-28T18:00:00.000"  "0 9-17 * * *"    "2018-03-29T09:00:00.000"
    ;; year boundary: Dec 31, next Jan 1 is next year
    "2018-12-31T10:00:00.000"  "0 0 1 1 *"       "2019-01-01T00:00:00.000"
    ;; leap year: from 2018, next Feb 29 is 2020
    "2018-03-01T00:00:00.000"  "0 0 29 2 *"      "2020-02-29T00:00:00.000"
    ;; comma list: 0,30 at 10:20, next is 10:30
    "2018-03-28T10:20:00.000"  "0,30 * * * *"    "2018-03-28T10:30:00.000"
    ;; range with step: 0-30/10 at 10:15, next is 10:20
    "2018-03-28T10:15:00.000"  "0-30/10 * * * *" "2018-03-28T10:20:00.000"
    ;; dom OR dow: 15th or Tue, Apr 3 (Tue) comes first
    "2018-03-28T10:00:00.000"  "0 0 15 * 2"      "2018-04-03T00:00:00.000")
  ;; with offset: next Tue midnight -08:00 = 08:00 UTC
  (is (= (inst-ms (inst/next (inst/inst "2018-03-28T10:48:00.000-08:00") "0 0 * * 2" "-08:00"))
         (inst-ms (inst/inst "2018-04-03T08:00:00.000")))))

(deftest cron-previous
  (t/are [input cron expected]
    (= (inst-ms (inst/previous (inst/inst input) cron))
       (inst-ms (inst/inst expected)))
    ;; every minute: at 10:48, prev is 10:47
    "2018-03-28T10:48:00.000"  "* * * * *"    "2018-03-28T10:47:00.000"
    ;; specific minute: at 10:48, prev :30 is 10:30
    "2018-03-28T10:48:00.000"  "30 * * * *"   "2018-03-28T10:30:00.000"
    ;; specific minute after: at 10:20, prev :30 is 09:30
    "2018-03-28T10:20:00.000"  "30 * * * *"   "2018-03-28T09:30:00.000"
    ;; tuesday: 2018-03-28 is Wed, prev Tue is Mar 27
    "2018-03-28T10:48:00.000"  "0 0 * * 2"    "2018-03-27T00:00:00.000"
    ;; day of month: at Mar 28, prev 15th is Mar 15
    "2018-03-28T10:00:00.000"  "0 0 15 * *"   "2018-03-15T00:00:00.000"
    ;; year boundary: Jan 2, prev Jan 1 is same year
    "2019-01-02T10:00:00.000"  "0 0 1 1 *"    "2019-01-01T00:00:00.000"
    ;; leap year: from 2021, prev Feb 29 is 2020
    "2021-03-01T00:00:00.000"  "0 0 29 2 *"   "2020-02-29T00:00:00.000")
  ;; with offset: prev Tue midnight -08:00 = 08:00 UTC
  (is (= (inst-ms (inst/previous (inst/inst "2018-03-28T10:48:00.000-08:00") "0 0 * * 2" "-08:00"))
         (inst-ms (inst/inst "2018-03-27T08:00:00.000")))))

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
  (is (nil? (inst/next (inst/inst "2018-03-28T10:00:00.000") "0 0 31 2 *"))))

#?(:cljs
   (do
     (defmethod t/report [:cljs.test/default :end-run-tests] [m]
       (when-not (t/successful? m)
         (throw (ex-info "Tests failed" {}))))
     (defn -main []
       (t/run-tests 'filipesilva.inst-test))))
