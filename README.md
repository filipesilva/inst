# inst

`inst` is a small Clojure time library that always returns a `#inst`.

It is meant to complement Clojure's `#inst` reader and `inst?`, `inst-ms` functions.
Works in Clojure, ClojureScript, and Babashka. No dependencies.

## Install

``` clojure
;; deps.edn
{:deps {io.github.filipesilva/inst {:git/tag "v1.0.2" :git/sha "6c66ed2"}}}
```

## Usage

``` clojure
(require '[filipesilva.inst :as inst])

;; create from string, ms, existing inst, or no args for now
(def t (inst/inst "2018-03-28T10:48:00.000-08:00"))
(inst/inst 1522195200000)
(inst/inst #inst "2018-03-28")
(inst/inst)

;; convert to string, round-trips through inst/inst
(inst/str t)
(= t (-> t inst/str inst/inst))

;; add and subtract
;; units: :millis :seconds :minutes :hours :days :weeks :months :years
(inst/+ t 3 :days)
(inst/- t 2 :months)

;; compare via inst-ms
(< (inst-ms t) (inst-ms (inst/inst)))

;; cron: find next/previous matching inst
;; * * * * *
;; | | | | |
;; | | | | day of the week (0–6) (Sunday to Saturday)
;; | | | month (1–12)
;; | | day of the month (1–31)
;; | hour (0–23)
;; minute (0–59)
;;
;; * any value
;; , value list separator
;; - range of values
;; / step values
(inst/next t "0 0 * * 2")     ; next Tuesday midnight
(inst/previous t "0 0 * * 2") ; previous Tuesday midnight

;; lazy seq of next values
(->> t
     (iterate #(inst/next % "0 0 * * 2"))
     (take 4))
;; => (#inst "2018-03-28T18:48:00.000-00:00"  ; t
;;     #inst "2018-04-03T00:00:00.000-00:00"
;;     #inst "2018-04-10T00:00:00.000-00:00"
;;     #inst "2018-04-17T00:00:00.000-00:00")
```

## Timezones

An `#inst` is always in `+00:00` (UTC) even it was created with an offset.
But sometimes we need to work with timezones at the edges to read or print times as they show in that timezone.

`inst`, `str`, `next`, and `previous` accept an optional `tz-or-offset` parameter.
It can either be a fixed offset like `"-06:00"` or a timezone like `"America/Chicago"`.
The timezone only affects how local time is interpreted or displayed and these functions still return a `#inst`.

An offset is a fixed number of hours from UTC:

``` clojure
;; "6am at offset -06:00" is always 6 hours behind UTC
(inst/inst "2024-01-15T06:00" "-06:00")
;; => #inst "2024-01-15T12:00:00.000-00:00"

(inst/inst "2024-07-15T06:00" "-06:00")
;; => #inst "2024-07-15T12:00:00.000-00:00"
;;                       ^ same 12:00 UTC both times, offset doesn't change
```

But a timezone maps to different offsets depending on the date due to daylight saving time (DST):

``` clojure
;; "6am in Chicago" offset changes with DST between CST and CDT
(inst/inst "2024-01-15T06:00" "America/Chicago")  ; winter: CST = -06:00
;; => #inst "2024-01-15T12:00:00.000-00:00"

(inst/inst "2024-07-15T06:00" "America/Chicago")  ; summer: CDT = -05:00
;; => #inst "2024-07-15T11:00:00.000-00:00"
;;                       ^ 11:00 UTC instead of 12:00, one hour difference from DST
```

`str` with a timezone shows what the clock reads in that zone:

``` clojure
(def noon (inst/inst "2024-01-15T12:00"))

(inst/str noon "America/Chicago")
;; => "2024-01-15T06:00:00.000-06:00"  ; winter offset

(inst/str (inst/inst "2024-07-15T12:00") "America/Chicago")
;; => "2024-07-15T07:00:00.000-05:00"  ; summer offset

;; round-trips back to the same inst
(= noon (-> noon (inst/str "America/Chicago") inst/inst))
;; => true
```

`next` and `previous` with a timezone give you DST-correct cron matching:

``` clojure
;; "next 6am in Chicago" is a different UTC hour in winter vs summer
(inst/next (inst/inst "2024-01-14") "0 6 * * *" "America/Chicago")
;; => #inst "2024-01-14T12:00:00.000-00:00"  ; 6am CST = 12:00 UTC

(inst/next (inst/inst "2024-07-14") "0 6 * * *" "America/Chicago")
;; => #inst "2024-07-14T11:00:00.000-00:00"  ; 6am CDT = 11:00 UTC

;; on spring-forward day, 2:30am doesn't exist so it resolves to 3:30am CDT
(inst/next (inst/inst "2024-03-10T07:00:00.000Z") "30 2 * * *" "America/Chicago")
;; => #inst "2024-03-10T08:30:00.000-00:00"  ; 2:30am CST skipped → 3:30am CDT
```

`tzs` returns all supported timezone strings:

``` clojure
(take 3 (inst/tzs))
;; => ("Africa/Abidjan" "Africa/Accra" "Africa/Addis_Ababa")
```

