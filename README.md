# inst

`inst` is a small Clojure time library that always returns a `#inst`.

It is meant to complement Clojure's `#inst` reader and `inst?`, `inst-ms` functions.
Works in Clojure, ClojureScript, and Babashka. No dependencies.

## Install

``` clojure
;; deps.edn
{:deps {io.github.filipesilva/inst {:git/tag "v1.0.1" :git/sha "5843e31"}}}
```

## Usage

``` clojure
(require '[filipesilva.inst :as inst])

;; create from string, ms, existing inst, or no args for now
(def t (inst/inst "2018-03-28T10:48:00.000-08:00"))
(inst/inst 1522195200000)
(inst/inst #inst "2018-03-28")
(inst/inst)

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
(inst/next t "0 0 * * 2")              ; next Tuesday midnight
(inst/next t "0 0 * * 2" "-08:00")     ; with offset
(inst/previous t "0 0 * * 2")          ; previous Tuesday midnight

;; lazy seq of next values
(->> t
     (iterate #(inst/next % "0 0 * * 2"))
     (take 4))
;; => (#inst "2018-03-28T18:48:00.000-00:00"  ; t
;;     #inst "2018-04-03T00:00:00.000-00:00"
;;     #inst "2018-04-10T00:00:00.000-00:00"
;;     #inst "2018-04-17T00:00:00.000-00:00")
```

