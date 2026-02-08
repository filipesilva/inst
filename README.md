# inst

`inst` is a Clojure time library that always returns a Clojure `#inst`.

It is meant to complement Clojure's `#inst` reader and `inst?`, `inst-ms` functions.
Works in Clojure, ClojureScript, and Babashka and has no dependencies.

## Install

``` clojure
;; deps.edn
{:deps {io.github.filipesilva/inst {:git/tag "v1.0.0" :git/sha "..."}}}
```

## Usage

You can create an `#inst` with anything that `#inst` would take, with no args for now, with anything that would satisfy Clojure's `inst?`, or with ms:

``` clojure
(require '[filipesilva.inst :as inst])

(def t1 (inst/inst "2018-03-28")
(def t2 (inst/inst "2018-03-28T10:48:00.000")
(def t3 (inst/inst "2018-03-28T10:48:00.000-08:00")
(def t4 (inst/inst))
(def t5 (inst/inst #inst "2018-03-28"))
(def t6 (inst/inst (java.util.Date.)))
(def t7 (inst/inst (java.sql.Timestamp. 0)))
(def t8 (inst/inst (java.time.Instant/now)))
(def t9 (inst/inst (inst-ms #inst "2018-03-28")))
```

You can add/remove millis/seconds/minutes/days/weeks/months/years to `#inst`:

``` clojure
(inst/+ t1 3 :days)
(inst/- t1 500 :millis)
```

If you want to compare `#inst`s, use Clojure's `inst-ms` to turn them into numbers first:

``` clojure
(< (inst-ms t2) (inst-ms t3))
```

You can use a [cron expressions](https://en.wikipedia.org/wiki/Cron) to get the next or previous `#inst`:

``` clojure
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

(def every-tuesday "0 0 * * 2")
(def tz "-08:00")
(inst/next t3 every-tuesday)
(inst/next t3 every-tuesday tz)
(inst/previous t3 every-tuesday)
(inst/previous t3 every-tuesday tz)
```

