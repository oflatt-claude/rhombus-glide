#lang racket/base
;; The suite that needs nothing but Racket: no LibreOffice to render against, no
;; corpus to download. A few minutes, which is the one to run while working.
;;
;; What it does cover is everything with an exact answer -- the round trip
;; through the IR, the sync, the fuzzer, the parser's own units -- so a
;; regression in any of that shows up here rather than in the long sweep.
(require "unit.rkt" "fuzz.rkt" "structural.rkt" "flatten.rkt"
         "sync.rkt" "actions.rkt" "sessions.rkt" "scenarios.rkt" "watch.rkt")
