#lang racket/base
;; The suite that needs nothing but Racket: no LibreOffice to render against, no
;; corpus to download. A few minutes, which is the one to run while working.
;;
;; What it does cover is everything with an exact answer -- the round trip
;; through the IR, the sync, the fuzzer, the parser's own units -- so a
;; regression in any of that shows up here rather than in the long sweep.
(require rackunit/log
         "unit.rkt" "fuzz.rkt" "structural.rkt" "flatten.rkt"
         "sync.rkt" "actions.rkt" "sessions.rkt" "scenarios.rkt" "watch.rkt")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
