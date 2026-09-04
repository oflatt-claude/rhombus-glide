#lang racket/base
;; Runs every test module. `raco test tests/all.rkt` is the whole suite.
;;
;; The order is deliberate: unit tests first because they are fast and their
;; failures explain the others, then emission, then fidelity, which needs
;; LibreOffice and takes the longest.
(require "unit.rkt" "coverage.rkt" "roundtrip.rkt" "fidelity.rkt" "export.rkt" "elements.rkt"
         "sync.rkt" "watch.rkt" "flatten.rkt" "structural.rkt" "corpus.rkt")
