#lang info

(define collection "glide-pptx")
(define version "0.0")
(define license 'MIT)

(define deps '("base" "pict-lib" "draw-lib" "rhombus-lib" "shrubbery-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "Direct manipulation of Rhombus and Racket pict slideshows through PowerPoint or Keynote.")

;; `raco glide-pptx <command>`
(define raco-commands
  '(("glide-pptx" glide-pptx/main "translate, export and sync PowerPoint decks" 60)))
