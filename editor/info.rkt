#lang info
(define collection "editor")
(define deps '(("base" "6.3")
               "syntax-color-lib"
               "draw-lib"
               "gui-lib"
               "images-lib"
               "wxme-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
;(define scribblings '(("scribblings/idmt.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(leif))
