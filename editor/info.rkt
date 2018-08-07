#lang info
(define collection "editor")
(define deps '(("base" "6.3")
               "syntax-color-lib"
               "draw-lib"
               "gui-lib"
               "images-lib"
               "wxme-lib"
               "drracket-plugin-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
;(define scribblings '(("scribblings/idmt.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(leif))
(define drracket-tools '(("private/surrogate.rkt")))
(define drracket-tool-name '("Editor"))
(define drracket-tool-icons '(#f))
