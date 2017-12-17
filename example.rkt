#lang racket/base

(require racket/class
         (only-in racket/gui/base frame%)
         racket/serialize
         racket/pretty)

(require "main.rkt")

(define idmt (new vertical-block$))
(define counter 0)
(define-editor add-item$ (receiver$$ base$)
  (super-new)
  (define/override (on-receive event)
    (send idmt add-idmt (new label$ [text (format "Item: ~a" counter)]))
    (set! counter (add1 counter))))
(define-editor save$ (receiver$$ widget$)
  (super-new)
  (define/override (on-receive event)
    (pretty-print (serialize (send this get-parent)))))
(new label$
     [parent idmt]
     [text "Hello"])
(new label$
     [parent idmt]
     [text "World"])
(new label$
     [parent idmt]
     [text "I am an IDMT!!!"])
idmt

(define f (new frame% [label "IDMT"]))
(new idmt-canvas%
     [parent f]
     [idmt idmt])
(send f show #t)

#|
(define btn (new button$ [label (new label$ [text "CLICK ME!"])]))
(send btn register-receiver (new add-item$))
(send idmt add-idmt btn)
(send idmt add-idmt (new field$))
(define btn2 (new button$ [label (new label$ [text "SAVE"])]))
(define save (new save$))
(send btn2 register-receiver save)
(send save register-parent idmt)
(send idmt add-idmt btn2)
;(serialize idmt)
;(deserialize (serialize idmt))
;(new idmt-snip%
;     [idmt idmt])
|#
