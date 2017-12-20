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
    (send idmt add-child (new label$ [text (format "Item: ~a" counter)]))
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

(new button$ [parent idmt]
     [label (new label$ [text "CLICK ME!"])]
     [receiver (new add-item$)])
(new field$ [parent idmt])
(new button$ [parent idmt]
     [label (new label$ [text "SAVE"])]
     [receiver (new save$ [parent idmt])])
;(serialize idmt)
;(deserialize (serialize idmt))
(new editor-snip%
     [editor idmt])

(define f (new frame% [label "IDMT"]))
(new editor-canvas%
     [parent f]
     [editor idmt])
(send f show #t)
