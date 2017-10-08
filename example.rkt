#lang racket/base

(require racket/class
         racket/gui/base
         racket/serialize)

(require "main.rkt")

(define f (new frame% [label "IDMT"]))
(define idmt (new vertical-block$))
(define counter 0)
(define-idmt add-item$ (receiver$ base$)
  (super-new)
  (define/override (on-receive event)
    (send idmt add-idmt (new label$ [text (format "Item: ~a" counter)]))
    (set! counter (add1 counter))))
(send idmt add-idmt (new label$ [text "Hello"]))
(send idmt add-idmt (new label$ [text "World"]))
(send idmt add-idmt (new label$ [text "I am an IDMT!!!"]))
(define btn (new button$ [label (new label$ [text "CLICK ME!"])]))
(send btn register-receiver (new add-item$))
(send idmt add-idmt btn)
(send idmt add-idmt (new field$))
(new idmt-canvas%
     [parent f]
     [idmt idmt])
(send f show #t)
(serialize idmt)
(deserialize (serialize idmt))
