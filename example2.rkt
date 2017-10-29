#lang racket/base

(require racket/class
         racket/gui/base
         racket/serialize
         racket/pretty)

(require "main.rkt")

(define idmt (new horizontal-block$))
(define list-a (new vertical-block$))
(send idmt add-idmt list-a)
(send list-a add-idmt (new label$ [text "Button A"]))
(define btn-a (new button$ [label (new label$ [text "Click"])]))
(send list-a add-idmt btn-a)
(define list-b (new vertical-block$))
(send idmt add-idmt list-b)
(send list-b add-idmt (new label$ [text "Button B"]))

(define f (new frame% [label "IDMT"]))
(new idmt-canvas%
     [parent f]
     [idmt idmt])
(send f show #t)
