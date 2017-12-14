#lang racket/base

(require racket/class
         (except-in racket/gui/base editor-snip% editor-canvas%)
         racket/serialize
         racket/pretty)

(require "main.rkt")

(define idmt (new horizontal-block$))
(define list-a (new vertical-block$))
(send idmt add-child list-a)
(send list-a add-child (new label$ [text "Button A"]))
(define btn-a (new button$ [label (new label$ [text "Click"])]))
(send list-a add-child btn-a)
(define list-b (new vertical-block$))
(send idmt add-child list-b)
(send list-b add-child (new label$ [text "Button B"]))

(define f (new frame% [label "IDMT"]))
(new editor-canvas%
     [parent f]
     [editor idmt])
(send f show #t)
