#lang racket/base

(require racket/class
         (except-in racket/gui/base editor-snip% editor-canvas%)
         racket/serialize
         racket/pretty)

(require "main.rkt")

;(define m (new vertical-block$))
(define m (new horizontal-block$))

(define la (new vertical-block$))
;(send m add-child (new label$ [text "Hello"]))
(send la add-child (new label$ [text "Hello"]))
(send m add-child la)

(define lb (new vertical-block$))
;(send m add-child (new label$ [text "World"]))
(send lb add-child (new label$ [text "World"]))
(send m add-child lb)

(define f (new frame% [label "A List"]))
(new editor-canvas%
     [parent f]
     [editor m])
(send f show #t)
 