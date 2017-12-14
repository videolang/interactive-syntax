#lang racket/base

(require racket/class
         (except-in racket/gui/base editor-snip% editor-canvas%)
         racket/serialize
         racket/pretty)

(require "main.rkt")

(define m (new vertical-block$))
(send m add-child (new label$ [text "Hello"]))
(send m add-child (new label$ [text "World"]))

(define f (new frame% [label "A List"]))
(new editor-canvas%
     [parent f]
     [editor m])
(send f show #t)
