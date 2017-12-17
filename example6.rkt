#lang racket/base

(require racket/class
         (except-in racket/gui/base editor-snip% editor-canvas%)
         racket/serialize
         racket/pretty)

(require "main.rkt")

(define m (new vertical-block$))
(new label$ [parent m]
     [text "Hello World!"])
(new field$ [parent m])

(define f (new frame% [label "An Editor"]))
(new editor-canvas%
     [parent f]
     [editor m])
(send f show #t)
