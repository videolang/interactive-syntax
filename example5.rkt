#lang racket/base

(require racket/class
         (except-in racket/gui/base editor-snip% editor-canvas%)
         racket/serialize
         racket/pretty)

(require "main.rkt")

(define m (new button$ [label (new label$ [text "Click Me Please"])]))

(log-editor-debug "")

(define f (new frame% [label "IDMT"]))
(new editor-canvas%
     [parent f]
     [editor m])
(send f show #t)