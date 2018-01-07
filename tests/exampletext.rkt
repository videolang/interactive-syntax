#lang racket

(require (only-in racket/gui/base frame%))

(require "main.rkt")

(define m (new label$ [text "Hello World!"]))

(define f (new frame% [label "HI"]))
(new editor-canvas% [parent f]
     [editor m])
(send f show #t)
