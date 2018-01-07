#lang racket/base

(require racket/class
         (except-in racket/gui/base editor-snip% editor-canvas%)
         racket/serialize
         racket/pretty)

(require "../editor/main.rkt"
         (submod "../editor/stdlib.rkt" editor))

(define m (new horizontal-block$))

(define list-a (new vertical-block$ [parent m]))
(new label$ [parent list-a]
     [text "Button A"])
(new button$ [parent list-a]
     [label (new label$ [text "Click"])])

(define list-b (new vertical-block$ [parent m]))
(new label$ [parent list-b]
     [text "Button B"])
(new button$ [parent list-b]
     [label (new label$ [text "ME!!!!"])])
(new label$ [parent list-b]
     [text "^^ The Cool Button ^^"])

(define f (new frame% [label "Editor"]))
(new editor-canvas%
     [parent f]
     [editor m])
(send f show #t)
