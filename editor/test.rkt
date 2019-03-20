#lang racket/base

(provide test-window
         editor-canvas%)
(require racket/class
         (prefix-in gui: racket/gui/base)
         "private/context.rkt")

(define (test-window editor)
  (define f (new gui:frame% [label "Test Window"]))
  (new editor-canvas% [parent f]
       [editor editor])
  (send f show #t))
