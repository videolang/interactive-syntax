#lang racket/base

(provide test-window
         editor-canvas%
         editor->string
         editor->sexp)
(require racket/class
         racket/port
         (prefix-in gui: racket/gui/base)
         "private/context.rkt"
         "private/read-editor.rkt")

(define (test-window editor)
  (define f (new gui:frame% [label "Test Window"]))
  (new editor-canvas% [parent f]
       [editor editor])
  (send f show #t))

(define (editor->string editor)
  (define f (new editor-snip% [editor editor]))
  (send f get-text 0 #f))

(define (editor->sexp editor)
  (parameterize ([current-readtable (make-editor-readtable)])
    (with-input-from-string (editor->string editor) read)))
