#lang racket/base

(require "private/lang.rkt"
         "private/stdlib.rkt"
         "private/editor.rkt"
         (for-syntax racket/base
                     "private/lang.rkt")
         (for-editor "private/lang.rkt"
                     (from-editor "private/stdlib.rkt")))

(begin-for-syntax
  (current-editor-lang 'racket/base)
  (current-editor-base "editor.rkt"))

(provide (~all-from-out "private/stdlib.rkt")
         (for-editor (~all-from-out (from-editor "private/stdlib.rkt")))
         define-editor
         define-editor-mixin
         begin-for-editor
         define-for-editor
         for-editor
         from-editor
         (rename-out [~require require]
                     [~all-from-out all-from-out]))
