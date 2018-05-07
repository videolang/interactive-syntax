#lang racket/base

(require "base.rkt"
         "private/lang.rkt"
         (for-syntax racket/base
                     "private/lang.rkt")
         (for-editor "private/lang.rkt"
                     (from-editor "base.rkt")))

(begin-for-syntax
  (current-editor-lang 'racket/base)
  (current-editor-base "editor.rkt"))

(provide (all-from-out "base.rkt")
         (for-editor (~all-from-out (from-editor "base.rkt"))))

#|
(require "base.rkt"
         "private/lang.rkt")


(provide (all-from-out "base.rkt")
         (for-editor (all-from-out (from-editor "base.rkt"))))

|#