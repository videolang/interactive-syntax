#lang racket/base

(provide (all-defined-out))
(require (submod "lang.rkt" key-submod))

(define ((mk-editor-list-proc key) modpath [ns #f])
  (define the-list (box '()))
  (with-continuation-mark key the-list
    (parameterize ([current-namespace (make-base-namespace)])
      (when ns
        (namespace-attach-module-declaration ns modpath))
      (with-handlers ([exn:fail? (λ (e) (void))])
        (dynamic-require modpath (void)))))
  (unbox the-list))

(define list-editors
  (mk-editor-list-proc editor-list-key))

(define list-editor-mixins
  (mk-editor-list-proc editor-mixin-list-key))
