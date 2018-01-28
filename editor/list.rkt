#lang racket/base

(provide (all-defined-out))

(define ((mk-editor-list-proc symb) modpath [ns #f])
  (parameterize ([current-namespace (make-base-namespace)])
    (when ns
      (namespace-attach-module-declaration ns modpath))
    (with-handlers ([exn:fail? (λ (e) (void))])
      (dynamic-require modpath (void)))
    (define the-list
      (dynamic-require-for-syntax '(submod editor/lang submod-acc)
                                  symb))
    (unbox the-list)))

(define list-editors
  (mk-editor-list-proc 'editor-list-box))

(define list-editor-mixins
  (mk-editor-list-proc 'editor-mixin--list-box))
