#lang racket/load

(require racket/match
         racket/dict
         syntax/modresolve
         rackunit)

(define identifier->original-identifier
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'editor/private/editor)
    (parameterize ([current-namespace (module->namespace 'editor/private/editor)])
      (eval '(let-syntax ([foo (λ (stx) #`'#,identifier->original-identifier)])
               (foo))))))

(module direct racket
  (provide x)
  (define-syntax x 42))
(require 'direct)
(check-equal? (identifier->original-identifier ''direct 'x)
              (cons ''direct 'x))

(module direct-rename racket
  (provide (rename-out [x y]))
  (define-syntax x 42))
(require 'direct-rename)
(check-equal? (identifier->original-identifier ''direct-rename 'y)
              (cons ''direct-rename 'x))

(module indirect-reprovide racket
  (require 'direct-rename)
  (provide y))
(require 'indirect-reprovide)
(check-equal? (identifier->original-identifier ''indirect-reprovide 'y)
              (cons (module-path-index-join ''direct-rename #f) 'x))

(module indirect-rename racket
  (require 'direct-rename)
  (provide (rename-out [y z])))
(require 'indirect-rename)
(check-equal? (identifier->original-identifier ''indirect-rename 'z)
              (cons (module-path-index-join ''direct-rename #f) 'x))

(module for-syntax-reprovide racket
  (require (for-syntax 'direct-rename))
  (provide (for-syntax y)))
(require 'for-syntax-reprovide)
(check-equal? (identifier->original-identifier ''for-syntax-reprovide 'y 1)
              (cons (module-path-index-join ''direct-rename #f) 'x))
