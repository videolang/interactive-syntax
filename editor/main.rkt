#lang racket/base

(module reader racket/base

  (provide (rename-out [e:read read]
                       [e:read-syntax read-syntax]
                       [e:get-info get-info])
           wrap-reader
           wrap-info)
  (require syntax/module-reader
           syntax/parse
           racket/match
           racket/pretty
           "private/read-editor.rkt")

  (define ((wrap-reader t) . args)
    (define outer-scope (make-syntax-introducer #t))
    (parameterize ([current-readtable (make-editor-readtable #:outer-scope outer-scope)])
      (define stx (apply t args))
      (if (syntax? stx)
          (syntax-parse stx
            [(module name lang
               (mod-beg (~optional (~seq #:headers (headers ...)) #:defaults ([(headers 1) '()]))
                        body ...))
             (outer-scope
              #`(module name lang
                  (mod-beg
                   headers ...
                   #,(datum->syntax #f '(#%require (only editor/private/editor #%editor)))
                   ;#,(outer-scope #'(#%require (only editor/base)))
                   body ...)))])
          (match stx
            [`(module ,name ,lang
                (,mod-beg #:headers (,headers ...)
                          ,body ...))
             `(module ,name ,lang
                ,@headers
                ,@body)]
            [_ stx]))))

  (define ((wrap-info defproc) key default)
    (case key
      [(color-lexer)
       (lex-editor (defproc 'color-lexer default))]
      [(definitions-text-surrogate)
       'editor/private/surrogate-base]
      [(definitions-text-surrogate-list)
       (define base-list
         (or (defproc key default)
             (let* ([alt (defproc 'definitions-text-surrogate default)])
               (and alt (list alt)))))
       (if base-list
           (cons 'editor/private/surrogate base-list)
           (list 'editor/private/surrogate-base))]
      [(drracket:toolbar-buttons)
       (define others (defproc key default))
       (list* (dynamic-require 'editor/private/surrogate 'toggle-button)
              (dynamic-require 'editor/private/editselect 'insert-button)
              (if (list? others)
                  others
                  '()))]
      [else (defproc key default)]))

  (define-values (e:read e:read-syntax e:get-info)
    (make-meta-reader
     'editor
     "Embedded Editors"
     lang-reader-module-paths
     wrap-reader
     wrap-reader
     wrap-info)))
