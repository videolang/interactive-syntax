#lang racket/base

(require "lang.rkt"
         racket/list
         racket/stxparam
         racket/class
         racket/splicing
         syntax/location
         racket/serialize
         syntax/parse/define
         (for-syntax racket/base
                     (submod "lang.rkt" key-submod)
                     racket/serialize
                     racket/function
                     syntax/location
                     racket/syntax
                     syntax/parse))
(provide (all-defined-out)
         (all-from-out "lang.rkt"))

;; Keys for hidden methods. Only functions in
;;   this module should access these methods directly.
(define serial-key (generate-member-key))
(define deserial-key (generate-member-key))
(define copy-key (generate-member-key))
;(define elaborator-key (generate-member-key))
(define elaborator-key (member-name-key elaborate))

;; Only introduced by #editor reader macro. Handles deserializing
;;  the editor.
(define-syntax (#%editor stx)
  (syntax-parse stx
    [(_ (elaborator-binding elaborator-name) body)
     (define/syntax-parse elaborator
       (syntax-local-lift-require (deserialize (syntax->datum #'elaborator-binding))
                                  (datum->syntax #f (syntax->datum #'elaborator-name))))
     #'(elaborator body)]))

;; Returns an identifier that contains
;;   the binding for an editor's elaborator.
;; To be put into the #editor()() form.
;; (is-a?/c editor$) -> (listof module-path-index? symbol?)
(define (editor->elaborator editor)
  (define-member-name elaborator elaborator-key)
  (send editor elaborator))

(define-syntax-parser define-getter
  [(_ _:id _:id #f)
   (syntax/loc this-syntax (void))]
  [(_ state:id getter:id #t)
   (quasisyntax/loc this-syntax
     (define-getter state getter #,(syntax/loc this-syntax (λ () state))))]
  [(_ _:id getter:id body)
   (syntax/loc this-syntax (define/public getter body))])

(define-syntax-parser define-setter
  [(_ _:id _:id #f)
   (syntax/loc this-syntax (void))]
  [(_ state:id setter:id #t)
   (quasisyntax/loc this-syntax
     (define-setter state setter #,(syntax/loc this-syntax (λ (new-val) (set! state new-val)))))]
  [(_ _:id setter:id body)
   (syntax/loc this-syntax (define/public setter body))])

(begin-for-syntax
  (define-syntax-class defelaborate
    #:literals (define-elaborate)
    (pattern (define-elaborate data body ...+)))
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state marked-name:id
               (~alt (~optional (~seq #:persistence persistence) #:defaults ([persistence #'#t]))
                     (~optional (~seq #:getter getter) #:defaults ([getter #'#f]))
                     (~optional (~seq #:setter setter) #:defaults ([setter #'#f]))
                     (~once default))
               ...)
             #:attr name (editor-syntax-introduce (attribute marked-name))
             #:attr getter-name (format-id this-syntax "get-~a" #'name)
             #:attr setter-name (format-id this-syntax "set-~a!" #'name))))

(define-syntax-parameter define-elaborate
  (syntax-parser
    [de:defelaborate
     (raise-syntax-error 'define-elaborate "Use outside of define-editor is an error" this-syntax)]))

(define-syntax-parameter define-state
  (syntax-parser
    [x:defstate
     (raise-syntax-error 'define-state "Use outside of define-editor is an error" this-syntax)]))

;; We don't want to get editor classes when
;; deserializing new editors.
(define deserialize-editor-classes?
  (make-parameter #t))

;; Each editor definition has three parts:
;; 1. A phase 1 elaboration
;; 2. A submodule with interaction code
;; 3. A deserializer submodule
(define-syntax (~define-editor stx)
  (syntax-parse stx
    [(_ orig-stx name:id supclass (interfaces ...)
        (~or (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
             (~optional (~seq #:direct-deserialize? dd?) #:defaults ([dd? #'#t])))
        ...
        (~and
         (~seq (~or plain-state:defstate
                    (~optional elaborator:defelaborate
                               #:defaults ([elaborator.data #'this]
                                           [(elaborator.body 1) (list #'this)]))
                    internal-body) ...)
         (~seq body ...)))
     #:with elaborator-name (format-id stx "~a:elaborate" #'name)
     #:with name-deserialize (format-id stx "~a:deserialize" #'name)
     #:with (marked-interfaces ...) (editor-syntax-introduce #'(interfaces ...))
     #:with (marked-body ...) (editor-syntax-introduce #'(body ...) 'add)
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(,(syntax-parameter-value #'current-editor-lang)
                                     racket/class
                                     racket/serialize
                                     ,(syntax-parameter-value #'current-editor-base)))
     #:with marked-supclass (editor-syntax-introduce #'supclass)
     #:with (state:defstate ...) (editor-syntax-introduce #'(plain-state ...))
     #;((dynamic-require 'racket/pretty 'pretty-print) (list (attribute elaborator)
                                                           (attribute elaborator.data)
                                                           (attribute elaborator.body)))
     (define dd?* (syntax-e #'dd?))
     (unless (or (not dd?*)
                 (eq? 'module-begin (syntax-local-context)) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define serialize-method (gensym 'serialize))
     (define deserialize-method (gensym 'deserialize))
     (define copy-method (gensym 'copy))
     (define elaborator-method (gensym 'elaborator))
     (define state-methods (for/list ([i (in-list (attribute state.getter-name))])
                             (gensym (syntax->datum i))))
     (define base? (syntax-e (attribute b?)))
     #`(begin
         #,@(if dd?*
                (list #'(provide elaborator-name)
                      #'(begin-for-syntax
                          (let ()
                            (define b (continuation-mark-set-first #f editor-list-key))
                            (when (and b (box? b))
                              (set-box! b (cons #'name (unbox b)))))))
                '())
         (define-syntax (elaborator-name stx)
           (syntax-parse stx
             [(_ data)
              #'(let ()
                  (define elaborator.data
                    (parameterize (#;[deserialize-module-guard
                                    (λ (mod name)
                                      (void))])
                      (deserialize 'data)))
                  elaborator.body ...)]))
         (#,@(if dd?*
                 #`(editor-submod
                    (require marked-reqs ...)
                    (#%require #,(quote-module-path)))
                 #'(begin))
          (define-member-name #,serialize-method serial-key)
          (define-member-name #,deserialize-method deserial-key)
          (define-member-name #,copy-method copy-key)
          (define-member-name #,elaborator-method elaborator-key)
          #,@(if dd?*
                 (list
                  #'(provide name)
                  #`(module+ deserialize
                     (provide name-deserialize)
                     (define name-deserialize
                       (make-deserialize-info
                        (λ (sup table)
                          (define this (new name))
                          (send this #,deserialize-method (vector sup table))
                          (send this on-state-changed)
                          this)
                        (λ ()
                          (define pattern (new name))
                          (values pattern
                                  (λ (other)
                                    (send pattern #,copy-method other)
                                    (send pattern on-state-changed))))))))
                  '())
          (splicing-syntax-parameterize
              ([define-state
                 (syntax-parser
                   [st:defstate
                    #`(begin
                        #,(syntax/loc #'st (define st.marked-name st.default))
                        #,(syntax/loc #'st (define-getter st.marked-name st.getter-name st.getter))
                        #,(syntax/loc #'st (define-setter st.marked-name st.setter-name st.setter)))])]
               [define-elaborate
                 (syntax-parser
                   [de:defelaborate
                    #'(begin)])])
            (define name
              (let ()
                #,@(for/list ([sm (in-list state-methods)])
                     #`(define-local-member-name #,sm))
                (class/derived
                 orig-stx
                 (name
                  marked-supclass
                  ((interface* () ([prop:serializable
                                    (make-serialize-info
                                     (λ (this)
                                       (send this #,serialize-method))
                                     (cons 'name-deserialize
                                           (module-path-index-join (quote-module-path deserialize)
                                                                   #f))
                                     #t
                                     (or (current-load-relative-directory) (current-directory)))]))
                   marked-interfaces ...)
                  #f)
                 #,@(if dd?*
                        (list
                         #`(define (#,elaborator-method)
                             (list #,(if dd?* #'(quote-module-path "..") #'(quote-module-path))
                                   'elaborator-name))
                         #`(#,(if base? #'public #'override) #,elaborator-method))
                        '())
                 (define (#,serialize-method)
                   (vector #,(if base?
                                 #'#f
                                 #`(super #,serialize-method))
                           (let ()
                             (define state-vars
                               `((state.marked-name
                                  ,state.marked-name
                                  ,state.persistence)
                                 ...))
                             (for/hash ([var (in-list state-vars)]
                                        #:when (third var))
                               (values (first var) (second var))))))
                 (#,(if base? #'public #'override) #,serialize-method)
                 (define (#,deserialize-method data)
                   (define sup (vector-ref data 0))
                   (define table (vector-ref data 1))
                   #,(if base?
                         #`(void)
                         #`(super #,deserialize-method sup))
                   #,@(for/list ([i (in-list (attribute state.marked-name))]
                                 [p? (in-list (attribute state.persistence))])
                        (define key (syntax->datum i))
                        #`(when (hash-has-key? table '#,key)
                            (define other-val (hash-ref table '#,key))
                            (let ([p* #,p?])
                              (case p*
                                [(#t) (set! #,i other-val)]
                                [(#f) (void)]
                                [else (set! #,i (p* #,i other-val))])))))
                 (#,(if base? #'public #'override) #,deserialize-method)
                 (define (#,copy-method other)
                   #,(if base?
                         #`(void)
                         #`(super #,copy-method other))
                   #,@(for/list ([i (in-list (attribute state.marked-name))]
                                 [get (in-list state-methods)])
                        #`(set! #,i (send other #,get)))
                   (void))
                 (#,(if base? #'public #'override) #,copy-method)
                 #,@(for/list ([i (in-list (attribute state.marked-name))]
                               [sm (in-list state-methods)])
                      #`(define/public (#,sm) #,i))
                 marked-body ...))))))]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-editor #,stx name super (interfaces ...) #:base? #t body ...)]))

(define-syntax (define-editor stx)
  (syntax-parse stx
    [(_ name:id super
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()])))
        ...
        body ...)
     #`(~define-editor #,stx name super (interfaces ...) body ...)]))

;; Mixin-editors are not at module level, and thus are not
;; implicetly provided by the ~define-editor helper macro.
(define-syntax (define-editor-mixin stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()]))
             (~optional (~seq #:mixins (mixins ...)) #:defaults ([(mixins 1) '()])))
        ...
        body ...)
     #:with (marked-body ...) (editor-syntax-introduce #'(body ...) 'add)
     #:with (marked-interfaces ...) (editor-syntax-introduce #'(interfaces ...))
     #:with (marked-mixins ...) (editor-syntax-introduce #'(mixins ...))
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(,(syntax-parameter-value #'current-editor-lang)
                                     racket/class
                                     racket/serialize
                                     ,(syntax-parameter-value #'current-editor-base)))
     #`(begin
         (begin-for-syntax
           (let ()
             (define b (continuation-mark-set-first #f editor-mixin-list-key))
             (when (and b (box? b))
               (set-box! b (cons #'name (unbox b))))))
         (editor-submod
          (provide name)
          (require marked-reqs ...)
          (#%require #,(quote-module-path))
          (define (name $)
            (~define-editor #,stx
                            name
                            ((compose #,@(reverse (attribute marked-mixins))) $)
                            (marked-interfaces ...)
                            #:direct-deserialize? #f
                            marked-body ...)
            name)))]))
