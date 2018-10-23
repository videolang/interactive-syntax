#lang racket/base

(require "lang.rkt"
         racket/list
         racket/stxparam
         racket/class
         racket/splicing
         syntax/location
         racket/serialize
         racket/dict
         syntax/parse/define
         racket/runtime-path
         racket/match
         (for-syntax racket/base
                     (submod "lang.rkt" key-submod)
                     racket/match
                     racket/serialize
                     racket/function
                     syntax/location
                     racket/syntax
                     racket/path
                     syntax/parse))
(provide (all-defined-out)
         (all-from-out "lang.rkt"))

;; Keys for hidden methods. Only functions in
;;   this module should access these methods directly.
(define serial-key (generate-member-key))
(define deserial-key (generate-member-key))
(define deserial-binding-key (member-name-key deserialize-binding))
(define copy-key (generate-member-key))
(define elaborator-key (member-name-key elaborate))
(define modpath-key (member-name-key get-modpath))

(module m->r racket/base
  (provide (all-defined-out))
  (require racket/path
           racket/match
           syntax/parse/define
           syntax/modresolve
           (for-syntax racket/base))
  (define-syntax (this-mod-dir stx)
    #'(modpath->relpath (resolve-module-path-index
                         (module-path-index-join "here.rkt" #f))))
  (define (modpath->relpath modpath)
    (if (path-string? modpath)
        (path-only modpath)
        modpath)))
(require 'm->r (for-syntax 'm->r))

;; Because deserialized editors use a pseudo-identifier
;;   to resolve to an elaborator, we need to reconstruct a
;;   racket identifier out of their symbol and modpath.
;; NOTE THIS DOES NOT ACTUALLY REQUIRE THE MODPATH, USE
;;   syntax-local-lift-require FOR THAT!!!!  (Rather, this uses
;;   whatever module exists in the registry under that name.)
;; resolved-module-path? symbol? -> identifier?
(define-for-syntax (forge-identifier modpath sym)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns]
                 [current-module-declare-name modpath])
    (eval #`(module dummy racket/base
              (define #,sym 'dummy)))
    (namespace-syntax-introduce (datum->syntax #f sym)
                                (module->namespace modpath))))

;; Only introduced by #editor reader macro. Handles deserializing
;;  the editor.
(define-syntax-parser #%editor
  [(_ binding-information body)
   #'(splicing-let-syntax
         ([this (λ (stx)
                  (parameterize ([current-load-relative-directory (this-mod-dir)])
                    (match-define `((,editor-binding ,editor-name)
                                    (,deserialize-binding ,deserialize-name)
                                    (,elaborator-binding ,elaborator-name))
                      (deserialize 'binding-information))
                    (define/syntax-parse elaborator
                      (forge-identifier
                       ;syntax-local-lift-require
                       (module-path-index-resolve elaborator-binding)
                       elaborator-name))
                    #'(elaborator body)))])
       (this))])

;; Returns an identifier that contains
;;   the binding for an editor's elaborator.
;; To be put into the #editor()() form.
;; (is-a?/c editor$) -> (listof module-path-index? symbol?)
(define (editor->elaborator editor)
  (define-member-name elaborator elaborator-key)
  (define-member-name deserialize-binding deserial-binding-key)
  (define-member-name modpath modpath-key)
  (define binding ((send editor deserialize-binding)))
  (list (send editor modpath)
        (if (pair? binding)
            (list (cdr binding) (car binding))
            (list #"???" #"???"))
        (send editor elaborator)))

;; Deserializes an editor, but giving a new modpath for its
;;   deserialize id.
;; An optional dictionary of children can be provided to rehome
;;   child elements in the editor as well.
;; editor : An editor instance
;; new-modpath : Any value given to prop:serialize's deserialize-id
;; children : (Dictof <Editor> <Modpath>)
(define (serialize+rehome editor new-modpath
                          #:deserialize-relative-directory [rel-to #f]
                          #:children [children* #f])
  (define-member-name deserial-binding deserial-binding-key)
  (define children (or children* (hash)))
  (let loop ([child# (dict-iterate-first children)])
    (if child#
        (parameterize ([(send (dict-iterate-key children child#) deserial-binding)
                        (dict-iterate-value children child#)])
          (loop (dict-iterate-next children child#)))
        (parameterize ([(send editor deserial-binding) new-modpath])
          (serialize editor #:deserialize-relative-directory rel-to)))))

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
     #:with elaborator-name (format-id #'orig-stx "~a:elaborate" #'name)
     #:with name-deserialize (format-id #'orig-stx "~a:deserialize" #'name)
     #:with (marked-interfaces ...) (editor/user-syntax-introduce #'(interfaces ...))
     #:with (marked-body ...) (editor/user-syntax-introduce #'(body ...) 'add)
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(,(syntax-parameter-value #'current-editor-lang)
                                     racket/class
                                     racket/serialize
                                     ,(syntax-parameter-value #'current-editor-base)))
     #:with marked-supclass (editor/user-syntax-introduce #'supclass)
     #:with (state:defstate ...) (editor/user-syntax-introduce #'(plain-state ...))
     #:with serialize-method (gensym 'serialize)
     #:with deserialize-method (gensym 'deserialize)
     #:with deserialize-binding-method (gensym 'deserialize-binding)
     #:with copy-method (gensym 'copy)
     #:with elaborator-method (gensym 'elaborator)
     #:with modpath-method (gensym 'get-modpath)
     #:with (state-methods ...) (for/list ([i (in-list (attribute state.getter-name))])
                                  (gensym (syntax->datum i)))
     (define dd?* (syntax-e #'dd?))
     (unless (or (not dd?*)
                 (eq? 'module-begin (syntax-local-context)) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define base? (syntax-e (attribute b?)))
     (define/syntax-parse public/override
       (if base? #'public #'override))
     #`(begin
         #,@(if dd?*
                (list #'(provide elaborator-name)
                      #'(begin-for-syntax
                          (let ()
                            (define b (continuation-mark-set-first #f editor-list-key))
                            (when (and b (box? b))
                              (set-box! b (cons #'name (unbox b)))))))
                '())
         (define-syntax-parser elaborator-name
           [(_ data)
            #`(let ()
                (define elaborator.data
                  (parameterize ([current-load-relative-directory (this-mod-dir)])
                    (deserialize 'data)))
                elaborator.body ...)])
         (#,@(if dd?*
                 #`(editor-submod
                    (require marked-reqs ...)
                    (#%require #,(quote-module-path))
                    (define this-modpath
                      (variable-reference->module-path-index (#%variable-reference)))
                    (define-runtime-module-path-index this-filepath "."))
                 #'(begin
                     (define this-modpath #f)))
          (define-member-name serialize-method serial-key)
          (define-member-name deserialize-method deserial-key)
          (define-member-name deserialize-binding-method deserial-binding-key)
          (define-member-name copy-method copy-key)
          (define-member-name elaborator-method elaborator-key)
          (define-member-name modpath-method modpath-key)
          #,@(if dd?*
                 (list
                  #'(provide name)
                  #`(module+ deserialize
                     (provide name-deserialize)
                     (define name-deserialize
                       (make-deserialize-info
                        (λ (sup table)
                          (define this (new name))
                          (send this deserialize-method (vector sup table))
                          (send this on-state-changed)
                          this)
                        (λ ()
                          (define pattern (new name))
                          (values pattern
                                  (λ (other)
                                    (send pattern copy-method other)
                                    (send pattern on-state-changed))))))))
                  '())
          (splicing-syntax-parameterize
              ([define-state
                 (syntax-parser
                   [st:defstate
                    #`(begin
                        #,(syntax/loc #'st
                            (define st.marked-name st.default))
                        #,(syntax/loc #'st
                            (define-getter st.marked-name st.getter-name st.getter))
                        #,(syntax/loc #'st
                            (define-setter st.marked-name st.setter-name st.setter)))])]
               [define-elaborate
                 (syntax-parser
                   [de:defelaborate
                    #'(begin)])])
            (define deserialize-binding
              (make-parameter (cons 'name-deserialize
                                    (module-path-index-join '(submod "." deserialize) this-modpath))))
            (define name
              (let ()
                (define-local-member-name state-methods) ...
                (class/derived
                 orig-stx
                 (name
                  marked-supclass
                  ((interface* () ([prop:serializable
                                    (make-serialize-info
                                     (λ (this)
                                       (send this serialize-method))
                                     deserialize-binding
                                     #t
                                     (or (current-load-relative-directory) (current-directory)))]))
                   marked-interfaces ...)
                  #f)
                 (define (elaborator-method)
                   #,(if dd?*
                         #'(list (module-path-index-join '(submod "..") this-modpath)
                                 'elaborator-name)
                         #'(list this-modpath #f)))
                 (public/override elaborator-method)
                 (define (serialize-method)
                   (vector #,(if base?
                                 #'#f
                                 #`(super serialize-method))
                           (let ()
                             (define state-vars
                               `((state.marked-name
                                  ,state.marked-name
                                  ,state.persistence)
                                 ...))
                             (for/hash ([var (in-list state-vars)]
                                        #:when (third var))
                               (values (first var) (second var))))))
                 (public/override serialize-method)
                 (define (deserialize-method data)
                   (define sup (vector-ref data 0))
                   (define table (vector-ref data 1))
                   #,(if base?
                         #`(void)
                         #`(super deserialize-method sup))
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
                 (public/override deserialize-method)
                 (define (copy-method other)
                   #,(if base?
                         #`(void)
                         #`(super copy-method other))
                   (set! state.marked-name (send other state-methods)) ...)
                 (public/override copy-method)
                 (define (deserialize-binding-method)
                   deserialize-binding)
                 (public/override deserialize-binding-method)
                 (define (modpath-method)
                   (list this-modpath 'name))
                 (public/override modpath-method)
                 (define/public (state-methods) state.marked-name) ...
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
     #:with (marked-body ...) (editor/user-syntax-introduce #'(body ...) 'add)
     #:with (marked-interfaces ...) (editor/user-syntax-introduce #'(interfaces ...))
     #:with (marked-mixins ...) (editor/user-syntax-introduce #'(mixins ...))
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
