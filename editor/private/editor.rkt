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
         racket/stxparam
         racket/pretty
         (for-syntax racket/base
                     racket/dict
                     racket/class
                     (submod "lang.rkt" key-submod)
                     "log.rkt"
                     syntax/modresolve
                     racket/match
                     racket/serialize
                     racket/function
                     syntax/location
                     racket/syntax
                     racket/path
                     racket/pretty
                     syntax/parse))
(provide (all-defined-out)
         (all-from-out "lang.rkt")
         (for-syntax all-defined-out))

(define EDITOR-SERIALIZE-VERSION 0)

;; Keys for hidden methods. Only functions in
;;   this module should access these methods directly.
(define serial-key (generate-member-key))
(define deserial-key (generate-member-key))
(define deserial-binding-key (member-name-key deserialize-binding))
(define copy-key (generate-member-key))
(define elaborator-key (member-name-key elaborate))
(define modpath-key (member-name-key get-modpath))
(define equal?-key (generate-member-key))
(define hash-key (generate-member-key))
(define hash2-key (generate-member-key))

(define editor-deserialize-for-elaborator
  (make-parameter #f))

(define editor-deserialize-for-text
  (make-parameter #f))

(module m->r racket/base
  (provide (all-defined-out))
  (require racket/path
           racket/match
           syntax/parse/define
           syntax/modresolve
           syntax/location
           (for-syntax racket/base))
  (define (modpath->relpath modpath)
    (if (path-string? modpath)
        (path-only modpath)
        modpath))
  (define-syntax (this-mod-dir stx)
    (syntax/loc stx
      (resolve-module-path-index
       (module-path-index-join "here.rkt" #f)))))
(require 'm->r (for-syntax 'm->r))

;; Placed in a sub module because the define-interactive-syntax
;;   template uses these bindings in a begin-for-syntax
;;   block and the deserializer-submod block.
(module elaborator-keys racket/base
  (provide (all-defined-out))
  (require racket/class)
  (define elaborator-deserialize-key (generate-member-key))
  (define elaborator-copy-key (generate-member-key)))
(require (for-syntax 'elaborator-keys))


;; Runtime test for persistence. In case the syntax check did not work.
(define (runtime-persist? persist this [other #f])
  (cond
    [(procedure? persist) (persist this other)]
    [else persist]))

(define-syntax-parameter current-editor-modpath-mode 'user)
(begin-for-syntax
  (define (package/quote-module-path . submods)
    (define this-mod
      (case (syntax-parameter-value #'current-editor-modpath-mode)
        [(user) 'editor/private/editor]
        [(package) (quote-module-path)]))
    (if (null? submods)
        this-mod
        `(submod ,this-mod ,@submods))))

;; Finds the original source of an identifier. Assumes that
;;   the module chain has already been loaded.
;; ModulePath Symbol -> (Pair ModulePathIndex Symbol)
(define-for-syntax (identifier->original-identifier module sym [phase 0])
  (define-values (vals stxs)
    (module->exports module 'defined-names))
  (define binding
    (let* ([acc (dict-ref stxs phase)])
      (dict-ref acc sym)))
  (match binding
    [`(() ,orig-sym)
     (cons module orig-sym)]
    [`(((,mpi ,shift-phase ,orig-sym ,orig-phase) ,rest ...) ,eventual-sym)
     (identifier->original-identifier mpi orig-sym orig-phase)]
    [`((,mpi ,rest ...) ,orig-sym)
     (identifier->original-identifier mpi sym phase)]))

;; Because deserialized editors use a pseudo-identifier
;;   to resolve to an elaborator, we need to reconstruct a
;;   racket identifier out of their symbol and modpath.
;; Unless find-original? is set to #f, this function will use
;;   identifier->original-identifier to forge the defining identifier,
;;   and puts the given identifier in the nominal source fields.
;; NOTE THIS DOES NOT ACTUALLY REQUIRE THE MODPATH, USE
;;   syntax-local-lift-require FOR THAT!!!!  (Rather, this uses
;;   whatever module exists in the registry under that name.)
;; resolved-module-path? symbol? boolean? -> identifier?
(define-for-syntax (forge-identifier modpath sym [find-original? #t])
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-load-relative-directory (build-path (this-mod-dir) "..")])
    (when find-original?
      (namespace-require modpath))
    (define real-id (if find-original?
                        (identifier->original-identifier modpath sym)
                        (cons #f #f)))
    (define real-modpath (or (car real-id) modpath))
    (define real-sym (or (cdr real-id) sym))
    (let* ([acc (syntax-binding-set)]
           [acc (syntax-binding-set-extend acc sym 0 real-modpath
                                           #:source-symbol real-sym
                                           #:nominal-symbol sym
                                           #:nominal-module modpath)])
      (syntax-binding-set->syntax acc sym))))

;; Only introduced by #editor reader macro. Handles deserializing
;;    the editor.
;; It shouldn't need to be a stuct, because the logic _should_ be part
;;    of the reader proper.
(begin-for-syntax
  (define read-#%editor
    (syntax-parser
      [(_ binding-information body)
       (parameterize ([current-load-relative-directory (this-mod-dir)])
         (match-define `((,editor-binding ,editor-name)
                         (,deserialize-binding ,deserialize-name)
                         (,elaborator-binding ,elaborator-name))
           (deserialize (syntax->datum #'binding-information)))
         (define same-mod? (equal? (resolve-module-path-index elaborator-binding)
                                   (this-mod-dir)))
         ;; If the editor definition and use are from the same file,
         ;;  then the current module is not yet named.
         ;; So eval variable-reference to get it.
         (define/syntax-parse elaborator
           (if same-mod?
               (forge-identifier
                (variable-reference->module-path-index (eval #'(#%variable-reference)))
                elaborator-name
                #f)
               (forge-identifier elaborator-binding elaborator-name)))
         (define/syntax-parse that-syntax (syntax-local-introduce #`#,this-syntax))
         ;(pretty-write (identifier-binding #'elaborator))
         ;(pretty-write (syntax-debug-info #'elaborator))
         #'(elaborator body that-syntax))]))
  (struct #%editor-struct ()
    #:property prop:procedure
    (λ (this stx) (read-#%editor stx))
    #:property prop:match-expander
    read-#%editor))
(define-syntax #%editor (#%editor-struct))

;; Returns an identifier that contains
;;   the binding for an editor, elaborator, and deserializer.
;; To be put into the #editor()() form.
;; (is-a?/c editor$) -> (listof (list module-path-index? symbol?))
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

;; Macros used by state variables (getter, setter, init)
(module state-macros racket/base
  (provide (all-defined-out))
  (require syntax/parse/define
           racket/class
           (for-syntax racket/base))
  (define-syntax-parser define-init
    [(_ name:id default #f)
     (syntax/loc this-syntax
       (define name default))]
    [(_ name:id default #t)
     (syntax/loc this-syntax
       (define-init name default (λ (x) x)))]
    [(_ name:id default init-proc)
     (quasisyntax/loc this-syntax
       (begin
         (init [(ist name) default])
         #,(syntax/loc this-syntax
             (define name (init-proc ist)))))])

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
     (syntax/loc this-syntax (define/public setter body))]))
(require 'state-macros (for-syntax 'state-macros))

(begin-for-syntax
  (define-syntax-class defelaborate
    #:attributes (type
                  data
                  this-editor
                  [body 1]
                  struct)
    #:literals (define-elaborator)
    (pattern (define-elaborator data
               (~optional (~seq #:this-editor this-editor))
               body ...+)
             #:attr struct #f
             #:attr type 'simple)
    (pattern (define-elaborator
               struct)
             #:attr data #f
             #:attr this-editor #f
             #:attr (body 1) '()
             #:attr type 'struct))
  (define-splicing-syntax-class defstate-options
    (pattern (~seq
              (~alt (~optional (~seq #:persistence persistence) #:defaults ([persistence #'#t]))
                    (~optional (~seq #:getter getter) #:defaults ([getter #'#f]))
                    (~optional (~seq #:setter setter) #:defaults ([setter #'#f]))
                    (~optional (~seq #:serialize serialize) #:defaults ([serialize #'#f]))
                    (~optional (~seq #:deserialize deserialize) #:defaults ([deserialize #'#f]))
                    (~optional (~seq #:init init) #:defaults ([init #'#f]))
                    (~optional (~seq #:elaborator elaborator) #:defaults ([elaborator #'#f]))
                    (~optional (~seq #:elaborator-default elaborator-default)
                               #:defaults ([elaborator-default #'#f]))
                    (~once default))
              ...)))
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state marked-name:id
               options:defstate-options)
             #:attr name (editor-syntax-introduce (attribute marked-name))
             #:attr getter-name (format-id this-syntax "get-~a" #'name)
             #:attr setter-name (format-id this-syntax "set-~a!" #'name)
             #:attr default #'options.default
             #:attr init #'options.init
             #:attr persistence #'options.persistence
             #:attr getter #'options.getter
             #:attr setter #'options.setter
             #:attr serialize #'options.serialize
             #:attr deserialize #'options.deserialize
             #:attr elaborator #'options.elaborator
             #:attr elaborator-default #'options.elaborator-default)))

(define-syntax-parameter define-elaborator
  (syntax-parser
    [de:defelaborate
     (raise-syntax-error 'define-elaborator "Use outside of define-interactive-syntax is an error" this-syntax)]))

(define-syntax-parameter define-state
  (syntax-parser
    [x:defstate
     (raise-syntax-error 'define-state "Use outside of define-interactive-syntax is an error" this-syntax)]))

;; We don't want to get editor classes when
;; deserializing new editors.
(define deserialize-editor-classes?
  (make-parameter #t))

;; Each editor definition has three parts:
;; 1. A phase 1 elaboration
;; 2. A submodule with interaction code
;; 3. A deserializer submodule
(define-syntax (~define-interactive-syntax stx)
  (syntax-parse stx
    [(_ orig-stx name:id supclass (interfaces ...)
        (~alt (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
              (~optional (~seq #:mixin mixin)))
        ...
        (~and
         (~seq (~alt plain-state:defstate
                     (~optional elaborator:defelaborate
                                #:defaults ([elaborator.type 'simple]
                                            [elaborator.data #'this-data]
                                            [elaborator.this-editor #'#f]
                                            [(elaborator.body 1)
                                             (list #'#'#f)])) ; ???  #'#'this-editor instead ???
                     internal-body) ...)
         (~seq body ...)))
     #:with marked-name (editor/user-syntax-introduce #'name)
     #:with marked-supclass (editor/user-syntax-introduce #'supclass)
     #:with elaborator-name (format-id #'orig-stx "~a:elaborate" #'name)
     #:with name-deserialize (format-id #'orig-stx "~a:deserialize" #'name)
     #:with (marked-interfaces ...) (editor/user-syntax-introduce #'(interfaces ...))
     #:with (marked-body ...) (editor/user-syntax-introduce #'(body ...) 'add)
     #:with (state:defstate ...) (editor/user-syntax-introduce #'(plain-state ...))
     #:with serialize-method (gensym 'serialize)
     #:with deserialize-method (gensym 'deserialize)
     #:with deserialize-binding-method (gensym 'deserialize-binding)
     #:with copy-method (gensym 'copy)
     #:with elaborator-method (gensym 'elaborator)
     #:with modpath-method (gensym 'get-modpath)
     #:with elaborator-deserialize-method (gensym 'deserialize)
     #:with elaborator-copy-method (gensym 'copy)
     #:with equal?-method (gensym 'equal?)
     #:with hash-method (gensym 'hash/)
     #:with hash2-method (gensym 'hash2/)
     #:with (state-methods ...) (for/list ([i (in-list (attribute state.getter-name))])
                                  (gensym (syntax->datum i)))
     (unless (or (eq? 'module-begin (syntax-local-context)) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define m? (and (attribute mixin) #t))
     (define base? (syntax-e (attribute b?)))
     (define/syntax-parse public/override
       (if base? #'public #'override))
     (define (deserialize-proc rec for-editor?)
       #`(λ (data)
           (define sup (vector-ref data 0))
           (define key (vector-ref data 1))
           (define table (vector-ref data 2))
           #,(if base?
                 #`(void)
                 #`(super #,rec (if (eq? 'name key)
                                    sup
                                    data)))
           (unless (eq? key 'name)
             (log-editor-warning "Missing data for key ~a, trying super"
                                 'name))
           (when (eq? key 'name)
             (void)
            #,@(for/list ([i (in-list (attribute state.marked-name))]
                           [p? (in-list (attribute state.persistence))]
                           [s? (in-list (attribute state.setter))]
                           [d? (in-list (attribute state.deserialize))]
                           [e? (in-list (attribute state.elaborator))])
                  (define key (syntax->datum i))
                  #`(when (hash-has-key? table '#,key)
                      (define des-proc #,(if for-editor? d? #f))
                      (define maybe-other-val (hash-ref table '#,key))
                      (define other-val (if des-proc
                                            (des-proc maybe-other-val)
                                            maybe-other-val))
                      #,(if for-editor?
                            #`(let ([p* #,p?])
                                (case p*
                                  [(#t) (set! #,i other-val)]
                                  [(#f) (void)]
                                  [else (set! #,i (p* #,i other-val))]))
                            #`(set! #,i other-val)))))))
     (define (copy-proc rec)
       #`(λ (other)
           #,(if base?
                 #`(void)
                 #`(super #,rec other))
           (set! state.marked-name (send other state-methods)) ...))
     (define (equal?-proc rec)
       #`(λ (other equal?/rec)
           (and #,(if base?
                      #'#t
                      #`(super #,rec other equal?/rec))
                (let ()
                  (define left (state-methods))
                  (define right (send other state-methods))
                  (or (not (runtime-persist? state.persistence left other))
                      (equal?/rec left right))) ...)))
     (define (hash-proc rec)
       #`(λ (hash/rec)
           (+ #,(if base?
                    #'0
                    #`(super #,rec hash/rec))
              (let ()
                (define left (state-methods))
                (if (runtime-persist? state.persistence left)
                    (hash/rec left)
                    0)) ...)))
     (define (hash2-proc rec)
       #`(λ (hash2/rec)
           (* #,(if base?
                    #'1
                    #`(super #,rec hash2/rec))
              (let ()
                (define left (state-methods))
                (if (runtime-persist? state.persistence left)
                    (hash2/rec left)
                    1)) ...)))
     #`(begin
         #,@(if m?
                (list)
                (list #'(provide elaborator-name)
                      #'(begin-for-syntax
                          (let ()
                            (define b (continuation-mark-set-first #f editor-list-key))
                            (when (and b (box? b))
                              (set-box! b (cons #'name (unbox b))))))))
         ;; Submodule for deserialization, used by both editor submodule
         ;;   and begin-for-syntax elaborator.
         (deserializer-submod
          (provide name-deserialize)
          (#%require racket/class
                     #,(package/quote-module-path)
                     #,(package/quote-module-path 'elaborator-keys))
          (define-member-name serialize-method serial-key)
          (define-member-name deserialize-method deserial-key)
          (define-member-name deserialize-binding-method deserial-binding-key)
          (define-member-name copy-method copy-key)
          (define-member-name elaborator-method elaborator-key)
          (define-member-name modpath-method modpath-key)
          (define-member-name elaborator-deserialize-method elaborator-deserialize-key)
          (define-member-name elaborator-copy-method elaborator-copy-key)
          (define (get-name)
            (cond [(editor-deserialize-for-elaborator)
                   (namespace-require `(for-template ,(quote-module-path "..")))
                   (namespace-variable-value 'name)]
                  [else 
                   (dynamic-require (quote-module-path ".." editor)
                                    'name)]))
          (define name-deserialize
            (make-deserialize-info
             (λ (version args)
               (cond [(editor-deserialize-for-text)
                      (vector version args)]
                     [else
                      (define this (new (get-name)))
                      (cond [(editor-deserialize-for-elaborator)
                             (send this elaborator-deserialize-method args)]
                            [else
                             (send this deserialize-method args)])
                      this]))
             (λ ()
               (cond [(editor-deserialize-for-text)
                      (define vec (make-vector #f))
                      (values vec
                              (λ (other)
                                (vector-set! vec 0 other)))]
                     [else
                      (define pattern (new (get-name)))
                      (values pattern
                              (λ (other)
                                (cond [(editor-deserialize-for-elaborator)
                                       (send pattern elaborator-copy-method other)]
                                      [else
                                       (send pattern copy-method other)])))])))))
         ;; Main editor class
         (editor-submod
          (provide name)
          (#%require #,(package/quote-module-path))
          (define this-modpath
            (variable-reference->module-path-index (#%variable-reference)))
          (define-runtime-module-path-index this-filepath ".")
          (define-member-name serialize-method serial-key)
          (define-member-name deserialize-method deserial-key)
          (define-member-name deserialize-binding-method deserial-binding-key)
          (define-member-name copy-method copy-key)
          (define-member-name elaborator-method elaborator-key)
          (define-member-name modpath-method modpath-key)
          (define-member-name equal?-method equal?-key)
          (define-member-name hash-method hash-key)
          (define-member-name hash2-method hash2-key)
          (splicing-syntax-parameterize
              ([define-state
                 (syntax-parser
                   [st:defstate
                    #`(begin
                        #,(syntax/loc #'st
                            (define-init st.marked-name st.default st.init))
                        #,(syntax/loc #'st
                            (define-getter st.marked-name st.getter-name st.getter))
                        #,(syntax/loc #'st
                            (define-setter st.marked-name st.setter-name st.setter)))])]
               [define-elaborator
                 (syntax-parser
                   [de:defelaborate
                    #'(begin)])])
            (define deserialize-binding
              (make-parameter
               (cons 'name-deserialize
                     (module-path-index-join '(submod ".." deserializer) this-modpath))))
            (define-syntax marked-name (make-rename-transformer #'name))
            (define #,(if m? #'(name mixin) #'name)
              (let ()
                (define-local-member-name state-methods) ...
                (class/derived
                 orig-stx
                 (name
                  #,(if m? #'(supclass mixin) #'marked-supclass)
                  ((interface* () ([prop:serializable
                                    (make-serialize-info
                                     (λ (this)
                                       (define ret
                                         (vector
                                          EDITOR-SERIALIZE-VERSION
                                          (send this serialize-method)))
                                       ret)
                                     deserialize-binding
                                     #t
                                     (or (current-load-relative-directory) (current-directory)))]
                                   [prop:equal+hash
                                    (list (λ (this other rec)
                                            (send this equal?-method other rec))
                                          (λ (this rec)
                                            (send this hash-method rec))
                                          (λ (this rec)
                                            (send this hash2-method rec)))]))
                   marked-interfaces ...)
                  #f)
                 (define (elaborator-method)
                   #,(if m?
                         #'(list this-modpath #f)
                         #'(list (module-path-index-join '(submod "..") this-modpath)
                                 'elaborator-name)))
                 (public/override elaborator-method)
                 (define (serialize-method)
                   (vector #,(if base?
                                 #'#f
                                 #`(super serialize-method))
                           'name
                           (let ()
                             (define state-vars
                               `((state.marked-name
                                  ,state.marked-name
                                  ,state.persistence
                                  ,state.serialize)
                                 ...))
                             (for/hash ([var (in-list state-vars)]
                                        #:when (third var))
                               (define val (second var))
                               (define serial-proc (fourth var))
                               (values (first var) (if serial-proc
                                                       (serial-proc val)
                                                       val))))))
                 (public/override serialize-method)
                 (define deserialize-method
                   #,(deserialize-proc #'deserialize-method #t))
                 (public/override deserialize-method)
                 (define copy-method
                   #,(copy-proc #'copy-method))
                 (public/override copy-method)
                 (define (deserialize-binding-method)
                   deserialize-binding)
                 (public/override deserialize-binding-method)
                 (define (modpath-method)
                   (list this-modpath 'name))
                 (public/override modpath-method)
                 (define equal?-method
                   #,(equal?-proc #'equal?-method))
                 (public/override equal?-method)
                 (define hash-method
                   #,(hash-proc #'hash-method))
                 (public/override hash-method)
                 (define hash2-method
                   #,(hash2-proc #'hash2-method))
                 (public/override hash2-method)
                 (define/public (state-methods) state.marked-name) ...
                 marked-body ...)))))
         ;; Special class used by elaborator for deserialization.
         ;;   Because can't init racket/gui/base twice in one process...
         (begin-for-syntax
           (provide name)
           (define-member-name elaborator-deserialize-method elaborator-deserialize-key)
           (define-member-name elaborator-copy-method elaborator-copy-key)
           (define #,(if m? #'(name $) #'name)
             (let ()
               (define-local-member-name state-methods) ...
               (class #,(cond [base? #'object%]
                              [m? #'(supclass $)]
                              [else #'marked-supclass])
                 (super-new)
                 (define elaborator-deserialize-method
                   #,(deserialize-proc #'elaborator-deserialize-method #f))
                 (public/override elaborator-deserialize-method)
                 (define elaborator-copy-method
                   #,(copy-proc #'elaborator-copy-method))
                 (public/override elaborator-copy-method)
                 (define/public (state-methods) state.marked-name) ...
                 (define state.marked-name state.elaborator-default) ...
                 (define-getter state.marked-name state.getter-name state.elaborator) ...))))
         ;; Elaborator must be split into two parts to bind the
         ;;   elaborator.this-editor in the template.
         (define-syntax-parser elaborator-inside
           [(_ data-id:id data orig)
            (syntax-parse #'orig
              [_
               #:do [(define elaborator.data
                       (parameterize ([(dynamic-require
                                        '#,(package/quote-module-path)
                                        'editor-deserialize-for-elaborator) #t]
                                      [current-load-relative-directory (this-mod-dir)])
                         (deserialize (syntax->datum #'data))))
                     (define/syntax-parse
                       #,(if (syntax->datum #'elaborator.this-editor)
                             #'elaborator.this-editor
                             #'ignored-binding)
                       #'data-id)]
               elaborator.body ...])])
         (define-syntax elaborator-name
           #,(case (attribute elaborator.type)
               [(struct) #'elaborator.struct]
               [(simple)
                #`(elaborator-transformer #'elaborator-inside elaborator.this-editor)])))]))

(define-for-syntax (elaborator-transformer inside use-elaborator-this?)
  (syntax-parser
    [(_ data orig)
     #:with elaborator-inside inside
     (if use-elaborator-this?
         #`(splicing-let ([data-id
                           (parameterize ([current-load-relative-directory (this-mod-dir)])
                             (deserialize 'data))])
             (elaborator-inside data-id data orig))
         #'(elaborator-inside data-id data orig))]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-interactive-syntax #,stx name super (interfaces ...) #:base? #t body ...)]))

(define-syntax (define-interactive-syntax stx)
  (syntax-parse stx
    [(_ name:id super
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()])))
        ...
        body ...)
     #`(~define-interactive-syntax #,stx name super (interfaces ...) body ...)]))

;; Mixin-editors are not at module level, and thus are not
;; implicetly provided by the ~define-interactive-syntax helper macro.
(define-syntax (define-interactive-syntax-mixin stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()]))
             (~optional (~seq #:mixins (mixins ...)) #:defaults ([(mixins 1) '()]))
             (~optional (~seq #:super $) #:defaults ([$ #'super])))
        ...
        body ...)
     #:with (marked-mixins ...) (editor/user-syntax-introduce #'(mixins ...) 'add)
     #`(begin
         (begin-for-syntax
           (let ()
             (define b (continuation-mark-set-first #f editor-mixin-list-key))
             (when (and b (box? b))
               (set-box! b (cons #'name (unbox b))))))
         (~define-interactive-syntax #,stx
                         name
                         (compose #,@(reverse (attribute marked-mixins)))
                         (interfaces ...)
                         #:mixin $
                         body ...))]))
