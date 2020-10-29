#lang racket/base

(provide (all-defined-out))
(require racket/splicing
         "editor.rkt"
         (for-syntax racket/base))

(splicing-syntax-parameterize ([current-editor-lang "../lang.rkt"]
                               [current-editor-base '(submod "../base.rkt" editor)]
                               [current-editor-modpath-mode 'package])
  (begin-for-interactive-syntax) ; <--- TODO...WHY!!!
  (require syntax/location
           "context.rkt"
           syntax/parse
           (prefix-in gui: racket/gui/base)
           racket/class
           racket/serialize
           racket/port
           images/icons/style
           images/icons/control
           (prefix-in gui: racket/gui/base)
           syntax/modread
           "stdlib.rkt"
           (for-editor "context.rkt"
                       racket/class
                       (prefix-in gui: racket/gui/base)
                       racket/async-channel))

  (define-interactive-syntax picker$ dialog$
    (inherit get-frame
             set-result!
             show)
    (super-new)
    (new label$ [parent this]
         [text "Select Interactive-Syntax Extension"])
    
    (new blank$ [parent this]
         [height 3])
    (define items-row (new horizontal-block$ [parent this]))
    (define label-col (new vertical-block$ [parent items-row]))
    (define field-col (new vertical-block$ [parent items-row]))
    (new blank$ [parent this]
         [height 3])
    (define confirm-row (new horizontal-block$ [parent this]))

    (new label$ [parent label-col]
         [text "Module:"])
    (new blank$ [parent label-col]
         [height 1])
    (new label$ [parent label-col]
         [text "Interactive Syntax:"])
    (new button$ [parent confirm-row]
         [label (new label$ [text "Cancel"])]
         [callback (λ (button event)
                     (show #f))])
    (define ok-space (new blank$ [parent confirm-row]
                          [width 5]))
    (define ok-button
      (new button$ [parent confirm-row]
           [label (new label$ [text "Insert"])]
           [callback (λ (b event)
                       (set-result!
                        (cons (send mod-name get-text)
                              (send editor-name get-text)))
                       (show #f))]))
      
    (define mod-name (new field$ [parent field-col]
                          [callback (λ (t e)
                                      (update-width!))]))
    (new blank$ [parent field-col]
         [height 1])
    (define editor-name (new field$ [parent field-col]
                             [callback (λ (t e)
                                         (update-width!))]))

    (define (update-width!)
      (define-values (mw mh) (send mod-name get-extent))
      (define-values (ew eh) (send editor-name get-extent))
      (define-values (ow oh) (send ok-button get-extent))
      (define width (max mw ew))
      (send ok-space set-width! (max 0 (+ width 40))))
    (update-width!))

  (begin-for-interactive-syntax
    (provide get-module)
    (define (get-module [parent #f])
      (define f (new gui:dialog% [parent parent]
                     [label "Interactive Syntax Selector"]))
      (define p (new picker$
                     [frame f]))
      (send p show #t)
      (send p get-result)))

  (define insert-button
    (list "Insert Editor"
          (record-icon #:color "red"
                       #:height (toolbar-icon-height))
          (λ (this)
            (define get-module (dynamic-require (from-editor (quote-module-path)) 'get-module))
            (define text (send this get-definitions-text))
            (define text-surrogate (send text get-surrogate))
            (define the-editor (get-module this))
            (writeln the-editor)
            (when (and the-editor (pair? the-editor))
              (define editor-class$
                (cond
                  [(equal? (car the-editor) "")
                   (define out (open-output-bytes))
                   (define mod-name (send text-surrogate get-mod-name))
                   (parameterize* ([current-namespace (send text-surrogate get-editor-namespace)]
                                   #;[current-namespace (module->namespace (from-editor `',mod-name))])
                     (namespace-require (from-editor mod-name))
                     (namespace-variable-value (with-input-from-string (cdr the-editor) read)))]
                  [else
                   (define directory (send (send this get-current-tab) get-directory))
                   (unless directory
                     (error 'editor "Could not determine the current dirrectory"))
                   (define read-path (with-input-from-string (car the-editor) read))
                   (define full-path
                     (if (or (not (path-string? read-path))
                             (and (path-string? read-path)
                                  (absolute-path? read-path)))
                         read-path
                         (build-path directory read-path)))
                   (with-handlers ([exn:fail? (λ (e)
                                                (error 'editor "Could not load ~a in ~a got ~s"
                                                       (cdr the-editor)
                                                       full-path
                                                       e))])
                     (parameterize* ([current-namespace (send text-surrogate get-editor-namespace)]
                                     #;[current-namespace (module->namespace (from-editor full-path))])
                       (namespace-require (from-editor full-path))
                       (namespace-variable-value
                        (with-input-from-string (cdr the-editor) read))))]))
              (send text insert (new editor-snip%
                                     [editor (new editor-class$)]
                                     [namespace (send text-surrogate get-editor-namespace)]
                                     [mod-name (send text-surrogate get-mod-name)]))))
          #f)))
