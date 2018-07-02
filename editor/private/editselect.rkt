#lang racket/base

(provide (all-defined-out))
(require racket/splicing
         "editor.rkt"
         (for-syntax racket/base))

(splicing-syntax-parameterize ([current-editor-lang "../lang.rkt"]
                               [current-editor-base '(submod "../base.rkt" editor)])
  (begin-for-editor) ; <--- TODO...WHY!!!
  (require syntax/location
           "context.rkt"
           racket/class
           racket/serialize
           racket/port
           images/icons/style
           images/icons/control
           (for-editor "context.rkt"
                       (prefix-in gui: racket/gui/base)
                       racket/async-channel))

  (define-editor picker$ window$
    (super-new)
    (new label$ [parent this]
         [text "Select Editor"])

    (init-field result-box
                frame)

    (define mod-row (new horizontal-block$ [parent this]))
    (define editor-row (new horizontal-block$ [parent this]))
    (define confirm-row (new horizontal-block$ [parent this]))

    (new label$ [parent mod-row]
         [text "Module:"])
    (new label$ [parent editor-row]
         [text "Editor:"])
    (new button$ [parent confirm-row]
         [label (new label$ [text "Cancel"])]
         [callback (λ (button event)
                     (send this show #f)
                     (send frame show #f))])
    (new button$ [parent confirm-row]
         [label (new label$ [text "OK"])]
         [callback (λ (b event)
                     (set-box! result-box
                               (cons (send mod-name get-text)
                                     (send editor-name get-text)))
                     (send this show #f)
                     (send frame show #f))])

    (define mod-name (new field$ [parent mod-row]))
    (define editor-name (new field$ [parent editor-row])))

  (begin-for-editor
    (provide get-module)
    (define (get-module [parent #f])
      (define res (box #f))
      (define f (new gui:dialog% [parent parent]
                     [label "Editor Selector"]))
      (define p (new picker$
                     [result-box res]
                     [frame f]))
      (new editor-canvas% [parent f]
           [editor p])
      (send f show #t)
      (unbox res)))

  (define insert-button
    (list "Insert Editor"
          (record-icon #:color "red"
                       #:height (toolbar-icon-height))
          (λ (this)
            (define get-module (dynamic-require (from-editor (quote-module-path)) 'get-module))
            (define text (send this get-definitions-text))
            (define the-editor (get-module this))
            (define directory (send (send this get-current-tab) get-directory))
            (unless directory
              (error 'editor "Could not determine the current dirrectory"))
            (when (and the-editor (pair? the-editor))
              (define read-path (with-input-from-string (car the-editor) read))
              (define full-path
                (if (absolute-path? read-path)
                    read-path
                    (build-path directory read-path)))
              (with-handlers ([exn:fail? (λ (e)
                                           (error 'editor "Could not load ~a in ~a got ~s"
                                                  (cdr the-editor)
                                                  full-path
                                                  e))])
                (define editor-class$
                  (dynamic-require (from-editor full-path)
                                   (with-input-from-string (cdr the-editor) read)))
                (send text insert (new editor-snip%
                                       [editor (new editor-class$)])))))
          #f)))
