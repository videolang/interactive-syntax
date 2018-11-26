#lang racket/base

(provide (all-defined-out))
(require racket/class
         (prefix-in gui: racket/gui/base)
         racket/unit
         racket/runtime-path
         racket/list
         racket/fasl
         syntax/modread
         drracket/tool
         framework
         (only-in racket/gui/base
                  open-input-text-editor
                  make-gui-empty-namespace)
         images/icons/style
         images/icons/control
         syntax-color/module-lexer
         syntax/modresolve
         racket/match
         racket/set
         racket/port
         racket/serialize
         syntax/parse
         "../lang.rkt"
         "context.rkt"
         "editor.rkt"
         "stdlib.rkt"
         "read-editor.rkt")

(define-namespace-anchor anchor)

(define (make-editor-namespace)
  (define ns (make-gui-empty-namespace))
  (namespace-attach-module (namespace-anchor->empty-namespace anchor)
                           'editor/lang
                           ns)
  (namespace-require 'racket/base ns)
  (namespace-require 'editor/base ns)
  (namespace-require 'editor/lang ns)
  (namespace-require 'racket/serialize ns)
  ns)

(define surrogate%
  (class* racket:text-mode% (racket:text-mode<%>)
    (super-new)
    ;; Need the text object when enabled
    (define text #f)
    (define/override (on-enable-surrogate t)
      (super on-enable-surrogate t)
      (set! text t)
      (reset-editor-namespace))
    ;; Ensure all editors in a buffer use the same namespace
    (define editor-namespace (make-editor-namespace))
    (define stored-mod-stx #f)
    (define stored-mod-name #f)
    (define/public (get-mod-name)
      stored-mod-name)
    (define/public (get-editor-namespace)
      editor-namespace)
    (define/private (maybe-get-filename)
      (define tmp (box #f))
      (define filename (send text get-filename tmp))
      (and (not (unbox tmp))
           filename
           (let* ([_ (if (string? filename)
                         (string->path filename)
                         filename)]
                  [_ (resolve-module-path _)])
             _)))
    (define/public (reset-editor-namespace)
      (define maybe-filename (maybe-get-filename))
      (parameterize ([editor-read-as-snip? #t])
        (define new-ns (make-editor-namespace))
        (parameterize ([current-namespace new-ns])
          (with-handlers ([exn:fail?
                           (λ (e)
                             (log-warning "~a" e)
                             (define frame (send (send text get-tab) get-frame))
                             (send frame show-editor-error-panel (exn-message e)))])
            (define-values (mod-stx mod-name)
              (let ([stx (try-read-editor)])
                (values stx
                      (or maybe-filename
                          (list 'quote
                                (syntax-parse stx
                                  [(mod name lang body ...)
                                   (syntax->datum #'name)]))))))
            (parameterize ([current-module-declare-name
                            (and maybe-filename (make-resolved-module-path maybe-filename))])
              (eval mod-stx))
            (namespace-require/expansion-time mod-name)
            (namespace-require (from-editor mod-name))
            (namespace-require `(submod ,mod-name editor deserialize))
            (set! stored-mod-name mod-name)
            (set! stored-mod-stx mod-stx)
            (set! editor-namespace new-ns)))))
    (define/public (try-read-editor)
      (parameterize ([editor-read-as-snip? #t])
        (define out (open-output-bytes))
        (define mod-text (send text save-port out 'standard))
        (with-input-from-bytes (get-output-bytes out)
          (λ ()
            (with-module-reading-parameterization
              read-syntax)))))
    ;; Ensure that #editor()() format is used
    (define prev-format #f)
    (define/override (after-save-file orig inner success?)
      (when prev-format
        (send orig set-file-format prev-format)))
    (define/override (on-save-file orig inner filename format)
      (set! prev-format (send orig get-file-format))
      (send orig set-file-format 'text)
      (inner filename 'text))))

(define editor-icon
  (fast-forward-icon #:color "green"
                     #:height (toolbar-icon-height)))

(define (update-editors! text editors)
  (parameterize ([editor-read-as-snip? #t])
    (define text-surrogate (send text get-surrogate))
    (send text-surrogate reset-editor-namespace)
    (define editor-namespace (send text-surrogate get-editor-namespace))
    (define editor-mod-name (send text-surrogate get-mod-name))
    ;; First, update all editor-snips already in use.
    (let loop ([last-editor #f])
      (define current-editor (send text find-next-non-string-snip last-editor))
      (cond
        [(not current-editor) (void)]
        [else
         (when (is-a? current-editor editor-snip%)
           (define-values (binding is-same-file des-name)
             (send current-editor editor-binding))
           (define serial
             (if is-same-file
                 (serialize+rehome (send current-editor get-editor) des-name)
                 (serialize (send current-editor get-editor))))
           (send current-editor set-editor! (eval `(deserialize ',serial) editor-namespace))
           (send current-editor set-namespace! editor-namespace)
           (send current-editor set-mod-name! editor-mod-name))
         (loop current-editor)]))
    ;; Finally, replace their text with an actual editor snip
    ;; Go from end of file to start to ensure the placements haven't changed.
    (send text set-file-format 'standard)
    (define sorted-editors
      (sort (set->list editors) > #:key second))
    (for ([e (in-list sorted-editors)])
      (with-handlers ([exn:fail? (λ (e)
                                   (raise e)
                                   (void))])
        (match-define `(#%editor ,elaborator ,editor)
          (let ([edi (first e)])
            (cond [(string? edi)
                   (with-input-from-string edi
                     (λ ()
                       (parameterize ([current-readtable (make-editor-readtable)])
                         (read))))]
                  [else edi])))
        (define des (eval `(deserialize ',editor) editor-namespace))
        (send text delete (sub1 (second e)) (sub1 (third e)) #f)
        (send text insert (new editor-snip%
                               [editor des]
                               [namespace editor-namespace]
                               [mod-name editor-mod-name]) (sub1 (second e)))))))
  
(define toggle-button
  (list "Update Editors"
        editor-icon
        (λ (this)
          (parameterize ([editor-read-as-snip? #t])
            (define text (send this get-definitions-text))
            (define port #f)
            (define data (mutable-set))
            ;; First, grab the location of every editor in text
            (dynamic-wind
             (λ ()
               (set! port (open-input-text-editor text 0 'end values text #t #:lock-while-reading? #t)))
             (λ ()
               (match-define-values (_ _ _ _ _ _ out)
                 (module-lexer port 0 #f))
               ;; This button 'should' only get clicked
               ;; when the language is editor.
               (when #t ;(eq? out lex-editor)
                 (let loop ()
                   (match-define-values (_ _ _ start end)
                     (lex-editor port
                                 #:fill-matches data))
                   (when (and start end)
                     (loop)))))
             (λ ()
               (close-input-port port)))
            ;; Finally, update the editors
            (update-editors! text data)))
          #f))

(define (editor-status-mixin super%)
  (class super%
    (define status-panel-parent #f)
    (define status-panel #f)
    (define status-editor #f)
    (define d/i-p-p #f)
    (define showing? #f)
    (super-new)
    (define/override (get-definitions/interactions-panel-parent)
      (unless status-panel-parent
        (set! status-panel-parent (new panel:vertical-dragable%
                                    [parent (super get-definitions/interactions-panel-parent)]))
        (set! status-panel (new gui:vertical-panel% [parent status-panel-parent]
                                [style '(border)]
                                [stretchable-width #f]
                                [stretchable-height #f]))
        (send status-panel-parent change-children (λ (l) '()))
        (define horiz-row (new gui:horizontal-pane% [parent status-panel]))
        (set! status-editor (new gui:text%))
        (new gui:editor-canvas% [parent horiz-row]
             [editor status-editor]
             [line-count 3])
        (new gui:button% [parent horiz-row]
             [label "Close"]
             [callback (λ (b e)
                         (hide-editor-error-panel))])
        (set! d/i-p-p (make-object gui:vertical-panel% status-panel-parent)))
      d/i-p-p)
    (define/public (show-editor-error-panel [msg ""])
      (unless showing?
        (send status-panel-parent change-children (λ (l) (cons status-panel l)))
        (send status-panel-parent set-percentages
              '(1/10 9/10))
        (set! showing? #t))
      (send status-editor begin-edit-sequence)
      (send status-editor select-all)
      (send status-editor clear)
      (send status-editor insert msg)
      (send status-editor end-edit-sequence))
    (define/public (hide-editor-error-panel)
      (when showing?
        (send status-panel-parent change-children (λ (l) (rest l)))
        (set! showing? #f)))))

(define-runtime-path background.rkt "background.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame editor-status-mixin)
    (drracket:module-language-tools:add-online-expansion-monitor	
     background.rkt 'expansion-monitor
     (λ (text data)
       (match data
         #;[(vector elaborator editor start end)
          (update-editors! text `(((#%editor ,(fasl->s-exp elaborator)
                                             ,(fasl->s-exp editor)) ,start ,end)))]
         [_ (void)])))))
