#lang racket/base

(provide (all-defined-out))
(require racket/class
         racket/list
         framework
         (only-in racket/gui/base
                  open-input-text-editor
                  make-gui-empty-namespace)
         images/icons/style
         images/icons/control
         syntax-color/module-lexer
         racket/match
         racket/set
         racket/port
         racket/serialize
         "../lang.rkt"
         "context.rkt"
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
    ;; Ensure all editors in a buffer use the same namespace
    (define editor-namespace (make-editor-namespace))
    (define/public (get-editor-namespace)
      editor-namespace)
    (define/public (reset-editor-namespace)
      (set! editor-namespace (make-editor-namespace)))
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

(define toggle-button
  (list "Update Editors"
        editor-icon
        (λ (this)
          (send text reset-editor-namespace)
          (define text (send this get-definitions-text))
          (define editor-namespace (send text get-editor-namespace))
          (define meybe-file
            (let ()
              (define tmp (box #f))
              (define file (send text get-filename))
              (and tmp file)))
          (define port #f)
          (define data (mutable-set))
          ;; First, update all editor-snips already in use.
          (let loop ([last-editor #f])
            (define current-editor (send text find-next-non-string-snip last-editor))
            (cond
              [(not current-editor) (void)]
              [else
               (when (is-a? current-editor editor-snip%)
                 (define serial
                   (serialize (send current-editor get-editor)))
                 (define new-editor (eval `(deserialize ',serial) editor-namespace))
                 (send current-editor set-editor! new-editor))
               (loop current-editor)]))
          ;; Then grab the location of every editor in text
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
          ;; Finally, replace their text with an actual editor snip
          (send text set-file-format 'standard)
          (define sorted-editors
            (sort (set->list data) > #:key second))
          (for ([e (in-list sorted-editors)])
            (with-handlers ([exn:fail? (λ (e)
                                         (raise e)
                                         (void))])
              (match-define `(#%editor ,elaborator ,editor)
                (with-input-from-string (first e)
                  (λ ()
                    (parameterize ([current-readtable (make-editor-readtable)])
                      (read)))))
              (define des (eval `(deserialize ',editor) editor-namespace))
              (send text delete (sub1 (second e)) (sub1 (third e)) #f)
              (send text insert (new editor-snip% [editor des]) (sub1 (second e))))))
        #f))
