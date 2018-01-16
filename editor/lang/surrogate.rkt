#lang racket/base

(provide (all-defined-out))
(require racket/class
         racket/list
         framework
         (only-in racket/gui/base open-input-text-editor)
         images/icons/style
         images/icons/control
         syntax-color/module-lexer
         racket/match
         racket/set
         racket/port
         racket/serialize
         "../stdlib.rkt"
         "read-editor.rkt")

(define surrogate%
  (class racket:text-mode%
    (super-new)))
#|
    (define to-convert '())
    (define/override (on-change-style orig call-inner start end)
      (define text (send orig get-text start (+ start end)))
      (when (equal? text "#editor")
        (set! to-convert (cons (list start end text) to-convert)))
      (call-inner start end))
    (define/override (after-edit-sequence orig call-inner)
      (define conv-list (sort to-convert > #:key first))
      (set! to-convert '())
      (for/list ([i (in-list conv-list)])
        (displayln i))
      (call-inner))))
|#

(define editor-icon
  (record-icon #:color "red"
               #:height (toolbar-icon-height)))

(define toggle-button
  (list "Update Editors"
        editor-icon
        (λ (this)
          (define text (send this get-definitions-text))
          (define port #f)
          (define data (mutable-set))
          ;; First grab the location of every editor
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
          ;; Then, replace their text with an actual editor snip
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
              ;(displayln (deserialize editor))
              (send text delete (sub1 (second e)) (sub1 (third e)) #f)
              (send text insert (new editor-snip% [editor (deserialize editor)]) (sub1 (second e))))))
        #f))
