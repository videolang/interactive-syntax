#lang racket/base

(require racket/gui/base
         racket/class)

(define new-text%
  (class text%
    (inherit delete get-text)
    (super-new)
    (define/augment (after-change-style start end)
      (displayln (get-text start (+ start end)))
      (delete start (+ start end))
      (inner (void) after-change-style start end))))

(define nt (new new-text%))
(send nt insert "HELLO Wonderful WOarRLD")
(send nt change-style #f 6 16 #f)
(send nt change-style #f 8 10 #f)

(define f (new frame% [label "A new kind of editor"]))
(new editor-canvas% [parent f]
     [editor nt]
     [min-width 300]
     [min-height 300])
(send f show #t)
