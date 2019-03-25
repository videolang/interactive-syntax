#lang info

(define collection "editor")
;(define scribblings '(("scribblings/idmt.scrbl" ())))
(define raco-commands '(("editor-test"
                         (submod editor/test/raco main)
                         "Run tests for edit-time"
                         #f)))

(define drracket-tools '(("private/surrogate.rkt")))
(define drracket-tool-name '("Editor"))
(define drracket-tool-icons '(#f))
