;;
;; Parser for the simply-typed lambda calculus (including shorthand notation).
;;
(declare (unit simply-typed-parser)
         (uses lambda-ast))

(include "simply-typed.yy.scm")
(include "lambda-lexer.scm")

(define (st-parse-string str)
  (with-input-from-string
    str
    (lambda () (simply-typed-parser (make-lexer errorp) errorp))))

(define (st-parse)
  (simply-typed-parser (make-lexer errorp) errorp))

(define (errorp msg . args)
  (display msg)
  (for-each display args))


