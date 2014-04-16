;;
;; Parser for the lambda calculus (including shorthand notation).
;;
(declare (unit lambda-parser)
         (uses lambda-ast))

(include "lambda.yy.scm")
(include "lambda-lexer.scm")

(define (parse str)
  (with-input-from-string
    str
    (lambda () (lambda-parser (make-lexer errorp) errorp))))

;(define (parse)
;  (lambda-parser (make-lexer errorp) errorp))

(define (errorp msg . args)
  (display msg)
  (for-each display args))

(define (ast->string ast)
  (cond
    ((variable? ast) (string (variable-name ast)))
    ((abstraction? ast)
      (string-append (string #\( #\u03bb)
                     (ast->string (abstraction-variable ast))
                     (string #\.)
                     (ast->string (abstraction-body ast))
                     ")"))
    ((application? ast)
      (string-append "("
                     (ast->string (application-function ast))
                     (ast->string (application-argument ast))
                     ")"))
    (else
      (display "ERROR: Non-AST object in AST: ")
      (display ast))))
