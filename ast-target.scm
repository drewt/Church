;;
;; Abstract syntax tree target.
;;
(declare (unit ast-target)
         (uses lambda-ast)
         (export *ast-target*))

(include "ast-case.scm")

(define (ast-compile ast out)
  (define (*ast-compile ast)
    (ast-case ast
      ((var: name type)
        (list 'var name))
      ((fun: var body)
        (list 'fun (*ast-compile var)
                   (*ast-compile body)))
      ((app: fun arg)
        (list 'app (*ast-compile fun)
                   (*ast-compile arg)))))
  (display (*ast-compile ast)))

(define *ast-target*
  (make-compiler-target ast-compile))
