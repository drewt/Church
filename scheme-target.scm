;;
;; Scheme target.
;;
(declare (unit scheme-target)
         (uses lambda-ast)
         (export *scheme-target*))

(include "ast-case.scm")

(define (scheme-compile ast out)
  (define (*scheme-compile ast)
    (ast-case ast
      ((var: name type) (string->symbol (string name)))
      ((fun: var body)
        (list 'lambda (list (*scheme-compile var))
                (*scheme-compile body)))
      ((app: fun arg)
        (list (*scheme-compile fun)
              (*scheme-compile arg)))))
  (display (*scheme-compile ast)))

(define *scheme-target*
  (make-compiler-target scheme-compile))
