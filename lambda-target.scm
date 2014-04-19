
;;
;; Lambda calculus target.  Takes a lambda AST back to its representation in
;; (shorthand) lambda calculus.
;;
(declare (unit lambda-target)
         (uses lambda-ast)
         (export *lambda-target*))

(define-syntax parenthesize
  (syntax-rules ()
    ((parenthesize expr)
      (begin (display #\()
             expr
             (display #\))))))

(define (compile-type type out)
  (cond
    ((base-type? type)
      (display (type-param type) out))
    ((base-type? (type-param type))
      (compile-type (type-param type) out)
      (display #\u2192 out)
      (compile-type (type-return type) out))
    (else
      (parenthesize (compile-type (type-param type) out))
      (display #\u2192 out)
      (compile-type (type-return type) out))))

(define (compile-variable var out)
  (let ((name (variable-name var))
        (type (variable-type var)))
    (display name out)
    (when type
      (display #\: out)
      (compile-type type out))))

(define (compile-abstraction fun out)
  (define (compile-subabstraction fun out)
    (lambda-compile (abstraction-variable fun) out)
    (let ((body (abstraction-body fun)))
      (if (abstraction? body)
        (begin (display #\, out)
               (compile-subabstraction body out))
        (begin (display #\. out)
               (lambda-compile body out)))))
  (display #\u03bb out)
  (compile-subabstraction fun out))

(define (compile-application ast out)
  (let ((fun (application-function ast))
        (arg (application-argument ast)))
    (when (abstraction? fun)
      (display #\())
    (lambda-compile fun out)
    (when (abstraction? fun)
      (display #\)))
    (when (not (variable? arg))
      (display #\())
    (lambda-compile arg out)
    (when (not (variable? arg))
      (display #\)))))

(define (lambda-compile ast out)
  (cond
    ((variable? ast)    (compile-variable ast out))
    ((abstraction? ast) (compile-abstraction ast out))
    ((application? ast) (compile-application ast out))
    (else (fprintf (current-error-port)
                   "ERROR: unexpected value: ~a~n" ast))))

(define *lambda-target*
  (make-compiler-target lambda-compile))
