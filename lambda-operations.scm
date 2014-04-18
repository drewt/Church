(declare (unit lambda-operations)
         (uses lambda-ast)
         (export lambda-apply
                 lambda-equal?
                 lambda-freevars
                 lambda-reduce
                 lambda-substitute))

(include "ast-case.scm")

(define (lambda-equal? a b)
  (ast-case a
    ((var: name type)
      (and (variable? b) (char=? name (variable-name b))))
    ((fun: var body)
      (and (abstraction? b)
           (lambda-equal? var (abstraction-variable b))
           (lambda-equal? body (abstraction-body b))))
    ((app: fun arg)
      (and (application? b)
           (lambda-equal? fun (application-function b))
           (lambda-equal? arg (application-argument b))))))

(define (lambda-substitute ast v expr)
  (ast-case ast
    ((var: name type)
      (if (lambda-equal? ast v) expr ast))
    ((fun: var body)
      (if (lambda-equal? var v)
        ast
        (*make-fun var (lambda-substitute body v expr))))
    ((app: fun arg)
      (make-app (lambda-substitute fun v expr)
                (lambda-substitute arg v expr)))))

(define (lambda-apply fun arg)
  ; TODO: alpha-fixup
  (lambda-substitute (abstraction-body fun)
                     (abstraction-variable fun)
                     arg))

(define (lambda-reduce ast)
  (nor ast))
 
;; "call by name" reduction strategy
(define (cbn ast)
  (ast-case ast
    ((var: n t) ast)
    ((fun: v b) ast)
    ((app: app-fun arg)
      (let ((fun (cbn app-fun)))
        (if (abstraction? fun)
          (cbn (lambda-apply fun arg))
          (make-app fun arg))))))

;; normal order reduction strategy
(define (nor ast)
  (ast-case ast
    ((var: name type) ast)
    ((fun: var body) (*make-fun var (nor body)))
    ((app: app-fun arg)
      (let ((fun (cbn app-fun)))
        (if (abstraction? fun)
          (nor (lambda-apply fun arg))
          (make-app (nor fun) (nor arg)))))))

(define (lambda-freevars ast)
  (define (visit ast bound)
    (ast-case ast
      ((var: name type)
        (if (member name bound)
          '()
          (list name)))
      ((fun: var body)
        (visit body (cons (variable-name var) bound)))
      ((app: fun arg)
        (append (visit fun bound)
                (visit arg bound)))))
  (visit ast '())) 
