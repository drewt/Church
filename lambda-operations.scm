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
    ((var: a-name)
      (and (variable? b) (char=? a-name (variable-name b))))
    ((fun: a-var a-body)
      (and (abstraction? b)
           (lambda-equal? a-var (abstraction-variable b))
           (lambda-equal? a-body (abstraction-body b))))
    ((app: a-fun a-arg)
      (and (application? b)
           (lambda-equal? a-fun (application-function b))
           (lambda-equal? a-arg (application-argument b))))))

(define (lambda-substitute ast v expr)
  (ast-case ast
    ((var: var-name)
      (if (lambda-equal? ast v) expr ast))
    ((fun: fun-var fun-body)
      (if (lambda-equal? fun-var v)
        ast
        (*make-fun fun-var (lambda-substitute fun-body v expr))))
    ((app: app-fun app-arg)
      (make-app (lambda-substitute app-fun v expr)
                (lambda-substitute app-arg v expr)))))

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
    ((var: n) ast)
    ((fun: v b) ast)
    ((app: app-fun arg)
      (let ((fun (cbn app-fun)))
        (if (abstraction? fun)
          (cbn (lambda-apply fun arg))
          (make-app fun arg))))))

;; normal order reduction strategy
(define (nor ast)
  (ast-case ast
    ((var: name) ast)
    ((fun: var body) (*make-fun var (nor body)))
    ((app: app-fun arg)
      (let ((fun (cbn app-fun)))
        (if (abstraction? fun)
          (nor (lambda-apply fun arg))
          (make-app (nor fun) (nor arg)))))))

(define (lambda-freevars ast)
  (define (visit ast bound)
    (ast-case ast
      ((var: var-name)
        (if (member var-name bound)
          '()
          (list var-name)))
      ((fun: fun-var fun-body)
        (visit fun-body (cons (variable-name fun-var) bound)))
      ((app: app-fun app-arg)
        (append (visit app-fun bound)
                (visit app-arg bound)))))
  (visit ast '())) 
