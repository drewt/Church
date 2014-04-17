(declare (unit lambda-operations)
         (uses lambda-ast)
         (export lambda-apply
                 lambda-equal?
                 lambda-freevars
                 lambda-reduce
                 lambda-substitute))

(define-syntax ast-case
  (syntax-rules (var fun app)
    ((ast-case ast
       ((var var-name)         var-case ...)
       ((fun fun-var fun-body) fun-case ...)
       ((app app-fun app-arg)  app-case ...))
     (cond
       ((variable? ast)
         (let ((var-name (variable-name ast)))
           var-case ...))
       ((abstraction? ast)
         (let ((fun-var (abstraction-variable ast))
               (fun-body (abstraction-body ast)))
           fun-case ...))
       ((application? ast)
         (let ((app-fun (application-function ast))
               (app-arg (application-argument ast)))
           app-case ...))))))

(define (lambda-equal? a b)
  (ast-case a
    ((var a-name)
      (and (variable? b) (char=? a-name (variable-name b))))
    ((fun a-var a-body)
      (and (abstraction? b)
           (lambda-equal? a-var (abstraction-variable b))
           (lambda-equal? a-body (abstraction-body b))))
    ((app a-fun a-arg)
      (and (application? b)
           (lambda-equal? a-fun (application-function b))
           (lambda-equal? a-arg (application-argument b))))))

(define (lambda-substitute ast v expr)
  (ast-case ast
    ((var var-name)
      (if (lambda-equal? ast v) expr ast))
    ((fun fun-var fun-body)
      (if (lambda-equal? fun-var v)
        ast
        (*make-fun fun-var (lambda-substitute fun-body v expr))))
    ((app app-fun app-arg)
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
  (if (or (variable? ast) (abstraction? ast))
    ast
    (let ((fun (cbn (application-function ast)))
          (arg (application-argument ast)))
      (if (abstraction? fun)
        (cbn (lambda-apply fun arg))
        (make-app fun arg)))))

;; normal order reduction strategy
(define (nor ast)
  (cond
    ((variable? ast) ast)
    ((abstraction? ast)
      (*make-fun (abstraction-variable ast)
                 (nor (abstraction-body ast))))
    ((application? ast)
      (let ((fun (cbn (application-function ast)))
            (arg (application-argument ast)))
        (if (abstraction? fun)
          (nor (lambda-apply fun arg))
          (make-app (nor fun) (nor arg)))))))

(define (lambda-freevars ast)
  (define (visit ast bound)
    (ast-case ast
      ((var var-name)
        (if (member var-name bound)
          '()
          (list var-name)))
      ((fun fun-var fun-body)
        (visit fun-body (cons (variable-name fun-var) bound)))
      ((app app-fun app-arg)
        (append (visit app-fun bound)
                (visit app-arg bound)))))
  (visit ast '())) 
