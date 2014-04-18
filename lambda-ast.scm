;;
;; AST for the lambda calculus
;;
(declare (unit lambda-ast))

(define-record-type variable
  (*make-var name type)
  variable?
  (name variable-name)
  (type variable-type))

(define-record-type abstraction
  (*make-fun variable body)
  abstraction?
  (variable abstraction-variable)
  (body abstraction-body))

(define-record-type application
  (make-app function argument)
  application?
  (function application-function)
  (argument application-argument))

(define (make-var name)
  (*make-var name #f))

(define (make-fun variables body)
  (if (null? (cdr variables))
    (*make-fun (car variables) body)
    (*make-fun (car variables)
               (make-fun (cdr variables) body))))

(define (ast? v)
  (or (variable? v)
      (abstraction? v)
      (application? v)))
