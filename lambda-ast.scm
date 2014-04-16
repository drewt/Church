;;
;; AST for the lambda calculus
;;
(declare (unit lambda-ast))

(define-record-type variable
  (make-var name)
  variable?
  (name variable-name))

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

(define (make-fun variables body)
  (if (null? (cdr variables))
    (*make-fun (car variables) body)
    (*make-fun (car variables)
               (make-fun (cdr variables) body))))


