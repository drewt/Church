;;
;; AST for the lambda calculus
;;
(declare (unit lambda-ast))

(define-record-type type
  (make-type param return)
  type?
  (param type-param)
  (return type-return))

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

(define (make-var name #!optional (type #f))
  (*make-var name type))

(define (make-fun variables body)
  (cond
    ((variable? variables) (*make-fun variables body))
    ((null? (cdr variables))
      (*make-fun (car variables) body))
    (else
      (*make-fun (car variables)
                 (make-fun (cdr variables) body)))))

;; -- An AST is a variable, abstraction or application.  Types are not
;;    considered part of an AST, but rather attributes of AST nodes.
(define (ast? v)
  (or (variable? v)
      (abstraction? v)
      (application? v)))

;; -- A base type is any type without a return type.
(define (base-type? type)
  (and (type? type) (not (type-return type))))

;; -- The name of a base type is stored as the param type.
(define base-type-name type-param)
