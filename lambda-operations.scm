(declare (unit lambda-operations)
         (uses lambda-ast)
         (export lambda-apply
                 lambda-equal?
                 lambda-freevars
                 lambda-reduce
                 lambda-substitute))

(require-extension srfi-14)
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
  (assert (abstraction? fun))
  (let ((fixed-fun (alpha-fixup fun arg)))
    (lambda-substitute (abstraction-body fixed-fun)
                       (abstraction-variable fun)
                       arg)))

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
          (list ast)))
      ((fun: var body)
        (visit body (cons (variable-name var) bound)))
      ((app: fun arg)
        (append (visit fun bound)
                (visit arg bound)))))
  (visit ast '()))

(define (lambda-vars ast)
  (ast-case ast
    ((var: name type) (list ast))
    ((fun: var body) (cons var (lambda-vars body)))
    ((app: fun arg) (append (lambda-vars fun)
                            (lambda-vars arg)))))

(define var-names
  (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (unused-var ast)
  (let ((used (lambda-vars ast)))
    (let loop ((cursor (char-set-cursor var-names)))
        (cond
          ((end-of-char-set? cursor) #f)
          ((in-set? (make-var (char-set-ref var-names cursor)) used)
            (loop (char-set-cursor-next var-names cursor)))
          (else (make-var (char-set-ref var-names cursor)))))))

(define (in-set? ast set)
  (cond
    ((null? set) #f)
    ((lambda-equal? ast (car set)) #t)
    (else (in-set? ast (cdr set)))))

(define (alpha-fixup fun arg)
  (let ((free (lambda-freevars arg)))
    (define (fixup ast)
      (ast-case ast
        ((var: name type) ast)
        ;; -- if fun-var free in arg, rename
        ((fun: var body)
          (if (in-set? var free)
            (let* ((new-var (unused-var ast))
                   (new-body (lambda-substitute body var new-var)))
              (*make-fun new-var (fixup new-body)))
            (*make-fun var (fixup body))))
        ((app: fun arg)
         (make-app (fixup fun) (fixup arg)))))
    (*make-fun (abstraction-variable fun)
               (fixup (abstraction-body fun)))))
