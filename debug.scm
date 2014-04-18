
(define d-parse lambdaNB-parse-string)

(define (d-compile v)
  (define (*compile v)
    (cond
      ((string? v) (*compile (d-parse v)))
      ((list? v) (for-each (lambda (x)
                             (display "[ ")
                             (*compile x)
                             (display " ]"))
                           v))
      ((ast? v) (compile *ast-target* v))))
  (*compile v)
  (newline))

(define (d-reduce str)
  (d-compile (lambda-reduce (d-parse str))))
