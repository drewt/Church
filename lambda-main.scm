(declare (uses interpreter
               target
               lambda-operations
               lambdaNB-parser
               lambda-target))
(use extras)

(define (run-interpreter)
  (interpreter parse
               (lambda (x) (compile *lambda-target* x))
               lambda-reduce))

(define (do-parse)
  (if expression
    (parse-string expression)
    (parse)))

(define (run-reduce)
  (compile *lambda-target* (lambda-reduce (do-parse))))

(define (run-parser)
  (compile *lambda-target* (do-parse)))

(define interactive? #f)
(define reduce? #f)
(define parse? #f)
(define expression #f)

(let loop ((args (command-line-arguments)))
  (when (not (null? args))
    (case (string->symbol (car args))
      ((-i --interactive) (set! interactive? #t))
      ((-r --reduce)      (set! reduce? #t))
      ((-p --parse)       (set! parse? #t))
      ((-e --expression)  (unless (null? (cdr args))
                            (set! expression (cadr args))
                            (set! args (cdr args))))
      (else (fprintf (current-error-port)
                     "unrecognized argument: ~a~n"
                     (car args))))
    (loop (cdr args))))

(cond
  (interactive? (run-interpreter))
  (reduce?      (run-reduce))
  (parse?       (run-parser))
  (else         (run-interpreter)))
