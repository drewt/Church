(declare (uses interpreter
               target
               lambda-operations
               lambda-parser
               lambdaNB-parser
               lambda-target
               scheme-target))
(use extras)

(define interactive? #f)
(define reduce? #f)
(define parse? #f)
(define parse lambdaNB-parse)
(define parse-string lambdaNB-parse-string)
(define target *lambda-target*)
(define expression #f)

(define (run-interpreter)
  (interpreter parse-string
               (lambda (x) (compile target x))
               lambda-reduce))

(define (do-parse)
  (if expression
    (parse-string expression)
    (parse)))

(define (run-reduce)
  (compile target (lambda-reduce (do-parse))))

(define (run-parser)
  (compile target (do-parse)))

(let loop ((args (command-line-arguments)))
  (when (not (null? args))
    (case (string->symbol (car args))
      ((-i --interactive) (set! interactive? #t))
      ((-r --reduce)      (set! reduce? #t))
      ((-p --parse)       (set! parse? #t))
      ((--sugarless)      (set! parse lambda-parse)
                          (set! parse-string lambda-parse-string))
      ((--scheme)         (set! target *scheme-target*))
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
