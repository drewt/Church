(declare (unit interpreter))
(use extras)

(include "string-case.scm")

(define (*read-line n)
  (printf "#;~a> " n)
  (read-line))

(define (interpreter parse compile evaluate #!optional (mode 'normal))
  (define (interpret expr)
    (let ((ast (parse expr)))
      (case mode
        ((normal)
          (compile ast) (newline)
          (display (string #\u2192 #\space))
          (compile (evaluate ast)) (newline))
        ((parse)
          (compile ast) (newline))
        ((reduce)
          (compile (evaluate ast)) (newline)))))
  (let loop ((iter 1))
      (let ((expr (*read-line iter)))
        (cond
          ;; ^D / EOF
          ((eqv? expr #!eof)
            (display ":quit") (newline))
          ;; interpreter built-in
          ((char=? (car (string->list expr)) #\:)
            (let ((name (list->string (cdr (string->list expr)))))
              (string-case name
                (("q" "quit")   (void))
                (("n" "normal") (set! mode 'normal) (loop iter))
                (("p" "parse")  (set! mode 'parse)  (loop iter))
                (("r" "reduce") (set! mode 'reduce) (loop iter))
                (otherwise (printf "Unknown command: ~a~n" name)
                           (loop iter)))))
          ;; expression
          (else
            (interpret expr)
            (loop (+ iter 1)))))))
