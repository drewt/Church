(declare (uses target
               lambda-operations
               lambdaNB-parser
               lambda-target))
(use extras)

(let loop ()
  (display "> ")
  (let ((expr (read-line)))
    (if (eqv? expr #!eof)
      (begin (display "quit") (newline))
      (let ((ast (parse expr)))
        (compile *lambda-target* ast)
        (newline) (display "-> ")
        (compile *lambda-target* (lambda-reduce ast))
        (newline)
        (loop)))))
