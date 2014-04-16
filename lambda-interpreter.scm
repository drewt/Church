(declare (uses target
               lambda-parser
               lambda-target))
(use extras)

(let loop ()
  (display "> ")
  (let ((expr (read-line)))
    (if (eqv? expr #!eof)
      (begin (display "quit") (newline))
      (let ((ast (parse expr)))
        (compile *lambda-target* ast)
        (newline)
        (loop)))))
