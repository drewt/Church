(require-extension lalr-driver)
(define lambda-parser
  (lr-driver
    '#(((*default* *error*) (ID 3) (LAMBDA 2) (LPAREN 1))
       ((*default* *error*) (ID 3) (LAMBDA 2) (LPAREN 1))
       ((*default* *error*) (ID 3))
       ((*default* -6))
       ((*default* -3))
       ((*default* -2))
       ((*default* *error*) (*eoi* 11) (ID 3) (LPAREN 10))
       ((*default* *error*) (ID 3) (LPAREN 10) (RPAREN 14))
       ((*default* *error*) (DOT 15))
       ((*default* -7) (ID 3))
       ((*default* *error*) (ID 3) (LAMBDA 2) (LPAREN 1))
       ((*default* -1) (*eoi* accept))
       ((*default* -5))
       ((*default* -10))
       ((*default* -4))
       ((*default* *error*) (ID 3) (LAMBDA 2) (LPAREN 1))
       ((*default* -8))
       ((*default* *error*) (ID 3) (LPAREN 10) (RPAREN 19))
       ((*default* -9) (ID 3) (LPAREN 10))
       ((*default* -11)))
    (vector
      '((4 . 4) (2 . 5) (1 . 6))
      '((4 . 4) (2 . 5) (1 . 7))
      '((3 . 8) (2 . 9))
      '()
      '()
      '()
      '((5 . 12) (2 . 13))
      '((5 . 12) (2 . 13))
      '()
      '((3 . 16) (2 . 9))
      '((4 . 4) (2 . 5) (1 . 17))
      '()
      '()
      '()
      '()
      '((4 . 4) (2 . 5) (1 . 18))
      '()
      '((5 . 12) (2 . 13))
      '((5 . 12) (2 . 13))
      '())
    (vector
      '()
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          $1))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 1 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 1 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 1 $2)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 1 (make-app $1 $2))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 2 (make-var $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 3 (cons $1 '()))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 3 (cons $1 $2))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($4 (vector-ref ___stack (- ___sp 1)))
               ($3 (vector-ref ___stack (- ___sp 3)))
               ($2 (vector-ref ___stack (- ___sp 5)))
               ($1 (vector-ref ___stack (- ___sp 7))))
          (___push 4 4 (make-fun $2 $4))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 5 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 $2))))))
