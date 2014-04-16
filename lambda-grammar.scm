(require-extension lalr)

;;;
;;;;   The LALR(1) parser
;;;

(define lambda-parser
  (lalr-parser

   ;; --- Options 
   ;; output a parser, called lambda-parser, in a separate file - lambda.yy.scm, 
   (output:    lambda-parser "lambda.yy.scm")
   ;; there should be no conflict
   (expect:    5)

   ;; --- token definitions
   (ID LAMBDA LPAREN RPAREN DOT)

   ;; --- rules
   ;; whichever file includes this parser is responsible for providing
   ;; definitions for make-var et. al.
   (expr (var)                  : $1
         (fun)                  : $1
         (LPAREN expr RPAREN)   : $2
         (expr tail)            : (make-app $1 $2))
   (var  (ID)                   : (make-var $1))
   (vars (var)                  : (cons $1 '())
         (var vars)             : (cons $1 $2))
   (fun  (LAMBDA vars DOT expr) : (make-fun $2 $4))
   (tail (var)                  : $1
         (LPAREN expr RPAREN)   : $2)))
