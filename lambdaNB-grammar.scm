(require-extension lalr)

;;;
;;;;   The LALR(1) parser
;;;

(define lambdaNB-parser
  (lalr-parser

   ;; --- Options 
   ;; output a parser, called lambdaNB-parser, in a separate file - lambdaNB.yy.scm, 
   (output:    lambdaNB-parser "lambdaNB.yy.scm")
   ;; there should be no conflict
   (expect:    5)

   ;; --- token definitions
   (ID LAMBDA BOOL NUM BUILTIN LPAREN RPAREN DOT)

   ;; --- rules
   ;; whichever file includes this parser is responsible for providing
   ;; definitions for make-var et. al.
   (expr  (const)                  : $1
          (fun)                  : $1
          (LPAREN expr RPAREN)   : $2
          (expr tail)            : (make-app $1 $2))
   (var   (ID)                   : (make-var $1))
   (vars  (var)                  : (cons $1 '())
          (var vars)             : (cons $1 $2))
   (const (var)                  : $1
          (NUM)                  : (make-num $1)
          (BOOL)                 : (make-bool $1)
          (BUILTIN)              : (make-builtin $1))
   (fun   (LAMBDA vars DOT expr) : (make-fun $2 $4))
   (tail  (const)                  : $1
          (LPAREN expr RPAREN)   : $2)))
