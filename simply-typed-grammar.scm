(require-extension lalr)

;;;
;;;;   The LALR(1) parser
;;;

(define simply-typed-parser
  (lalr-parser

   ;; --- Options 
   ;; output a parser, called lambdaNB-parser, in a separate file - lambdaNB.yy.scm, 
   (output:    simply-typed-parser "simply-typed.yy.scm")
   ;; there should be no conflict
   (expect:    5)

   ;; --- token definitions
   (ID LAMBDA BOOL NUM BUILTIN LPAREN RPAREN DOT COLON)

   ;; --- rules
   ;; whichever file includes this parser is responsible for providing
   ;; definitions for make-var et. al.
   (expr  (const)                 : $1
          (fun)                   : $1
          (LPAREN expr RPAREN)    : $2
          (expr tail)             : (make-app $1 $2))
   (var   (ID)                    : (make-var $1))
   (type  (ID COLON ID)           : (*make-var $1 $3))
   (types (type)                  : (cons $1 '())
          (type types)            : (cons $1 $2))
   (const (var)                   : $1
          (NUM)                   : (make-num $1)
          (BOOL)                  : (make-bool $1)
          (BUILTIN)               : (make-builtin $1))
   (fun   (LAMBDA types DOT expr) : (make-fun $2 $4))
   (tail  (const)                 : $1
          (LPAREN expr RPAREN)    : $2)))
