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
   (ID ARROW LAMBDA BOOL NUM BUILTIN LPAREN RPAREN DOT COMMA COLON)

   ;; --- rules
   ;; whichever file includes this parser is responsible for providing
   ;; definitions for make-var et. al.
   (expr  (const)                 : $1
          (fun)                   : $1
          (LPAREN expr RPAREN)    : $2
          (expr tail)             : (make-app $1 $2))
   (var   (ID)                    : (make-var $1))
   (*str  (ID)                    : (cons $1 '())
          (ID *str)               : (cons $1 $2))
   (str   (*str)                  : (list->string $1))
   (type  (str)                   : (make-type $1 #f)
          (LPAREN type RPAREN)    : $2
          (type ARROW type)       : (make-type $1 $3))
   (farg  (ID COLON type)         : (make-var $1 $3))
   (fargs (farg)                  : (cons $1 '())
          (farg COMMA fargs)      : (cons $1 $3))
   (const (var)                   : $1
          (NUM)                   : (make-num $1)
          (BOOL)                  : (make-bool $1)
          (BUILTIN)               : (make-builtin $1))
   (fun   (LAMBDA fargs DOT expr) : (make-fun $2 $4))
   (tail  (const)                 : $1
          (LPAREN expr RPAREN)    : $2)))
