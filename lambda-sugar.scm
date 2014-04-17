;;
;;
;;
(declare (unit lambda-sugar)
         (uses lambda-ast))

(define-syntax *fun
  (syntax-rules ()
    ((*fun (vars ...) fst snd rest ...)
      (make-fun (list->ast-list (list vars ...))
                (*make-app (list->ast-list (list fst snd rest ...)))))
    ((*fun (vars ...) body)
      (make-fun (list vars ...) body))))

(define (*make-app lst)
  (let loop ((prev (make-app (car lst) (cadr lst))) (rest (cddr lst)))
    (if (null? rest)
      prev
      (loop (make-app prev (car rest)) (cdr rest)))))

(define-syntax *app
  (syntax-rules ()
    ((*app fst snd rest ...)
      (*make-app (list->ast-list (list fst snd rest ...))))))

(define (list->ast-list lst)
  (let loop ((lst lst))
    (if (null? lst)
      lst
      (if (char? (car lst))
        (cons (make-var (car lst))
              (loop (cdr lst)))
        (cons (car lst)
              (loop (cdr lst)))))))

(define (make-num n)
  (let loop ((prev (make-var #\x)) (n n))
    (if (= n 0)
      (*fun (#\f #\x) prev)
      (loop (*app #\f prev) (- n 1)))))

(define (make-bool v)
  (if v TRUE FALSE))

(define (make-builtin f)
  (if (char? f)
    (char-fun f)
    (named-fun f))) 

(define (char-fun c)
  (case c
    ((#\$) SUCC)
    ((#\?) PRED)
    ((#\+) ADD)
    ((#\-) SUB)
    ((#\*) MUL)
    ((#\/) DIV)
    ((#\%) MOD)
    ((#\^) POW)))

(define (named-fun name)
  (cond
    ((string-ci=? name "if")    IFTHEN)
    ((string-ci=? name "and")   LAND)
    ((string-ci=? name "or")    LOR)
    ((string-ci=? name "not")   LNOT)
    ((string-ci=? name "cons")  PAIR)
    ((string-ci=? name "nil")   NIL)
    ((string-ci=? name "null?") LNULL?)
    ((string-ci=? name "car")   FST)
    ((string-ci=? name "cdr")   SND)
    ((string-ci=? name "succ")  SUCC)
    ((string-ci=? name "pred")  PRED)
    ((string-ci=? name "add")   ADD)
    ((string-ci=? name "sub")   SUB)
    ((string-ci=? name "mul")   MUL)
    ((string-ci=? name "exp")   POW)
    ((string-ci=? name "pdiv")  PDIV)
    ((string-ci=? name "div")   DIV)
    ((string-ci=? name "mod")   MOD)
    ((string-ci=? name "zero?") LZERO?)
    ((string-ci=? name "<")     LT?)
    ((string-ci=? name "<=")    LEQ?)
    ((string-ci=? name "=")     EQL?)
    ((string-ci=? name "!=")    NEQ?)
    ((string-ci=? name ">=")    GEQ?)
    ((string-ci=? name ">")     GT?)))

(define TRUE   (*fun (#\x #\y) #\x))
(define FALSE  (*fun (#\x #\y) #\y))
(define LAND   (*fun (#\p #\q) #\p #\q #\p))
(define LOR    (*fun (#\p #\q) #\p #\p #\q))
(define LNOT   (*fun (#\p #\a #\b) #\p #\b #\a))
(define IFTHEN (*fun (#\x #\y #\x) #\x #\y #\z))

(define PAIR    (*fun (#\x #\y #\f) #\f #\x #\y))
(define FST     (*fun (#\p) #\p TRUE))
(define SND     (*fun (#\p) #\p FALSE))
(define NIL     (*fun (#\x) TRUE))
(define LNULL?) (*fun (#\p) #\p (*fun (#\x #\y) FALSE))

(define Y (*fun (#\g) (*fun (#\x) #\g (*app #\x #\x))
                      (*fun (#\x) #\g (*app #\x #\x))))

(define SUCC (*fun (#\n #\f #\x)
               #\f
               (*app #\f
                     (*app #\n #\f #\x))))

(define PRED (*fun (#\n #\f #\x)
               #\n
               (*fun (#\g #\h) #\h (*app #\g #\f))
               (*fun (#\u) #\x)
               (*fun (#\u) #\u)))

(define ADD (*fun (#\m #\n #\f #\x)
              (*app #\m #\f)
              (*app #\n #\f #\x)))

(define SUB (*fun (#\m #\n) #\m PRED #\n))

(define MUL (*fun (#\m #\n #\f) #\m (*app #\n #\f)))

(define POW (*fun (#\b #\e) #\e #\b))

;;
;; Numeric comparisons
;;

(define LZERO? (*fun (#\n) #\n (*fun (#\x) FALSE) TRUE))
(define LEQ?   (*fun (#\m #\n) LZERO? (*app SUB #\m #\n)))
(define LT?    (*fun (#\a #\b) LNOT (*app LEQ? #\b #\a)))
(define EQL?   (*fun (#\m #\n) LAND (*app LEQ? #\m #\n) (*app LEQ? #\n #\m)))
(define NEQ?   (*fun (#\a #\b) LOR (*app LNOT (*app LEQ? #\a #\b))
                                   (*app LNOT (*app LEQ? #\b #\a))))
(define GEQ?   (*fun (#\a #\b) LEQ? #\b #\a))
(define GT?    (*fun (#\a #\b) LNOT (*app LEQ? #\a #\b)))

;;
;; PDIV needs LT?, so it goes here
;;

(define PDIV (*app Y
                   ;; PDIV quotient nr/remainder divisor
                   (*fun (#\g #\q #\n #\d)
                     LT? #\n #\d
                       (*app PAIR #\q #\n)
                       (*app #\g (*app SUCC #\q)
                                 (*app SUB #\n #\d)
                                 #\d))
                   (make-num 0)))

(define DIV (*fun (#\a #\b) FST (*app PDIV #\a #\b)))
(define MOD (*fun (#\a #\b) SND (*app PDIV #\a #\b)))
