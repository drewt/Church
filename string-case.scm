
(define-syntax p-case
  (syntax-rules (otherwise)
    ((p-case v eql?)
      (void))
    ((p-case v eql? (otherwise body ...))
      (begin body ...))
    ((p-case v eql? ((vs ...) body ...) rest ...)
      (if (foldl (lambda (a x) (or a (eql? x v)))
                 #f
                 (list vs ...))
        (begin body ...)
        (p-case v eql? rest ...)))))

(define-syntax string-case
  (syntax-rules ()
    ((string-case str rest ...)
      (p-case str string=? rest ...))))
