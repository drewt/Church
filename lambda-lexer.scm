(require-extension lalr)

;;;
;;;;   The lexer
;;;

(define (force-output) #f)
(define (port-line port) 
  (let-values (((line _) (port-position port)))
    line))

(define (port-column port)
  (let-values (((_ column) (port-position port)))
    column)) 

(define (char-variable? c)
  (or (char-alphabetic? c) (char-greek? c)))

(define (char-greek? c)
  (let ((code (char->integer c)))
    (cond
      ((< code #x370) #f)
      ;; -- Greek alphabet
      ((< code #x374) #t)
      ((< code #x376) #f)
      ((< code #x378) #t)
      ((< code #x37b) #f)
      ((< code #x37e) #t)
      ((= code #x386) #t)
      ((< code #x388) #f)
      ((= code #x38b) #f)
      ((= code #x38d) #f)
      ((= code #x3a2) #f)
      ((= code #x3bb) #f) ; lowercase lambda
      ((< code #x400) #t)
      ((< code #x1d6a8) #f)
      ;; -- mathematical lambdas
      ((= code #x1d6cc) #f)
      ((= code #x1d706) #f)
      ((= code #x1d740) #f)
      ((= code #x1d77a) #f)
      ((= code #x1d7b4) #f)
      ;; -- mathematical greek
      ((< code #x1d7cc) #t)
      (else #f))))

(define (char-operator? c)
  (member c '(#\$ #\? #\+ #\- #\* #\^ #\/ #\%)))

(define (make-lexer errorp)
  (lambda ()
    (letrec ((skip-spaces
              (lambda ()
                (let loop ((c (peek-char)))
                  (if (and (not (eof-object? c))
                           (or (char=? c #\space)
                               (char=? c #\tab)
                               (char=? c #\newline)))
                    (begin
                      (read-char)
                      (loop (peek-char)))))))
             (read-number
               (lambda (l)
                 (let ((c (peek-char)))
                   (if (char-numeric? c)
                     (read-number (cons (read-char) l))
                     (string->number (apply string (reverse l)))))))
             (read-name
               (lambda (l)
                 (let ((c (peek-char)))
                   (if (char-alphabetic? c)
                     (read-name (cons (read-char) l))
                     (apply string (reverse l)))))))

      ;; -- skip spaces
      (skip-spaces)
      ;; -- read the next token
      (let loop ()
        (let* ((location (make-source-location
                           "*stdin*"
                           (port-line (current-input-port))
                           (port-column (current-input-port)) -1 -1))
               (c (read-char)))
          (cond ((eof-object? c)      '*eoi*)
                ((char=? c #\.)       (make-lexical-token 'DOT     location #f))
                ((or (char=? c #\u03bb)
                     (char=? c #\\ )) (make-lexical-token 'LAMBDA  location #f))
                ((char=? c #\()       (make-lexical-token 'LPAREN  location #f))
                ((char=? c #\))       (make-lexical-token 'RPAREN  location #f))
                ((char=? c #\#)
                  (let ((v (read-char)))
                    (cond
                      ((char=? v #\t) (make-lexical-token 'BOOL    location #t))
                      ((char=? v #\f) (make-lexical-token 'BOOL    location #f))
                      (else (errorp "PARSE ERROR : illegal literal: " v)))))
                ((char=? c #\`)       (make-lexical-token 'BUILTIN location (read-name '())))
                ((char-operator? c)   (make-lexical-token 'BUILTIN location c))
                ((char-numeric? c)    (make-lexical-token 'NUM     location (read-number (list c))))
                ((char-variable? c)   (make-lexical-token 'ID      location c))
                (else
                 (errorp "PARSE ERROR : illegal character: " c)
                 (skip-spaces)
                 (loop))))))))
