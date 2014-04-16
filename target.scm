;;
;; Compiler target interface.
;;
(declare (unit target))

(define-record-type compiler-target
  (make-compiler-target compile)
  compiler-target?
  ;; compile function
  ;; -- takes an AST and an output port as arguments.
  (compile target-compile))

(define (compile target ast #!optional (out (current-output-port)))
  ((target-compile target) ast out))
