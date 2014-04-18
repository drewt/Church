(define-syntax ast-case
  (syntax-rules (var: fun: app:)
    ((ast-case ast
       ((var: var-name var-type) var-case ...)
       ((fun: fun-var fun-body)  fun-case ...)
       ((app: app-fun app-arg)   app-case ...))
     (cond
       ((variable? ast)
         (let ((var-name (variable-name ast))
               (var-type (variable-type ast)))
           var-case ...))
       ((abstraction? ast)
         (let ((fun-var (abstraction-variable ast))
               (fun-body (abstraction-body ast)))
           fun-case ...))
       ((application? ast)
         (let ((app-fun (application-function ast))
               (app-arg (application-argument ast)))
           app-case ...))))))
 
