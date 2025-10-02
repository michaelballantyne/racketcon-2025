#lang racket

(provide query load-table)

(require (for-syntax syntax/parse syntax/transformer)
         "tables.rkt"
         (prefix-in rt: "embedded.rkt")
         (prefix-in sugar: "sugar.rkt"))

(define-syntax query
  (syntax-parser
    [(_ f c ...)
     #'(compile-from f c ...)]))

(begin-for-syntax
  (define (make-column-reference-transformer name)
    (make-variable-like-transformer
      (lambda (id)
        #`(sugar:col #,name))))

  (struct column-binding-rep (name)
    #:property prop:set!-transformer
    (lambda (rep stx)
      ((make-column-reference-transformer (column-binding-rep-name rep))
       stx))))

(define-syntax compile-from
  (syntax-parser
    #:datum-literals (from)
    [(_ (from tbl (c ...)) cl ...)
     #'(let-syntax ([c (column-binding-rep 'c)] ...)
         (rt:query
           (rt:compose-query (rt:from tbl) (rt:select 'c ...))
           (compile-clauses cl ...)))]))

(define-syntax checked-ref
  (syntax-parser
    [(_ name:id)
     (if (column-binding-rep? (syntax-local-value #'name (lambda () #f)))
         (syntax-property
            #''name 'disappeared-use
            (list (syntax-local-introduce #'name)))
         (raise-syntax-error #f "not a column available here" #'name))]))

(define-syntax compile-clauses
  (syntax-parser
    [(_ cl cl* ...)
     (syntax-parse #'cl
       #:datum-literals (select where join limit) 
       [(select name ...)
        #'(rt:compose-clauses
            (rt:select (checked-ref name) ...)
            (compile-clauses cl* ...))]
       [(where condition)
        #'(rt:compose-clauses
            (sugar:where condition)
            (compile-clauses cl* ...))]
       [(join tbl^ (c ...) col1 col2)
       #'(let-syntax ([c (column-binding-rep 'c)] ...)
           (rt:compose-clauses
            (rt:join (rt:compose-query (rt:from tbl^) (rt:select 'c ...))
                     (checked-ref col1) (checked-ref col2))
            (compile-clauses cl* ...)))]
       [(limit n)
        #'(rt:compose-clauses
           (rt:limit n)
           (compile-clauses cl* ...))])]
    [(_)
     #'(lambda (q) q)]))

