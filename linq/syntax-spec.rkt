#lang racket

(provide (all-defined-out) load-table)

(require (for-syntax racket/match
                     syntax/parse)
         syntax-spec-v3
         "tables.rkt"
         (prefix-in rt: "embedded.rkt")
         (prefix-in sugar: "sugar.rkt"))

(module+ test (require rackunit))

(define-syntax-rule
  (query/print f c ...)
  (pretty-display (time (query f c ...))))

(syntax-spec
  (host-interface/expression
    (query f:from-clause c:clause ...)
    #:binding (nest f c ... [])

    #'(compile-query (query f c ...)))

  (nonterminal/nesting from-clause (nested)
    (from tbl:racket-expr (c:col ...))
    #:binding (scope (bind c) ... nested))
  
  (nonterminal/nesting clause (nested)
    (select q:col ...)
    (where condition:racket-expr)
    (join tbl:racket-expr (c:col ...) col1:col col2:col)
    #:binding (scope (bind c) ... col2 nested)
    (limit n:racket-expr))
    
  (binding-class col
    #:description "column name"
    #:reference-compiler col-reference-compiler))


(define-syntax compile-query
  (syntax-parser
    [(_ (query f c ...))

     (define/syntax-parse (c^ ...) (predicate-pushdown #'(c ...)))

     #'(rt:query (compile-from f)
                 (compile-clause c^)
                 ...)]))


;; Compile a `from` syntax into an expression that evaluates to a QueryResult
(define-syntax (compile-from stx)
  (syntax-parse stx
    #:datum-literals (from)
    [(_ (from tbl (cb ...)))
     #'(rt:compose-query (rt:from tbl) (rt:select 'cb ...))]))

;; Compile a `clause` syntax into an expression that evaluates to a (-> QueryResult QueryResult)
(define-syntax (compile-clause stx)
  (syntax-parse stx
    #:datum-literals (select where join limit)
    [(_ (select name ...))
     #'(rt:select 'name ...)]
    [(_ (where condition))
     #'(sugar:where condition)]
    [(_ (join tbl^ (cb ...) col1 col2))
     #'(rt:join (compile-from (from tbl^ (cb ...))) 'col1 'col2)]
    [(_ (limit n))
     #'(rt:limit n)]
    [(_ (order-by col <))
     #'(rt:order-by 'col <)]))


(begin-for-syntax
  (define col-reference-compiler
    (make-variable-like-reference-compiler (syntax-parser [x:id #'(sugar:col x)]))))


;; Predicate pushdown optimization
(begin-for-syntax
  ;; (ListOf ClauseSyntax) -> (ListOf ClauseSyntax)
  ;; Reorder the clauses to place `where` clauses before joins that introduce unrelated columns.
  (define (predicate-pushdown cs)
    (reverse (for/fold ([reversed (list)])
                       ([c (syntax->list cs)])
               (if (where-clause? c)
                   (push-down c reversed)
                   (cons c reversed)))))

  ;; ClauseSyntax -> Boolean
  (define (where-clause? c)
    (syntax-parse c
      #:datum-literals (where)
      [(where . _) #t]
      [_ #f]))

  ;; ClauseSyntax, (ListOf ClauseSyntax) -> (ListOf ClauseSyntax)
  ;; Add the clause c at the appropriate place in the reversed list of clauses,
  ;; pushing it beyond any clauses that do not bind columns referenced in the where.
  (define (push-down c reversed)
    (match reversed
      [(list) (list c)]
      [(cons c^ reversed^)
       (if (can-push-down? c c^)
           (cons c^ (push-down c reversed^))
           (cons c reversed))]))

  ;; ClauseSyntax ClauseSyntax -> Boolean
  ;; Can we move where-clause before other-clause?
  (define (can-push-down? where-clause other-clause)
    (define bound-vars (get-clause-bound-vars other-clause))
    (define referenced-vars (get-where-referenced-vars where-clause))
    (not (for/or ([referenced-var referenced-vars])
           (symbol-set-member? bound-vars referenced-var))))

  ;; ClauseSyntax -> SymbolSet
  (define (get-clause-bound-vars c)
    (syntax-parse c
      #:datum-literals (select derived where join order-by distinct limit aggregate)
      [(join _ (c ...) _ _) (apply immutable-symbol-set (attribute c))]
      [_ (immutable-symbol-set)]))

  ;; Syntax -> (Listof Identifier)
  (define (get-where-referenced-vars c)
    (syntax-parse c
      #:datum-literals (where)
      [(where condition) (get-racket-referenced-identifiers (col) #'condition)])))

