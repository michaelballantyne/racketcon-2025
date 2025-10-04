#lang racket

(provide query query/print load-table)

(require (for-syntax racket/match
                     syntax/parse
                     racket/pretty
                     syntax/id-set
                     syntax/transformer
                     ee-lib/private/flip-intro-scope)
         "tables.rkt"
         (prefix-in rt: "embedded.rkt")
         (prefix-in sugar: "sugar.rkt"))

(define-syntax-rule
  (query/print f c ...)
  (pretty-display (time (query f c ...))))

(define-syntax query
  (syntax-parser
    [(query f c ...)

     (define/syntax-parse (_ f^ c^ ...) (expand-query #'(query f c ...)))

     (define/syntax-parse (c^^ ...) (predicate-pushdown #'(c^ ...)))

     #'(rt:query/print (compile-from f^)
                 (compile-clause c^^)
                 ...)]))

;; DSL expander
(begin-for-syntax
  (define (expand-query stx)
    (syntax-parse stx
      #:datum-literals (query)
      [(query f cl ...)
       (define-values (f^ ctx nested) (expand-from #'f #'(cl ...)))
       (define/syntax-parse (cl^ ...) (expand-clauses nested ctx))
       #`(query #,f^ cl^ ...)]))

  (define (expand-clauses stx ctx)
    (syntax-parse stx
      [() #'()]
      [(cl cl* ...)
       (define-values (cl^ ctx^ nested) (expand-clause #'cl ctx #'(cl* ...)))
       (define/syntax-parse (cl*^ ...) (expand-clauses nested ctx^))
       #`(#,cl^ cl*^ ...)]))

  (define (expand-from stx nested)
    (syntax-parse stx
      #:datum-literals (from)
      [(from tbl (c ...))
       (define ctx (syntax-local-make-definition-context))
       (define/syntax-parse (c^ ...)
         (for/list ([c (attribute c)])
           (car (syntax-local-bind-syntaxes
                  (list (internal-definition-context-add-scopes ctx c))
                  #`(column-binding-rep '#,c)
                  ctx))))
       (define nested^ (internal-definition-context-add-scopes ctx nested))
       (values #'(from tbl (c^ ...)) ctx nested^)]))
      
  (define (expand-clause stx ctx nested)
    (syntax-parse stx
      #:datum-literals (select where join limit)
      [(select name ...)
       (for ([name (attribute name)])
         (check-column-reference name ctx))
       (values #'(select name ...) ctx nested)]
      [(where condition)
       (define/syntax-parse condition^ (annotate-environment #'condition ctx))
       (values #'(where condition^) ctx nested)]
      [(join tbl^ (c ...) col1 col2)
       (check-column-reference #'col1 ctx)
       (define ctx^ (syntax-local-make-definition-context ctx))
       (define/syntax-parse (c^ ...)
         (for/list ([c (attribute c)])
           (car (syntax-local-bind-syntaxes
                  (list (internal-definition-context-add-scopes ctx^ c))
                  #`(column-binding-rep '#,c)
                  ctx^))))
       (define/syntax-parse col2^ (internal-definition-context-add-scopes ctx^ #'col2))
       (check-column-reference
         #'col2^
         ctx^)
       (define nested^ (internal-definition-context-add-scopes ctx^ nested))
       (values #'(join tbl^ (c^ ...) col1 col2^) ctx^ nested^)]
      [(limit n)
       (values #'(limit n) ctx nested)]))

  (define (check-column-reference name ctx)
    (when (not (column-binding-rep? (syntax-local-value name (lambda () #f) ctx)))
      (raise-syntax-error #f "not a column available here" name))))

;; Reference binding representation and transformer
(begin-for-syntax
  (define current-referenced-vars (make-parameter #f))
  (define (make-column-reference-transformer name)
    (make-variable-like-transformer
      (lambda (id)
        (define refs (current-referenced-vars))
        (if refs
          (begin
            (free-id-set-add! refs id)
            #'(void))
          #`(sugar:col #,name)))))

  (struct column-binding-rep (name)
    #:property prop:set!-transformer
    (lambda (rep stx)
      ((make-column-reference-transformer (column-binding-rep-name rep))
       stx))))


;; Compile a `from` syntax into an expression that evaluates to a QueryResult
(define-syntax (compile-from stx)
  (syntax-parse stx
    #:datum-literals (from)
    [(_ (from tbl (cb ...)))
     (define/syntax-parse (cb-q ...) (map bind-quote (attribute cb)))
     #'(rt:compose-query (rt:from tbl) (rt:select cb-q ...))]))

;; Compile a `clause` syntax into an expression that evaluates to a (-> QueryResult QueryResult)
(define-syntax (compile-clause stx)
  (syntax-parse stx
    #:datum-literals (select where join limit)
    [(_ (select name ...))
     (define/syntax-parse (name-q ...) (map ref-quote (attribute name)))
     #'(rt:select name-q ...)]
    [(_ (where condition))
     #'(sugar:where condition)]
    [(_ (join tbl^ (cb ...) col1 col2))
     (define/syntax-parse col1-q (ref-quote #'col1))
     (define/syntax-parse col2-q (ref-quote #'col2))
     #'(rt:join (compile-from (from tbl^ (cb ...))) col1-q col2-q)]
    [(_ (limit n))
     #'(rt:limit n)]))

;; Helpers for recording binding information for the IDE
(begin-for-syntax
  (define (ref-quote id)
    (syntax-property #`'#,id 'disappeared-use (list (flip-intro-scope id))))
  (define (bind-quote id)
    (syntax-property #`'#,id 'disappeared-binding (list (flip-intro-scope id)))))


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
           (free-id-set-member? bound-vars referenced-var))))

  ;; ClauseSyntax -> SymbolSet
  (define (get-clause-bound-vars c)
    (syntax-parse c
      #:datum-literals (select derived where join order-by distinct limit aggregate)
      [(join _ (c ...) _ _) (immutable-free-id-set (attribute c))]
      [_ (immutable-free-id-set)]))

  ;; Syntax -> (Listof Identifier)
  (define (get-where-referenced-vars cl)
    (syntax-parse cl
      [(where (~and host-expr (_ condition)))
       (define ctx (syntax-property #'host-expr 'environment))
       (parameterize ([current-referenced-vars (mutable-free-id-set)])
         (local-expand #'condition 'expression '() ctx)
         (free-id-set->list (current-referenced-vars)))])))


;; Helpers for resuming expansion of host subexpressions with a context where
;; both DSL names and names from the compilation are available.
(begin-for-syntax
  (define (annotate-environment racket-expr ctx)
    (syntax-property
     #`(#%host-expression #,racket-expr)
     'environment
     ctx)))

(define-syntax #%host-expression
  (syntax-parser
    [(_ racket-expr)
     (define ctx (syntax-property this-syntax 'environment))
     (local-expand #'racket-expr 'expression '() ctx)]))