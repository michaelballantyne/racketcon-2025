#lang racket

(provide (all-defined-out) load-table)

(require (for-syntax racket/match
                     syntax/parse)
         syntax-spec-v3
         "tables.rkt"
         (prefix-in rt: "embedded.rkt")
         (prefix-in sugar: "sugar.rkt"))

(module+ test (require rackunit))

(begin-for-syntax
  (define col-reference-compiler
    (make-variable-like-reference-compiler (syntax-parser [x:id #'(sugar:col x)]))))

(syntax-spec
  (binding-class col
    #:description "column name"
    #:reference-compiler col-reference-compiler)

  (host-interface/expression
    (query f:from-clause c:clause ...)
    #:binding (nest f c ... [])

    (check-query #'(query f c ...))
    #'(rt:query (compile-from f)
                (compile-clause c)
                ...))
  
  (nonterminal/nesting from-clause (nested)
    (from tbl:racket-expr (c:col ...))
    #:binding (scope (bind c) ... nested))
  
  (nonterminal/nesting clause (nested)
    (select q:col ...)
    (where condition:racket-expr)
    (join tbl:racket-expr (c:col ...) col1:col col2:col)
    #:binding (scope (bind c) ... col2 nested)
    (limit n:racket-expr)))

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
     #'(rt:limit n)]))

;; Column availability check
(begin-for-syntax
  ;; QuerySyntax -> Void or error
  ;;
  ;; Raise an error when part of a query refers to a column binding, but the column will not in fact
  ;; be part of the row at that part of the query at runtime.
  ;;
  ;; Assumes there are no nested uses of `query` in a query.
  (define (check-query stx)
    (syntax-parse stx
      [(_ f c ...)
       (for/fold ([still-bound-ids (from-clause-bound-vars #'f)])
                 ([c (attribute c)])
         (check-clause c still-bound-ids))
       (void)]))

  ;; FromClauseSyntax -> ImmutableSymbolSet
  ;;
  ;; Compute the set of names bound in a `from` clause.
  (define (from-clause-bound-vars stx)
    (syntax-parse stx
      #:datum-literals (from)
      [(from table-expr (name:id ...))
       (apply immutable-symbol-set (attribute name))]))

  ;; ClauseSyntax, ImmutableSymbolSet -> ImmutableSymbolSet or error
  ;;
  ;; Check that the clause only refers to the `still-bound-ids` that were bound
  ;; in earlier clauses, and return an updated set representing the columns that should
  ;; remain accessible in subsequent clauses.
  (define (check-clause stx still-bound-ids)
    (syntax-parse stx
      #:datum-literals (select where join limit)
      [(select name ...)
       (for ([name (attribute name)])
         (check-id name still-bound-ids))
       (apply immutable-symbol-set (attribute name))]
      [(where body)
       (check-row-expr #'body still-bound-ids)
       still-bound-ids]
      [(join tbl (name:id ...) col1 col2)
       (check-non-row-expr #'tbl)
       (define names (apply immutable-symbol-set (attribute name)))
       (check-id #'col1 still-bound-ids)
       (check-id #'col2 names)
       (symbol-set-union still-bound-ids names)]
      [(limit n)
       (check-non-row-expr #'n)
       still-bound-ids]))

  ;; Identifier, ImmutableSymbolSet -> Void or error
  ;; Raise an error if the identifier is not in the set.
  (define (check-id id still-bound-ids)
    (unless (symbol-set-member? still-bound-ids id)
      (raise-syntax-error #f "column not available here" id)))

  ;; RacketExprSyntax, ImmutableSymbolSet -> Void or error
  ;; Raise an error if the Racket expression refers to columns that are not in the set.
  (define (check-row-expr stx still-bound-ids)
    (define referenced-ids (get-racket-referenced-identifiers [col] stx))
    (for ([id referenced-ids])
      (check-id id still-bound-ids)))

  ;; RacketExprSyntax -> Void or error
  ;; Raise an error if the Racket expression refers any columns.
  (define (check-non-row-expr stx)
    (define referenced-ids (get-racket-referenced-identifiers [col] stx))
    (for ([id referenced-ids])
      (raise-syntax-error #f "column not available here" id))))



(module+ test
  (define articles
    (list (hash 'id 0
                'title "how to climb a rock wall"
                'body "just go up"
                'author-id 0)
          (hash 'id 1
                'title "why I don't like Racket"
                'body "too many parentheses"
                'author-id 0)
          (hash 'id 2
                'title "why I like Racket"
                'body "all you need is parentheses"
                'author-id 1)))
  (define users
    (list (hash 'id 0
                'name "haskell-fan")
          (hash 'id 1
                'name "racket-enjoyer")))
  
  (check-equal?
   (query (from articles (author-id title))
          (join users (id name)
                author-id id)
          (select title name))
   (list (hash 'title "how to climb a rock wall" 'name "haskell-fan")
         (hash 'title "why I don't like Racket" 'name "haskell-fan")
         (hash 'title "why I like Racket" 'name "racket-enjoyer")))
  
  (check-equal?
   (query (from articles (author-id title))
          (join users (id name)
                author-id id)
          (select name)
          (limit 1))
   (list (hash 'name "haskell-fan")))
)