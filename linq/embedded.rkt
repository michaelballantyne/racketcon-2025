#lang racket

(require racket/hash
         "tables.rkt")

(provide (all-defined-out) load-table)

;; A Table is a (Listof Row)

;; A QueryResult is a
(struct query-result [data])
;; where data is a Stream.
 
;; A ColName is a Symbol
;; A Row is a (HashOf ColName Any)

;; A Clause is a (QueryResult -> QueryResult)

;; QueryResult, Clause ... -> Table
(define (query query-result . clauses)
  (query-result->table
    (apply compose-query query-result clauses)))

;; Compose a query that transforms the initial query-result (usually produced by `from`),
;; applying the transformation defined by each clause in the order they are provided.
(define (compose-query query-result . clauses)
  (for/fold ([qr query-result])
            ([clause clauses])
    (clause qr)))

;; Table -> QueryResult
;; Create a QueryResult from a table of data.
(define (from table)
  (query-result table))

;; (Row -> Boolean) -> Clause
;; Filter, keeping only rows that match the predicate.
(define (where keep?)
  (lambda (qr)
    (query-result
      (for/stream ([row (query-result-data qr)]
                 #:when (keep? row))
        row))))

;; ColName ... -> Clause
;; Keep only the columns specified.
(define (select . col-names)
  (lambda (qr)
   (query-result
     (for/stream ([row (query-result-data qr)])
       (for/hash ([col col-names])
         (values col (hash-ref row col)))))))

;; QueryResult, (Row -> Boolean) -> Clause
;; An inner join of the running query-result with the argument
;; query-result.
(define (join qr2 col1 col2)
  (lambda (qr1)
    (query-result
      (for*/stream ([row1 (query-result-data qr1)]
                    [row2 (query-result-data qr2)]
                    #:when (equal? (hash-ref row1 col1) (hash-ref row2 col2)))
        (hash-union row1 row2)))))

;; QueryResult, Integer -> Clause
;; Retain only the first `n` rows of the query result.
(define (limit n)
  (lambda (qr)
    (query-result
      (stream-take (query-result-data qr) n))))


;; QueryResult -> Table
;; Produce a table from the rows of a query result.
(define (query-result->table query-result)
  (stream->list (query-result-data query-result)))

