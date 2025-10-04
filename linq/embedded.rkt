#lang racket

(require racket/hash
         "tables.rkt")

(provide (all-defined-out) load-table)

;; A Table is a (Listof Row)
;; A Row is a (HashOf ColName Any)
;; A ColName is a Symbol

;; A Clause is a ((StreamOf Row) -> (StreamOf Row))


;; (StreamOf Row), Clause ... -> Table
(define (query query-result . clauses)
  (stream->list
   (apply-clauses query-result clauses)))

;; (StreamOf Row) Clause ... -> (StreamOf Row)
;; Execute a query by applying each clause in turn to the
;; stream produced by the initial `from` clause.
(define (apply-clauses query-result clauses)
  (for/fold ([qr query-result])
            ([clause clauses])
    (clause qr)))

;; Table -> (StreamOf Row)
;; Create a stream from a table of data.
(define (from table)
  (sequence->stream table))

;; (Row -> Boolean) -> Clause
;; Filter, keeping only rows that match the predicate.
(define (where keep?)
  (lambda (qr)
    (for/stream ([row qr]
                 #:when (keep? row))
      row)))

;; ColName ... -> Clause
;; Keep only the columns specified.
(define (select . col-names)
  (lambda (qr)
    (for/stream ([row qr])
      (for/hash ([col col-names])
        (values col (hash-ref row col))))))

;; QueryResult, (Row -> Boolean) -> Clause
;; An inner join of the running query-result with the argument
;; query-result.
(define (join qr2 col1 col2)
  (lambda (qr1)
    (for*/stream ([row1 qr1]
                  [row2 qr2]
                  #:when (equal? (hash-ref row1 col1) (hash-ref row2 col2)))
      (hash-union row1 row2))))

;; QueryResult, Integer -> Clause
;; Retain only the first `n` rows of the query result.
(define (limit n)
  (lambda (qr)
    (stream-take qr n)))






(define (compose-clauses . clauses)
  (lambda (qr)
    (apply-clauses qr clauses)))

;; Wrapper for `query` that prints the result and execution time.
(define (query/print query-result . clauses)
  (pretty-display
   (time
    (apply query query-result clauses))))

