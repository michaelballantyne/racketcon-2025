#lang racket

(require "../linq/embedded.rkt")

;; Columns: airline,route-airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "../openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "../openflights/airlines.csv"))


;; An SQL or LINQ-like query, expressed as a
;; composition of Racket function calls and data.

#;
(query/print
 (from routes)
 (select 'source-airport 'destination-airport
         'route-airline-id 'codeshare)
 (limit 5))





;; A fancier query with a join and some filters.


#;
(query/print
 (from routes)
 (join (from airlines)
       'route-airline-id 'airline-id)
 (where (lambda (row) (equal? (hash-ref row 'codeshare)
                              "Y")))
 (where (lambda (row) (equal? (hash-ref row 'name)
                              "American Airlines")))
 (select 'source-airport 'destination-airport)
 (limit 3))






;; What if we misuse the syntax?
#;
(query/print
 (select 'source-airport 'destination-airport)
 (from routes)
 (join (from airlines)
       'route-airline-id 'airline-id)
 (where (lambda (row) (equal? (hash-ref row 'codeshare)
                              "Y")))
 (where (lambda (row) (equal? (hash-ref row 'name)
                              "American Airlines")))
 (limit 3))





;; This query could be faster!

#;
(query/print
 (from routes)
 (join (from airlines)
       'route-airline-id 'airline-id)
 (where (lambda (row) (equal? (hash-ref row 'codeshare)
                              "Y")))
 (where (lambda (row) (equal? (hash-ref row 'name)
                              "American Airlines")))
 (select 'source-airport 'destination-airport)
 (limit 3))
;; ->
#;
(query/print
 (from routes)
 ;; Moved this `where` up before the join:
 (where (lambda (row) (equal? (hash-ref row 'codeshare)
                              "Y")))
 (join (from airlines)
       'route-airline-id 'airline-id)
 (where (lambda (row) (equal? (hash-ref row 'name)
                              "American Airlines")))
 (select 'source-airport 'destination-airport)
 (limit 3))
