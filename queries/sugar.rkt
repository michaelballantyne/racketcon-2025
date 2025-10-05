#lang racket

(require "../linq/sugar.rkt")

;; Columns: airline,route-airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "../openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "../openflights/airlines.csv"))



;; With a little syntactic sugar, our query looks nicer:

(query/print
 (from routes)
 (join (from airlines)
       route-airline-id airline-id)
 (where (equal? (col codeshare) "Y"))
 (where (equal? (col name) "American Airlines"))
 (select source-airport destination-airport)
 (limit 3))


;; After expansion, it'll use the embedding:
#;
(rt:query/print
 (rt:from routes)
 (rt:join (rt:from airlines)
          'route-airline-id 'airline-id)
 (rt:where (lambda (row) (equal? (hash-ref row 'codeshare)
                                 "Y")))
 (rt:where (lambda (row) (equal? (hash-ref row 'name)
                                 "American Airlines")))
 (rt:select 'source-airport 'destination-airport)
 (rt:limit 3))



;; But there is still no grammar check, checked binding, or optimization.

