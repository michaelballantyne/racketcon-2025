#lang racket

(require "../linq/embedded.rkt")

;; Columns: airline,route-airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "../openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "../openflights/airlines.csv"))

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


