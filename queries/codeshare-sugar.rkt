#lang racket

(require "../linq/sugar.rkt")

;; Columns: airline,airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "openflights/airlines.csv"))

(query/rows
 (from routes)
 (join (from airlines #:qualify 'airline)
       'route-airline-id 'airline.airline-id)
 (where (equal? (col codeshare) "Y"))
 (where (equal? (col airline.name) "American Airlines"))
 (select 'source-airport 'destination-airport)
 (limit 3))


