#lang racket

(require "../linq/sugar.rkt")

;; Columns: airline,route-airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "openflights/airlines.csv"))

(query
 (from routes)
 (join (from airlines)
       'route-airline-id 'airline-id)
 (where (equal? (col codeshare) "Y"))
 (where (equal? (col name) "American Airlines"))
 (select 'source-airport 'destination-airport)
 (limit 3))


