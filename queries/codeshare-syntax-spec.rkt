#lang racket

(require "../linq/syntax-spec.rkt")

;; Columns: airline,route-airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "../openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "../openflights/airlines.csv"))

(query
 (from routes (codeshare source-airport destination-airport route-airline-id))
 (join airlines (name airline-id)
       route-airline-id airline-id)
 (where (equal? codeshare "Y"))
 (where (equal? name "American Airlines"))
 (select source-airport destination-airport)
 (limit 3))


