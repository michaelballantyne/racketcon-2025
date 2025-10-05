#lang racket

(require "../linq/expander.rkt")

;; Columns: airline,route-airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "../openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "../openflights/airlines.csv"))


;; Same syntax, but this one is fast! How?

(query
 (from routes (source-airport destination-airport
               route-airline-id codeshare))
 (join airlines (name airline-id)
       route-airline-id airline-id)
 (where (equal? codeshare "Y"))
 (where (equal? name "American Airlines"))
 (select source-airport destination-airport)
 (limit 3))

;; The compiler applies predicate pushdown.
;;
;; To do so, it must be able to check binding first,
;; and then transform the syntax.
