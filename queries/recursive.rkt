#lang racket

(require "../linq/recursive.rkt"
         (prefix-in rt: "../linq/embedded.rkt"))

;; Columns: airline,route-airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "../openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "../openflights/airlines.csv"))


;; An improved query syntax:

(query/print
 (from routes (source-airport destination-airport
               route-airline-id codeshare))
 (join airlines (name airline-id)
       route-airline-id airline-id)
 (where (equal? codeshare "Y"))
 (where (equal? name "American Airlines"))
 (select source-airport destination-airport)
 (limit 3))

;; DrRacket understands the binding structure:
;;  column bindings are visible in the clauses
;;  that follow the `from` or `join` that bind them.



#;
;; Misuse leads to syntax errors:
(query/print
 (select source-airport destination-airport)
 (from routes (source-airport destination-airport
               route-airline-id codeshare))
 (join airlines (name airline-id)
       route-airline-id airline-id)
 (where (equal? codeshare "Y"))
 (where (equal? name "American Airlines"))
 (limit 3))

