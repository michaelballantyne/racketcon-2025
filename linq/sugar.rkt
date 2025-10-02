#lang racket

(provide where col
         (except-out (all-from-out "embedded.rkt") rt:where)
         load-table)

(require (rename-in "embedded.rkt"
                    [where rt:where])
         "tables.rkt"
         racket/stxparam
         (for-syntax syntax/parse))

;; Example:
#;(where (equal? (col codeshare) "Y"))
;; Expands to:
#;(rt:where
 (lambda (row)
   (equal? (hash-ref row 'codeshare)
           "Y")))

(begin-for-syntax
  (define (col-transformer row-id)
    (lambda (stx)
      (define/syntax-parse row row-id)
      (syntax-parse stx
        [(_ column:id)
         #'(hash-ref row 'column)]))))

;; Example:
#;(where (equal? (col codeshare) "Y"))
;; Expands to:
#;(rt:where
 (lambda (row)
   (syntax-parameterize ([col (col-transformer #'row)])
     (equal? (col codeshare) "Y"))))
;; Another example:
#;(where (> (col count) 3))
;; Expands to:
#;(rt:where
 (lambda (row)
   (syntax-parameterize ([col (col-transformer #'row)])
     (> (col count) 3))))
(define-syntax where
  (lambda (stx)
    (syntax-parse stx
      [(_ pred:expr)
       #'(rt:where
          (lambda (row)
            (syntax-parameterize ([col (col-transformer #'row)])
              pred)))])))


(define-syntax-parameter col
  (lambda (stx)
    (raise-syntax-error #f "must only be used in where or derived" stx)))