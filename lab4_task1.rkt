

(define (create-triangulars-list count)
  (define (create-triangulars-iter curr-index prev-value total-count)
    (let ((curr-value (+ prev-value curr-index))) 
      (if (= curr-index total-count) (cons curr-value '())
        (cons curr-value
              (create-triangulars-iter (+ 1 curr-index) curr-value total-count)))))
  (create-triangulars-iter 1 0 count))


(define (filter predicate lst)
  (if (null? lst) '()
      (if (predicate (car lst))
          (cons (car lst) (filter predicate (cdr lst)))
          (filter predicate (cdr lst)))))


(define (delete-if predicate lst)
  (filter (lambda (x) (not (predicate x))) lst))



(define (count-full-squares-in-neighbours lst)
  (cond
    ((null? lst) 0)
    ((not (null? (cdr lst)))
      (let ((sum (+ (car lst) (cadr lst))))
        (if (= (sqrt sum) (inexact->exact (round (sqrt sum))))
          (+ 1 (count-full-squares-in-neighbours (cdr lst)))
          (count-full-squares-in-neighbours (cdr lst)))))
    (else (count-full-squares-in-neighbours (cdr lst)))))


(display (create-triangulars-list 12))
(newline)
(display (delete-if (lambda (x) (if (null? x) #f (= 0 (remainder x 5)))) (create-triangulars-list 12)))
(newline)






(display "3")