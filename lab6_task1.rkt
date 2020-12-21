(define (filter predicate lst)
  (cond
     ((null? lst) '())
     ((predicate (car lst))
       (cons (car lst) (filter predicate (cdr lst))))
     (else
       (filter predicate (cdr lst)))))


(define (fill-vector vect lst)

  (define (fill-rec vect lst ind)
        (if (not (null? lst))
            (begin (vector-set! vect ind (car lst))
                   (fill-rec vect (cdr lst) (+ 1 ind)))))

  (cond
    ((not (= (vector-length vect) (length lst))) "Lengths must be equal!")
    ((not (vector? vect)) "As 1st param vector expected!")
    ((not (list? lst)) "As 2nd param list expected!")
    (else
      (fill-rec vect lst 0))))


(define (filter-vec predicate vect)
  (let ((result-lst (filter predicate (vector->list vect))))
    (let ((result (make-vector (length result-lst))))
      (fill-vector result result-lst)
      result
      )))


(define (find-prime-divisors num)
    ; вспомогательная функция для итераций
    (define (find-prime-divisors-iter num divisor)
        (cond 
            ((and (> num 0) (= 0 (remainder num divisor))) 
                (cons divisor
                      (find-prime-divisors-iter (/ num divisor) divisor)))
            ((<= divisor num) 
                (if (= divisor 2) (find-prime-divisors-iter num (+ divisor 1))
                    (find-prime-divisors-iter num (+ divisor 2))))
            (else '())))

    ; вызвать вспомогательную функцию, начиная с простого числа 2
    (find-prime-divisors-iter num 2))


(let ((vec (list->vector (list 1 3 4 7 9 18 13 22 23 6 8 12 11))))
  (display "vector: ")
  (newline)
  (display vec)
  (newline)

  (display "vector with prime numbers: ")
  (newline)
  (display (filter-vec (lambda (x) (= 1 (length (find-prime-divisors x)))) vec)))
