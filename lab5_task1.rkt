(define (numer x) (car x))
(define (denom x) (cdr x))
(define (make-rat n d) (cons n d))
(define (print-rat x) 
  (display (numer x))
  (display "/")
  (display (denom x)))


; вычисляет наименьшее общее кратное всех чисел в
; списке рациональных чисел
(define (rat-lst-lcm rat-lst)

  (define (lcm-iter rat-lst common)
    (cond
      ((null? rat-lst) common)
      (else
        (lcm-iter (cdr rat-lst) (lcm common (denom (car rat-lst)))))))

  (cond
    ((null? rat-lst) 0)
    ((not (list? rat-lst)) "ожидается список рациональных чисел..")
    ((= 1 (length rat-lst)) (denom (car rat-lst)))
    (else (lcm-iter rat-lst 1))))


; создаем новый список рациональных чисел с общим знаменателем
(define (rat-list-to-common-denom rat-lst)
 
  (define (create-new-rat-list old-rat-lst common-denom)
    (cond
      ((null? old-rat-lst) '())
      (else
        (cons
          (make-rat (* (numer (car old-rat-lst)) (/ common-denom (denom (car old-rat-lst)))) common-denom)
          (create-new-rat-list (cdr old-rat-lst) common-denom)))))
  ; общий знаменатель - это наименьшее общее кратное
  (let ((common-denom (rat-lst-lcm rat-lst)))
    (create-new-rat-list rat-lst common-denom)))

(define (filter predicate lst)
  (if (null? lst) '()
      (if (predicate (car lst))
          (cons (car lst) (filter predicate (cdr lst)))
          (filter predicate (cdr lst)))))
(define (remove-if predicate lst)
  (filter (lambda (x) (not (predicate x))) lst))


(define (sort-lst lst proc)
  (cond
    ((null? lst) '())
    (else
      (let ((should-be-first (proc lst (car lst))))
        (cons should-be-first (sort-lst (remove-if (lambda (x) (equal? x should-be-first)) lst) proc))))))


(define find-max-rat-by-numer
  (lambda (lst max-rat)
         (cond
           ((null? lst) max-rat)
           ((> (numer (car lst)) (numer max-rat))
             (find-max-rat-by-numer (cdr lst) (car lst)))
           (else
             (find-max-rat-by-numer (cdr lst) max-rat)))))

(define find-min-rat-by-numer
  (lambda (lst max-rat)
         (cond
           ((null? lst) max-rat)
           ((< (numer (car lst)) (numer max-rat))
             (find-min-rat-by-numer (cdr lst) (car lst)))
           (else
             (find-min-rat-by-numer (cdr lst) max-rat)))))


(let ((lst (list (make-rat 7 20) (make-rat 1 5) (make-rat 3 10))))
  (display "созданный список ")
  (map
    (lambda (rat) (print-rat rat) (display " "))
    lst)
  (newline)
  (set! lst (rat-list-to-common-denom lst))
  (display "с общим знаменателем ")
  (map
    (lambda (rat) (print-rat rat) (display " "))
    lst)
  (newline)
  (set! lst (sort-lst lst find-max-rat-by-numer))
  (display " сортировка по убыванию ")
  (map
    (lambda (rat) (print-rat rat) (display " "))
    lst)
  (newline)
  (set! lst (sort-lst lst find-min-rat-by-numer))
  (display "сортировка по возрастанию  ")
  (map
    (lambda (rat) (print-rat rat) (display " "))
    lst)
  (newline))