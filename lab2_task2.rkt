
(define (calc-periodic-fraction val is-even acc iter)
  (let ((next (+ (if is-even 2 1) (/ 1 val))))
    (display (exact->inexact next))
    (newline)
    (cond
      ((> (abs (- val next)) acc)
       (calc-periodic-fraction next (not is-even) acc (+ iter 1)))
      (else
       (display iter)
       (+ 1 (/ 1 next))))))

(define (calc-fraction-2 iter-count)
  (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t iter-count)))))

(define (calc-fraction acc)
  (calc-fraction-high-iter 0 acc))

(define (calc-fraction-high-iter iter-count acc)
  (let ((prev (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t iter-count)))))
        (next (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t (+ iter-count 1)))))))
  (if (< (abs (- next prev)) acc)
      next
      (calc-fraction-high-iter (+ iter-count 1) acc))))

(define (calc-fraction-iter is-even iter-count)
  (let ((num (if is-even 2 1)))
  (cond ((equal? 0 iter-count) (/ 1 num))
        (else
         (/ 1 (+ num (calc-fraction-iter (not is-even) (- iter-count 1))))))))


(calc-fraction 0.001)

;(calc-fraction-2 9)
;(- (calc-fraction-2 11) (calc-fraction-2 10))
;(- (calc-fraction-2 2001) (calc-fraction-2 2000))
