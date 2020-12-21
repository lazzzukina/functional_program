
(define (secant-method func start end acc)
    (let ((xk (- start (/ (* (func start) (- end start)) (- (func end) (func start))))))
      (cond
        ((> acc (abs (- (func xk) (func start)))) xk)
        ((> 0 (* (func xk) (func start)))
          (secant-method func start xk acc))
        (else
          (secant-method func xk end acc)))))


(define (simple-iterations-method func start end acc)
  (if (> start end) "нет корня на этом интервале:(")
  (let ((next-x (func start)))
    (if (< acc (abs (- next-x start)))
      (simple-iterations-method func next-x end acc)
      next-x)))

(define (fu)
  (let ((start (- 2)) (end 2) (accuracy 0.0001))
     (display "метод итераций _ ")
     (display (simple-iterations-method (lambda (x) (asin (/ (- (* x x)) 4))) start end accuracy))
     (newline)
     (display "___________________________________________")
     (newline)
     (display "метод хорд _ ")
     (display (secant-method (lambda (x) (+ (* x x) (* 4 (sin x)))) start end accuracy))))

(fu)
