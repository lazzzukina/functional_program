
(define (my-ln-func x acc)
  (cond
    ((and (> x -2) (<= x 0)) (* (recurrent-sum-ln (* 2 x) acc) (recurrent-sum-ln x acc)))
    ((and (> x 0)  (<= x 1)) (- (recurrent-sum-ln (/ x 2) acc) 1))))

(define (math-ln-func x)
  (cond
    ((and (> x -2) (<= x 0)) (* (log (* 2 x)) (log x)))
    ((and (> x 0)  (<= x 1)) (- (log (/ x 2)) 1))))


(define (recurrent-sum-ln x acc)
  (let ((series-sum (- x 1)))
    (calc-reccurent-sum-to-specified-accuracy series-sum next-reccurent-ln-elem (cons series-sum 1) x acc)))


(define (next-reccurent-ln-elem prev-elem x)
  (let ((d (cdr prev-elem)))
    (let ((b (if (even? d) (abs (car prev-elem)) (car prev-elem)))) 
      (cons (* (expt -1 d) b (- x 1)) (+ d 1)))))

(define (calc-reccurent-sum-to-specified-accuracy sum-var next-elem-generator prev-elem x acc)
  (let ((next-as-pair (next-elem-generator prev-elem x)))
    (let ((next (/ (car next-as-pair) (cdr next-as-pair))))
      (if (< acc (abs next))
        (calc-reccurent-sum-to-specified-accuracy (+ sum-var next) next-elem-generator next-as-pair x acc)
        (exact->inexact sum-var)))))


(define (main)
  (let ((accuracy 0.000001))
    (calc-task-iter 0.1 accuracy)
    (calc-task-iter 0.2 accuracy)
    (calc-task-iter 0.3 accuracy)
    (calc-task-iter 0.4 accuracy)
    (calc-task-iter 0.5 accuracy)
    (calc-task-iter 0.6 accuracy)
    (calc-task-iter 0.7 accuracy)
    (calc-task-iter 0.8 accuracy)
    (calc-task-iter 0.9 accuracy)
    ))

(define (calc-task-iter current acc-current)
  (newline)
  (display "х: ")
  (display current)
  (display " (точность): ")
  (display acc-current)
  (display " страндартное: ")
  (display (math-ln-func current))
  (display " рекурентное: ")
  (display (my-ln-func current acc-current))
  (display " производная: ")
  (display (abs (- (my-ln-func current acc-current) (math-ln-func current))))
  (newline))
(main)