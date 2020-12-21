#lang racket

(require rnrs/mutable-pairs-6)

(define ticket-windows-count 8)

(define queue-length 24)

(define (make-queue)
 (define p (mcons '() '() ))
 (mcons p p))


(define (fill-queue q len val)
  (cond
    ((> len 0)
        (push q val)
        (fill-queue q (- len 1) val))))


(define (null-queue? q)
 (and
  (eq? (front q) (rear q)) (eq? (mcar (front q)) '() )))


(define (front q)
 (mcar q)) 


(define (rear q)
 (mcdr q))


(define (push q e)
 (define p (mcons e '()))
 (if (null-queue? q)
   (begin
     (set-mcar! q p)
     (set-mcdr! q p))
   (begin
     (set-mcdr! (rear q) p)
     (set-mcdr! q p))))


(define (pop q)
 (define x 0)
 (if (null-queue? q)
  'Empty    
  (if (and (eq? (front q) (rear q))  (eq? '() (mcdr (front q)))   )
   (begin
    (set! x (mcar (front q)))
    (set-mcar! (front q) '() )
    x ) 
   (begin
     (set! x (mcar (front q)))
     (set-mcar! q (mcdr (front q)) )
     x))))


(define (update-windows-state w)
   (define (decrem-time w ind)
     (cond ((< ind (vector-length w))
              (vector-set! w ind (- (vector-ref w ind) 1))
              (cond
                ((= 0 (vector-ref w ind))
                 (display "Window #")(display ind)(display " has been released!")(newline)))
     (decrem-time w (+ 1 ind))
            )))
  (decrem-time w 0))


(define (set-up-new-clients w q)
   (define (iter w q ind)
      (cond
        ((and (< ind (vector-length w)) (= 0 (vector-ref w ind)))
           (vector-set! w ind (+ 2 (random 10)))
           (display "Window #")(display ind)(display " was acquired for ")
           (display (vector-ref w ind))(display " ticks")(newline)
           (pop q)
           (iter w q (+ 1 ind)))
        ((< ind (vector-length w))
           (iter w q (+ 1 ind)))))
   (iter w q 0))


(define (terminal-tick windows q tick-num)
   (display "time #")(display tick-num)(newline)
   (update-windows-state windows)
   (set-up-new-clients windows q)

   (cond
     ((null-queue? q) tick-num)
     (else
       (terminal-tick windows q (+ 1 tick-num)))))


(define (calculate-queue-compl-time q)
  (let ((windows (make-vector ticket-windows-count 1)))
     ;(display windows)
     ;(newline)
     (fill-queue q queue-length 1)
     (display (terminal-tick windows q 0))))

(let ((q (make-queue)))
  (display q)
  (newline)
  (push q 1)
  (display q)
  (newline)
  (push q 1)
  (display q)
  (newline)
  
  )

(calculate-queue-compl-time (make-queue))

;(display ticket-windows)