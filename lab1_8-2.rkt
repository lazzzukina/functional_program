(define (nums k s)
  (if (> k 1)
      s
      (if (EVEN? k)
          (+ s (nums (* k 1) s) s))))
(nums 3 15)