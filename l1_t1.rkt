(define (en-number)
  (display "Введите число: ")
  (let ((num (read)))
    (if (integer? num) num
        (en-number))))

(define (soldier-combination num)
  (cond
    ((< num 3) 0)
    ((= num 3) 1)
    ((even? num) (* 2 (soldier-combination (/ num 2))))
    ((odd? num) (+ (soldier-combination (quotient num 2))
                   (soldier-combination (+ 1 (quotient num 2)))))))

(define (recursion-depth num)
  (cond
    ((<= num 3) 1)
    ((even? num) (+ 1 (recursion-depth (/ num 2))))
    ((odd? num) (+ 1 (recursion-depth (quotient num 2))
                   (recursion-depth (+ 1 (quotient num 2)))))))

(define (main)
  (let ((num (en-number)))
    (display "Способы создания группы из 3х солдат: ")
    (display (soldier-combination num))
    (newline)
    (display "Глубина рекурсии: ")
    (display (recursion-depth num))))
(main)