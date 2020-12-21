
(define (appends list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append (cdr list1) list2))))
(append '((1 10.11) read) '((2 12.11)learn) '((3 13.11)documents) '((4 14.11)jump))
(newline)


(define (delete-n list n)
  (if (= n 0)
      (cdr list)
      (cons (car list) (delete-n (cdr list) (- n 1)))))

(define (insert-n list item n)
  (if (= n 0)
      (cons item list)
      (cons (car list) (insert-n (cdr list) item (- n 1)))))

(define (list-nth list n)
  (if (= n 0)
      (car list)
      (list-nth (cdr list) (- n 1))))

(define (replace-nth list n item)
  (if (= n 0)
      (cons item (cdr list))
      (cons (car list) (replace-nth (cdr list) (- n 1) item))))

(define (swap-list-item list m n)
  (let
    ((a (list-nth list m))
     (b (list-nth list n)))
    (replace-nth
      (replace-nth list m b) n a)))



(replace-nth (list 1 2 9 5) 2 3)
(delete-n (list 1 2 3 4) 2)
(insert-n (list 1 2 4 5) 3 2)

(swap-list-item (list 1 2 3 4 5 6) 2 4)

