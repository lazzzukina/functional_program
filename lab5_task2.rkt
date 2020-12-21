;селектори дійсної та уявної частин компл числа
(define (complex-real-part z) (car z))
(define (complex-imag-part z) (cdr z))
(define (square x) (* x x))

;тригонометрична форма компл числа
(define (complex-mag z)
  (sqrt (+ (square (complex-real-part z)) (square (complex-imag-part z)))))

(define (complex-ang z)
  (atan (complex-imag-part z) (complex-real-part z)))

(define (make-complex-real-imag real imag)
  (cons real imag))

(define (make-complex-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))


; додавання та віднімання компл чисел в алгебраїчній формі
(define (add-complex z1 z2)
 (make-complex-real-imag (+ (complex-real-part z1) (complex-real-part z2))
    (+ (complex-imag-part z1) (complex-imag-part z2))))

(define (mul-complex z1 z2)
   (cond
     ((and (real? z1) (pair? z2))
        (make-complex-real-imag (* (complex-real-part z2) z1) (* (complex-imag-part z2) z1)))
     ((and (real? z2) (pair? z1))
        (make-complex-real-imag (* (complex-real-part z1) z2) (* (complex-imag-part z1) z2)))
     (else
        (make-complex-mag-ang (* (complex-mag z1) (complex-mag z2))
                           (+ (complex-ang z1) (complex-ang z2))))))

(define (div-complex z1 z2)
        (make-complex-mag-ang (/ (complex-mag z1) (complex-mag z2))
                           (- (complex-ang z1) (complex-ang z2))))




(define (solve-complex-eq x-coef y-coef res)
  (let ((x-coefo x-coef) (y-coefo y-coef) (reso res))
  (cond
    ((complex? x-coef)
      (set! x-coef (make-complex-real-imag (real-part x-coef) (imag-part x-coef))))
    ((real? x-coef)
      (set! x-coef (make-complex-real-imag x-coef 0))))
  (cond
    ((complex? y-coef)
      (set! y-coef (make-complex-real-imag (real-part y-coef) (imag-part y-coef))))
    ((real? y-coef)
      (set! y-coef (make-complex-real-imag y-coef 0))))
  (cond
    ((complex? res)
      (set!  res (make-complex-real-imag (real-part res) (imag-part res))))
    ((real? res)
      (set! res (make-complex-real-imag res 0))))

  (let ((y 5/11))
    (let ((x (div-complex (add-complex
                            (make-complex-real-imag (- (complex-real-part (mul-complex y-coef y)))
                                                   (- (complex-imag-part (mul-complex y-coef y))))
                            res)
                          x-coef)))
      (cons (/ (+ (- (* y-coefo y)) reso) x-coefo) y)))))

(solve-complex-eq 1+2i 3-5i 1-3i)