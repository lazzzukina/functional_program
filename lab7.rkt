#lang racket

; читает число из пользовательского ввода
(define (enter-num)
  (display "введите количество строк для записи в файл: ")
  (let ((num (string->number (read-line))))
    (if (integer? num) num
        (enter-num))))


; прочитать n строк и вернуть их в виде списка
(define (read-n-lines n)
  (cond
    ((<= n 0) '())
    (else
      (cons (read-line) (read-n-lines (- n 1))))))

; записывает список строк в файл с заданным именем файла
(define (write-lines-to-file list-of-lines file-name)
  
  (let ((file (open-output-file file-name)))
    (map
       (lambda (str) (write str file) (newline file))
       list-of-lines)
    (close-output-port file)))


; возвращает все строки из файла в виде списка
(define (read-all-lines file)
  (let ((line (read file)))
    (cond
      ((eof-object? line) '())
      (else
        (cons line (read-all-lines file))))))


; возвращает список строк, прочитанных из файла с заданным именем файла
(define (read-lines-from-file file-name)
  
  (let ((file (open-input-file file-name)))
    (let ((lines (read-all-lines file)))
      (close-input-port file)
      lines)))



(write-lines-to-file (read-n-lines (enter-num)) "f.txt")

(display "в файл успешно записано: ")
(newline)
(let ((lines (read-lines-from-file "f.txt")))
  (map (lambda (str) (write str) (newline)) lines)
  (newline)

  (display "введите слово, которое нужно добавить в начало каждой строки: ")
  (let ((word (read-line)))
    (write-lines-to-file
      (map (lambda (str) (string-append word str)) lines)
      "f2.txt")))
