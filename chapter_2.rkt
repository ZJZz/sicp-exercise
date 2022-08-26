#lang racket

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


; 2.1
(define (abs x)
  (if (< x 0) (- x)
      x)

  
)

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (> (* n d) 0) (cons (/ n g) (/ d g))
        (cons (- (/ (abs n) g)) (/ (abs d) g))
     )
    )
)

(define one-half (make-rat 2 -4))

;(print-rat one-half)

; 2.2

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
              (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(print-point (make-point 2 4))

;(print-point (make-point 4 6))

;(print-point (midpoint-segment (make-segment (make-point 2 4) (make-point 4 6))))

; 2.3

;(define (calc-preimeter rect) (* 2 (+ (get-length rect) (get-width rect))))
;(define (calc-area rect) (* (get-length rect) (get-width rect)))


;(define (make-rect) ())

; using point and segment ?

; 2.4

(define (cons-custom x y)
  (lambda (m) (m x y)))

(define (car-custom z)
  (z (lambda (p q) p)))

(define (cdr-custom z)
  (z (lambda (p q) q)))

;(cdr-custom (cons-custom 1 2))

; 2.5
  
(define (cons-product a b) (* (exp 2 a) (exp 3 b)))

; 2.6

;(define zero (lambda (f) (lambda (x) x)))
;(define (add-1 n)
;  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)

; 2.17

(define (last-pair l)
  (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34))

; 2.18

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define nil '())

(define (reverse l)
  (if (null? (cdr l))
       l
       (append (reverse (cdr l)) (cons (car l) nil))
  )
)

(reverse (list 1 2 3 4 5))

; [5 4 nil] [3 nil]
; [5, 4, 3, nil] [2, nil]
; [5, 4, 3, 2, nil] [1, nil]
; [5, 4, 3, 2, 1, nil]


(define (reverse-iter-way l)
  (define (reverse-iter l res)
    (if (null? l)
        res
        (reverse-iter (cdr l) (cons (car l) res))
    )
  )
  (reverse-iter l nil)
)

(reverse-iter-way (list 1 2 3 4 5))

; 2.19

(define (first-denomination coin-values)
  (car coin-values)
)

(define (except-first-denomination coin-values)
  (cdr coin-values)
)

(define (no-more? coin-values)
  (null? coin-values)
)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount) (cc amount us-coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(count-change 100)

; 2.20

; need a flag to know first element is odd or even?
; 

(define (same-parity . arg-list)
  (define (iter-func l flag res)
    (if (null? l)
        res
        (if (= (modulo (car l) 2) flag)
            (iter-func (cdr l) flag (append res (list (car l))))
            (iter-func (cdr l) flag res)
    )
  ))
  (iter-func (cdr arg-list) (modulo (car arg-list) 2) (list (car arg-list)))
)

(same-parity 2 3 4 5 6 7)
