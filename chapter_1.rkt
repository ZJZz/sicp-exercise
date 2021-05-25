#lang racket
; 1.3
(define (square x) (* x x))

(define (sum_of_square x y)
  (+ (square x) (square y))
)

(define (max x y)
  (if (> x y)
      x
      y)
)

(define (sum_of_two_largest_square x y z)
  (sum_of_square (max x y) (max y z))
)

;(sum_of_two_largest_square 2 3 4)

; 1.7

(define (abs x)
  (if (< x 0) (- x)
      x)
)

(define (good_enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(good_enough? 55000.00045 1000000000)

(define (average x y)
  (/ (+ x y) 2)
)

(define (improve guess x)
  (average guess (/ x guess) )
)

(improve 9999.9999 1000000000)