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

(sum_of_two_largest_square 2 3 4)