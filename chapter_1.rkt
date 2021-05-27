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

; 1.7 (not done yet)

(define (abs x)
  (if (< x 0) (- x)
      x)
)

(define (good_enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(good_enough? 99999.99999 10000000000)

(define (average x y)
  (/ (+ x y) 2)
)

(define (improve guess x)
  (average guess (/ x guess) )
)

(improve 99999 10000000000)

(define (refine_good_enough? new_guess old_guess)
  (< (/ new_guess  old_guess) 0.00001)
)

(refine_good_enough? (improve 99999.99999 10000000000) 99999.99999)

;; 1.8

(define (cube x)
  (* x (square x))
)

(define (improve_cube y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3)
)

(define (good_enough_cube? guess x)
  (< (abs (- (cube guess) x)) 0.001)
)

(define (cube_root_iter guess x)
  (if (good_enough_cube? guess x)
      guess
      (cube_root_iter (improve_cube guess x) x))
)

(define (cube_root x)
  (cube_root_iter 3 x) 
)

(cube_root 9)


