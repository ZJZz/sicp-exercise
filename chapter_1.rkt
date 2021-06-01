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

; 1.9

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
;recursive

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
;iterative

; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 0 (A 1 9))
(* 2 (A 1 9))
(* 2 (A 0 (A 1 8)))
(* 2 (* 2 (A 1 8)))
(* 2 (* 2 (A 0 (A 1 7))))
(* 2 (* 2 (* 2 (A 1 7))))
(* 2 (* 2 (* 2 (A 0 (A 1 6)))))
(* 2 (* 2 (* 2 (* 2 (A 1 6)))))
(* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (A 1 5))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 4)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 3))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 1))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))
(* 2 (* 2 (* 2 (* 2 (* 2 32)))))
(* 2 (* 2 (* 2 (* 2 64))))
(* 2 (* 2 (* 2 128)))
(* 2 (* 2 256))
(* 2 512)
1024

; (define (f n) (A 0 n)) = 2n
; (define (g n) (A 1 n)) = 2^n

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 4))
(A 1 (A 1 4))
(A 1 16)
65536

; (define (h n) (A 2 n)) = 2^((2)^(2)^(...)) repeat n times

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 4)
65536

