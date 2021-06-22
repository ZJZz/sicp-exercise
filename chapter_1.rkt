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

; 1.11

; recursive
(define (f_recursive n)
  (if (< n 3) n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))
)

; iterative
; thoughts:
; function design: it should save result in a function parameter
; in certain situation, result should return that parameter.
; that parameter should change in each iteration
; question:
; what parameter should we have ? and how they change in each iteration ???

; maybe like this ?
; (define f_iter _ _ 0
;    if n < 3 n
;            f_iter  _ _ f_iter(***) + f_iter(***) + f_iter(***))
;   
; a parameter to record finish condition
; a parameter to save result
; a parameter to iterate
; apply definition change on the parameter change
;
; After a holiday, I think above is much complicated.
; It's on the right track, but not touch the key.
; The key point is think iterative method as for-loop.
; The same is need i as loop count variable, a variable to save result when loop is end.
; Then apply the formula in the loop body to transform the intermediate result.
; The difference is put those in the function parameter.

(define (f_iterative n)
  (f_iter 2 1 0 n)
)

(define (f_iter a b c i)
  (if (= i 0) c
  (f_iter (+ a (* 2 b) (* 3 c)) a b (- i 1)) 
  )
)

; 1.12
;     [0] [1] [2] [3] [4] [5] [6] [7] [8] 
; [0]                  1\n
; [1]              1       1\n
; [2]          1       2       1\n  
; [3]      1       3       3       1\n
; [4]  1       4       6       4       1\n 

; since recursive is a kind of top-down view,
; a suitable way is to think from the last row to the first row,
; maybe last column also.
; The process maybe look like this:
; (define (foo row col)
;    if( on_bounday (1))
;    if( out_right_bounday ("\n"))
;    if( inner and before_left_boundary " ")
;    (display (+ (foo row-1 col -1) (foo row-1 col + 1))) 
; )
; (foo 2 6)
; (foo 1 5) (foo 1 7)
; (foo 0 4) (foor 0 6) (foo 0 6) (foo 0 8)
; 
; every row right bounday col index: 2n - 1 - ( n - row ) = n + row + 1
; every row left bounday col index: n - row - 1
; every inner element index: even row: odd col odd row: even col

;(define (greater_equal x y)
;  (and (> x y) (= x y))
;)

;(define (is_even x)
;  (= (% x 2) 0)
;)

;(define (is_odd x)
;  (= (% x 2) 1)
;)

; we can use index odd and even property to bypass string + int situation
; if a row number index is same with row odd or even. the input element of current number
; must locat in last row but reverse odd/even index.

;(define (pascal_triangle_recursive row col n)
;  (cond ((and (< col (- (- n row) 1)) (greater_equal col 0) (< row n-1)) " ")
;        ((> col (+ n row 1)) "\n")
;        ((or (= row (+ (+ n row) 1)) (= row ((- (- n row) 1)))) (display 1))
;        ((is_even (+ row col)) " ")
;        (else (cond (is_even (+ row col)) (display (+ (pascal_triangle_recursive row-1 col-1 n) (pascal_triangle_recursive row-1 col+1 n)))
;              )
;        )
;  )
;)  

;(define (pascal_triangle n)
;(display )
;)

; I found it is hard to print the formated pascal triangle with limited 
; knowledge about racket language right now. For example what value should 
; return ? number ? or (dispaly x) ? how to traverse the whole col and iter wihtout
; loop ?
; After search the solution,
; the exercise original purpose is just compute the element, not print the whole 
; triangle. Now, switch to this track, reconsider this problme.

;     [0] [1] [2] [3] [4]
; [0]  1
; [1]  1   1
; [2]  1   2   1
; [3]  1   3   3   1
; [4]  1   4   6   4   1

; Now all the return value is number

; the base case is boundary element is 1
; left bounday is col = 0, right boundary col = row

(define (pascal_triangle_element row col)
  (if (or (= col 0) (= row col)) 1
      (+ (pascal_triangle_element (- row 1) col) (pascal_triangle_element (- row 1) (- col 1)))
  )
)

; go through the process:
; (foo 3 2)  
; (+ (foo 2 1) (foo 2 2))
; (+ (+ (foo 1 0) (foo 1 1)) (foo 2 2))
; (+ (+ 1 1) 1)
; (+ 2 1)
; 3
; way more easier !

; 1.16

(define (fast-exp-iter b n a)
(if (= n 1) a
  (fast-exp-iter b (/ n 2) (* a (square b)))
)
)

(define (fast-exp b n)
  (cond ((= n 0) 1)
      ((even? n) (fast-exp-iter b n 1))
      (else (fast-exp-iter b (- n 1) b))   
  )
)

; 1.17

(define (double x)
  (+ x x)
)

(define (halve x)
  (/ x 2)
)

(define (fast-multiply a b)
  (cond ((= b 0) 0)
      ((even? b) (double (fast-multiply a (halve b))))
      (else (+ a (double (fast-multiply a (halve (- b 1))))))   
  ) 
)

; 1.18

(define (fast-multiply-iter a b res)
 (display res)
 (display "\n")
 (cond ((= b 1) res)
       ((even? b) (fast-multiply-iter a (halve b) (+ res (double a))))
       (else (fast-multiply-iter a (halve (- b 1)) (+ res (double a) a)))
       )
)

(define (fast-multiply a b)
  (cond ((= b 0) 0)
       ((= b 1) a)
      ((even? b) (fast-multiply-iter a b 0))
       (else (fast-multiply-iter a (- b 1) a)
      )   
  )
)

(fast-multiply 3 6)