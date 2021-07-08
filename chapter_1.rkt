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

; this problem need notice a few points:
; - can not simply apply double the result to compute, and halve the scale factor  
; - the key is find the formula
; a * b
; b = 6 = 2 * 3
; a a a a a a
; {a a a} {a a a}
; {3a} {3a} how to compute 3a with only double ? a + double a, 
; {6a}
; 
; b = 12
; a a a a a a a a a a a a
; {a a a a a a} {a a a a a a}
; {6a} {6a}
; {12a}
; maybe is not compute so quickly
; a a a a a a a a a a a a 
; {a a} {a a} {a a} {a a} {a a} {a a}
; {2a} {2a} {2a} {2a} {2a} {2a}
; {4a} {4a} {4a}
; {8a} {4a}
; a a a a a a
; {2a} {2a} {2a}
; {4a} {2a}
;
; a  6
; 2a 3
; 2a + 2 * 2a
; 2a + 4a
; need change a value too !  

(define (fast-multiply-iter a b left_part)
  (cond ((= b 1) (+ left_part a))
        ((even? b) (fast-multiply-iter (double a) (halve b) left_part))
        (else (fast-multiply-iter a (- b 1) a))   
  )
)

(define (fast-multiply a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        (else (fast-multiply-iter a b 0))
  )       
)

(fast-multiply 3 12)

; 1.19
; F(1) = 1 F(0) = 0
;
; F(2) = F(1) + F(0) | F(1) = F(1)
;
; F(3) = F(2) + F(1) = F(1) + F(0) + F(1) | F(2) = F(1) + F(0)
;
; F(4) = F(3) + F(2) = F(2) + F(1) + F(2) | F(3) = F(2) + F(1) = F(1) + F(0) + F(1)
;
; F(5) = F(4) + F(3) = F(3) + F(2) + F(3) | F(4) = F(3) + F(2)
;
; F(6) = F(5) + F(4) = F(3) + F(2)        | F(5) = F(4) + F(3)
; consider every time the a and b is different from init

; after go through the code, there is some point worth to be noted
; when the count is even, only change p,q to p',q'
; when the count is odd, p,q keep same, only transform a,b
; the exercise is ask p' and q', so focus on how F(2) trans to F(4) 
; by F(0) and F(1), F(4) trans to F(8) by F(3) and F(2),
; F(3) to F(6) by F(2) and so on.
;
; When the count is odd, except the first time it use original p and q.
; the rest time it apply p' and q'
; To find the pattern, the key is know when apply p' and q', and what a and b is
; in this time.
; 
; I think my track is diverge from the right track. After check the solution on the 
; http://community.schemewiki.org/?sicp-ex-1.19. The simplest way is just deduce the formula.
; a' <- bq + aq + ap
; b' <- bp + aq
;
; a'' <- b'q + a'q + a'p = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p = b(2pq + q^2) + a(2pq + q^2) + a(p^2 + q^2)
; b'' <- b'p + a'q       = (bp + aq)p + (bq + aq + ap)q                   = b(p^2 + q^2) + a(2pq + q^2)


(define (fib n)
(fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
(cond ((= count 0) b)
      ((even? count)
       (fib-iter a
                 b
                 (+ (square p) (square q))
                 (+ (* 2 (* p q)) (square q))      ; compute q'
                 (/ count 2)))
      (else (fib-iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
; 1.22

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds))
)

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time)))
)

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
)

(define (iterate-range current end)
  (when (< current end)
    (timed-prime-test current)
    (iterate-range (+ current 2) end)
  )
  
)

(define (timing-range-prime start end)
  (if (odd? start) (iterate-range start end)
  (iterate-range (+ start 1) end))
)

; 1.23

(define (smallest-divisor n)
  (find-divisor-next n 2))

(define (next-divisor n)
  (if (= n 2) (3)
  (+ n 2))
)

(define (find-divisor-next n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

; 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test-fast n)
  (newline)
  (display n)
  (start-prime-test-fast n (current-inexact-milliseconds)))

(define (start-prime-test-fast n start-time)
  (when (fast-prime? n 10)
      (report-prime (- (current-inexact-milliseconds) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; TODO:1.25
; TODO:1.26
; TODO:1.27
; TODO:1.28

; 1.29
; I'm not sure how Simpson's Rule y_n factor change. Because 1, 2, 4 is change
; unregulated. And where is k come from ?
; oh, k is just the subscript. But how to determin the factor ?


; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0)
)

; 1.31
(define (product-recur term a next b)
(if (> a b) 1
  (* (term a) (product-recur term (next a) next b))
)
)

(define (product-iter term a next b)
(define (iter a result)
  (if (> a b)
      result
      (iter (next a) (* result (term a)))))
(iter a 1)
)

(define (product a b)
(product-iter identity a inc b)
)

(define (facotrial n)
  (product 1 n)
)

; the formula in the book is another unregulated changed form. list 
; this TODO for now.

; 1.32

(define (accumulate-recur combiner base term a next b)
  (if (> a b) base
    (combiner (term a) (accumulate-recur combiner base term (next a) next b))
  )
)

(define (accumulate-iter combiner base term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner result (term a)))
    )
  )
  (iter a base)
)

;(define (accumulate combiner null-value term a next b)
; (accumulate-recur combiner null-value term a next b) 
;)

(define (accumulate combiner null-value term a next b)
 (accumulate-iter combiner null-value term a next b) 
)

(define (add a b)
  (+ a b)
)

(define (product a b)
  (* a b)
)

(accumulate product 1 identity 1 inc 5)

; 1.33

(define (filtered-accumulate-recur combiner base term a next b filter)
(if  (> a b) base
     (if (filter a) (combiner (term a) (filtered-accumulate-recur combiner base term (next a) next b filter))
         (filtered-accumulate-recur combiner base term (next a) next b filter)
     )
)
)

(define (filtered-accumulate-iter combiner base term a next b filter)
(define (iter a result filter)
  (if (> a b) result
      (if (filter a) (iter (next a) (combiner result (term a)) filter)
          (iter (next a) result filter))
      
  )
)
(iter a base filter)
)

(define (filtered-accumulate combiner null-value term a next b filter)
(filtered-accumulate-iter combiner null-value term a next b filter) 
)

(define (gcd a b)
(if (= b 0)
    a
    (gcd b (remainder a b)))
)

(define (my-filter? i)
(and (= (gcd i 7) 1) (< i 7))
)

(filtered-accumulate add 0 square 2 inc 10 prime?)

(filtered-accumulate product 1 identity 1 inc 10 my-filter?)

; 1.34

(define (f g)
  (g 2)
)

; supposed to be error,
(f f) -> (f 2) -> (2, 2) 

; after validate in the IDE
; (f, f) is same error with (2, 2) 

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y))
       )
       (+ (* x (square a)) (* y b) (* a b))
  )
)

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
       (if (close-enough? neg-point pos-point) midpoint
          (let ((test-value (f midpoint)))
               (cond ((positive? test-value) (search f neg-point midpoint))
                     ((negative? test-value) (search f midpoint pos-point))
                     (else midpoint)
               )
          )
       )
  )
)

(define (half-interval-method f a b)
    (let ((a-value (f a)) 
          (b-value (f b)))
         (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
               ((and (negative? b-value) (positive? a-value)) (search f b a))
               (else (error "Values are not of opposite sign" a b)))
    )
)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
         (if (close-enough? guess next) next 
             (try next)
         )
    )
  )
  (try first-guess)
)

; 1.35

(fixed-point (lambda (x) (/ (+ x 1) x)) 2)
; output is 1.(377/610) which is close 1.61803

; 1.36