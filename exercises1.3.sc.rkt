#lang racket

(define (cube x) (* x x x))
; Recursive sum
(define (sum-old term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-old term (next a) next b))))
; Iterative sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)) )))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
; Simpsons integral
(define (s-integral f a b n)
  (define (get-constant a b n) (/ (/ (- b a) n) 3.0 ))
  (define (add-h x) (+ x  (get-constant a b n) ))
  (* (sum f a add-h b)
     (get-constant a b n )))
(s-integral cube 0 1 100)
(s-integral cube 0 1 1000)
(s-integral cube 0 1 10000)

; recursive product definition - similar to sum
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Implement factorial using product
(define (identity x) x)
(define (inc n) (+ n 1))
(define (factorial n)
  (product identity 1 inc n))
; Compute factorial using factorial function
(factorial 5)
(factorial 6)
; John Wallis pi approximation formula
(define (compute-pi n )
  (define (term x)  (* (/ (* 2 x) (- (* 2 x) 1))  (/ (* 2 x) (+ (* 2 x) 1))))
  (define (next x) (+ x 1) )
  (* (product term 1 next n) 2.0)
  )
  
(compute-pi 10000)

; Iterative product definition
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a ) (* result (term a)) )
        ))
  (iter 1 1)
  )
; Define factorial using product iterrative version
(define (factorial-iter n)
  (product-iter identity 1 inc n))

; Compute factorial using iterative version
(factorial-iter 5)
(factorial-iter 6)

; accumulate recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate combiner null-value term (next a) next b) (term a))
  )
  )
(define (sumcombiner accumulatedresult x) (+ accumulatedresult x))

(define (sum-accumulator term a next b)
  (accumulate sumcombiner 0 term a next b) )

(define (productcombiner accumulatedresult x) (* accumulatedresult x))
(define (product-accumulator term a next b)
  (accumulate productcombiner 1 term a next b)
  )
(define (factorial-using-accumulate n)
  (product-accumulator identity 1 inc n))
; Compute factorial using accumulate
(factorial-using-accumulate 5)
(factorial-using-accumulate 6)

; iterative accumulate and functions based off it
(define (accumulate-iterative combiner null-value term a next b)
  (define (loop combiner a term next b accumulatedvalue)
    (if (> a b)
       accumulatedvalue
       (loop combiner (next a) term next b (combiner accumulatedvalue (term a)))))
  (loop combiner a term next b null-value)
  )

(define (sum-accumulator-iterative term a next b)
  (accumulate-iterative sumcombiner 0 term a next b)
  )

(define (product-accumulator-iterative term a next b)
  (accumulate-iterative productcombiner 1 term a next b)
  )

(define (factorial-using-accumulate-iterative n)
  (product-accumulator-iterative identity 1 inc n))
; Compute factorial usig iterative accumulate function as base
(factorial-using-accumulate-iterative 5)
(factorial-using-accumulate-iterative 6)
  


  
  

    

              



