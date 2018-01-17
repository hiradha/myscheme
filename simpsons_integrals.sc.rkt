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
(factorial 5)
(factorial 6)

; Iterative product definition
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a ) (* result (term a)) )
        ))
  (iter 1 1)
  )

(define (factorial-iter n)
  (product-iter identity 1 inc n))

(factorial-iter 5)
(factorial-iter 6)

    

              



