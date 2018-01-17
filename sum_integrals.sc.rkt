#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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
(define (s-integral f a b n)
  (define (get-constant a b n) (/ (/ (- b a) n) 3.0 ))
  (define (add-h x) (+ x  (get-constant a b n) ))
  (* (sum f a add-h b)
     (get-constant a b n )))
(s-integral cube 0 1 100)
(s-integral cube 0 1 1000)
(s-integral cube 0 1 10000)



