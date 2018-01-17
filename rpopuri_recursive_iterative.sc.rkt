#lang racket/base
"Hello, World!"
(define 1-to-4 (list 1 2 3 4))
(car 1-to-4)
(cdr 1-to-4)
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3

(define (f  n)
(cond ((< n 3) n)
        (else ( + (f (- n 1))  (* 2 (f (- n 2)))  (* 3 (f (- n 3))) ))
  )
 )
 (f 8)


(define (fiter c b a  counter)
    (if (< counter 3 )
        a
     (fiter  b  a ( + a  (* 2 b)  (* 3  c) )  (- counter 1))
     )
  )
(fiter 0 1 2  8 )


(define (expt  b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if ( = counter 0)
    product
  (expt-iter b (- counter 1 ) (* b product) ))
)

(expt 2 4)












