#lang planet neil/sicp

;; 1.3.1 Formulating abstractions with higher-order procedures

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)

;; 1.3.2 Constructing procedures using lambda
; (lambda (<formal-parameters>) <body>)
;; using let to create local variables

;; 1.3.3 Procedures as general methods
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))))