(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                        kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

;; exercise 1.11
(define (f x)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 x))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)

;; exercise 1.12
;; f(r,c) = 1, c == 0 or c > row - 1
;;          f(r-1, c-1) + f(r-1, c), otherwise
(define (pascal-triangle row col)
  (if (or (= col 0) (> col (- row 1)))
    1
    (+ (pascal-triangle (- row 1) (- col 1))
       (pascal-triangle (- row 1) col))))

;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
(pascal-triangle 0 0)
(pascal-triangle 3 3)
(pascal-triangle 3 2)
(pascal-triangle 4 2)

;; Euclid's Algorithm
;; GDB(a, b) = GDB(b, r)
;; r is the remainder when a is devided by b
(define (gdb a b)
  (if (= b 0)
    a
    (gdb b (remainder a b))))
