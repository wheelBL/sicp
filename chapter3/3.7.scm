#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (check-password str)
    (eq? password str))
  (define (dispatch str m)
    (if (check-password str)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        (lambda _ "Incorrect password"))
    )
  dispatch)

; exercise 3.7
;; (define peter-acc (make-account 100 'open-sesame))
;; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;; ((paul-acc 'rosebud 'deposit) 10)
;; (define error-acc (make-joint peter-acc 'open 'rosebud))
(define (make-joint acc ori-passwd shared-passwd)
  (define (joint-acc passwd m)
    (cond ((eq? passwd shared-passwd)
           (acc ori-passwd m))
          (else (error "Incorrect password"))))
  (cond ((number? ((acc ori-passwd 'deposit) 0))
              joint-acc)
        (else (error "Original password is false" "make-joint"))))
