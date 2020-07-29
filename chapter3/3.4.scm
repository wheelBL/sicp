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
  
  (define times_try 7)

  (define (call-the-police) (error "The cops have been called!"))
  
  (define (check-passwd-with-times passwd)
    (if (check-password passwd)
        (begin (set! times_try 7) true)
        (begin (set! times_try (- times_try 1)) false)))

  (define (secure pass m)
    (if (check-passwd-with-times pass)
        (dispatch pass m)
        (if (<= times_try 0)
            (call-the-police)
            (lambda _ "Incorrect password"))))
            
  (define (dispatch str m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
    )
  secure)
