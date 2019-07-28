(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (< (* n d) 0)
                  -
                  +)))
    (cons (sign (abs (/ n g)))
          (abs (/ d g)))))
