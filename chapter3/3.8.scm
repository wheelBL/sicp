(define f
  (let ((count 1))
    (lambda (x)
      (set! count (* count x))
      count)))
