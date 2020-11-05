#lang racket

(define (hora N)
  (list (horas N) (minutos N) (segundos N))
  
)

  (define (m N)
  (quotient N 60)
  )
(define (h N)
  (quotient(m N) 60)
  )

(define (horas N)
  (- (remainder (h N) 24) 3)
  )

(define (minutos N)
  (remainder (m N) 60)
  )

(define (segundos N)
  (remainder N 60)
  )
