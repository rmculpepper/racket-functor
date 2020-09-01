#lang racket/base
(require syntax-functor)
(provide (all-defined-out))

(define-functor (dop-functor op)
  (define-syntax-rule (dop x) (op x x)))

(define-functor (myand-functor myif)
  (define-syntax myand
    (syntax-rules ()
      [(_ e) (#%expression e)]
      [(_ e1 e ...) (myif e1 (myand e ...) #f)])))

(define-functor (myand-rec-functor myif recur)
  (define-syntax myand
    (syntax-rules ()
      [(_ e) (#%expression e)]
      [(_ e1 e ...) (myif e1 (recur e ...) #f)])))

(define-functor (myand-imp-functor #:allow-implicit)
  (define-syntax myand
    (syntax-rules ()
      [(_ e) (#%expression e)]
      [(_ e1 e ...) (if e1 (myand e ...) #f)])))
