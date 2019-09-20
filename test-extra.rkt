#lang racket/base

(module f1 racket/base
  (require "functor.rkt")
  (define-functor (f)
    (define-syntax-rule (dop x) (op x x)))
  (provide (all-defined-out)))

(module u1 racket/base
  (require "functor.rkt" (submod ".." f1))
  (apply-functor f #:and [+ op])
  (printf "(dop 12) w/ += ~v\n" (dop 12))
  (let ()
    (apply-functor f #:and [* op])
    (printf "(dop 12) w/ * = ~v\n" (dop 12))))

(require 'u1)

(module f2 racket/base
  (require "functor.rkt")
  (define-functor (f)
    (define-syntax myand
      (syntax-rules ()
        [(_ e) (#%expression e)]
        [(_ e1 e ...) (if e1 (myand e ...) #f)])))
  (provide (all-defined-out)))

(module u2 racket/base
  (require "functor.rkt" (submod ".." f2))
  (define (p x) (printf "got ~v; " x) x)
  (apply-functor f #:and [if if])
  (printf "(myand 1 2 3) w/ if = ~v\n" (myand 1 2 #f (p 3)))
  (let ()
    (define (if* a b c) (if a b c))
    (apply-functor f #:and [if* if])
    (printf "(myand 1 2 3) w/ if* = ~v\n" (myand 1 2 #f (p 3)))))

(require 'u2)

(module f3 racket/base
  (require "functor.rkt")
  (define-functor (f)
    (define-syntax myand
      (syntax-rules ()
        [(_ e) (#%expression e)]
        [(_ e1 e ...) (myif e1 (myand* e ...) #f)])))
  (provide f))

(module u3 racket/base
  (require "functor.rkt" (submod ".." f3))
  (define (p x) (printf "got ~v; " x) x)
  (let ()
    (apply-functor f #:and [if myif] [myand myand*])
    (printf "(myand 1 2 (p 3)) rec = ~v\n" (myand 1 2 #f (p 3))))
  (let ()
    (define (if* a b c) (if a b c))
    (apply-functor f #:and [if* myif] [myand myand*])
    (printf "(myand 1 2 (p 3)) strict rec = ~v\n" (myand 1 2 #f (p 3)))))

(require 'u3)

;; ----

(module f4 racket/base
  (require "functor.rkt")
  (define-functor (f)
    (define-syntax ao
      (syntax-rules ()
        [(_) base]
        [(_ e) (#%expression e)]
        [(_ e1 e ...) (ao2 e1 (ao e ...))]))
    (define (aomap f xs)
      (if (pair? xs) (ao (f (car xs)) (aomap f (cdr xs))) base)))
  (provide f))

(module u4 racket/base
  (require "functor.rkt" (submod ".." f4) racket/bool)
  (define (p x) (printf "got ~v; " x) x)
  (let ()
    (apply-functor f #:and [false base] [or ao2])
    (printf "(ao 1 2 #f (p 3)) w/ or = ~v\n" (ao 1 2 #f (p 3)))
    (printf "(apmap odd? '(1 2 bad)) = ~v\n" (aomap odd? '(1 2 bad))))
  (let ()
    (apply-functor f #:and [true base] [and ao2])
    (printf "(ao 1 2 #f (p 3)) w/ and = ~v\n" (ao 1 2 #f (p 3)))
    (printf "(apmap odd? '(1 2 bad)) = ~v\n" (aomap odd? '(1 2 bad)))))

(require 'u4)