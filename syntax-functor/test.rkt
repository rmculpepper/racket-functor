#lang racket/base
(require racket/bool
         rackunit
         syntax-functor
         "test-helper.rkt")

;; ----------------------------------------
;; Can apply functors defined in local scope.

(let ()
  (define-functor (dop-functor op)
    (define-syntax-rule (dop x) (op x x)))
  (let ()
    (apply-functor dop-functor +)
    (check-equal? (dop 10) 20))
  (apply-functor dop-functor *)
  (check-equal? (dop 50) 2500))

;; ----------------------------------------
;; Can apply functors defined in submodule.

(module functors racket/base
  (require syntax-functor)
  (provide (all-defined-out))
  (define-functor (dop-functor op)
    (define-syntax-rule (dop x) (op x x)))
  (define-functor (myand-functor if)
    (define-syntax myand
      (syntax-rules ()
        [(_ e) (#%expression e)]
        [(_ e1 e ...) (if e1 (myand e ...) #f)]))))

(let ()
  (local-require (submod "." functors))
  (let ()
    (apply-functor dop-functor +)
    (check-equal? (dop 12) 24)))

;; ----------------------------------------
;; Can apply functors defined in other module.

(let ()
  (apply-functor dop-functor +)
  (check-equal? (dop 12) 24))
(let ()
  (apply-functor dop-functor *)
  (check-equal? (dop 12) 144))

(let ()
  (apply-functor myand-functor if)
  (check-equal? (myand 1 2 3) 3)
  (let ([b (box #f)])
    (check-equal? (myand 1 2 3 #f (set-box! b #t)) #f)
    (check-equal? (unbox b) #f)))
(let ()
  (define (strict-if a b c) (if a b c))
  (apply-functor myand-functor strict-if)
  (check-equal? (myand 1 2 3) 3)
  (let ([b #f])
    (check-equal? (myand 1 2 #f (set! b #t)) #f)
    (check-equal? b #t)))

;; ----------------------------------------
;; Recursive linking

(let ()
  (apply-functor myand-rec-functor if myand)
  (check-equal? (myand 1 2 3) 3)
  (let ([b (box #f)])
    (check-equal? (myand 1 2 3 #f (set-box! b #t)) #f)
    (check-equal? (unbox b) #f)))

;; ----------------------------------------
;; Nesting

(let ()
  (apply-functor dop-functor +)
  (let ()
    (apply-functor dop-functor *)
    (check-equal? (dop 12) 144)))

;; ----------------------------------------
;; Copy

(let ()
  (define-functor (f base ao2)
    (define-syntax ao
      (syntax-rules ()
        [(_) base]
        [(_ e) (#%expression e)]
        [(_ e1 e ...) (ao2 e1 (ao e ...))]))
    #:copy
    (define (aomap f xs)
      (if (pair? xs) (ao (f (car xs)) (aomap f (cdr xs))) base)))

  (let ()
    (apply-functor f false or)
    (check-equal? (ao) #f)
    (check-equal? (ao 1 2 3) 1)
    (let ([x #f])
      (check-equal? (ao 1 2 #f (set! x #t)) 1)
      (check-equal? x #f))
    (check-equal? (aomap odd? '(1 2 3)) #t)
    (check-equal? (aomap odd? '(1 2 bad)) #t)
    (check-equal? (aomap odd? '(2 4 6)) #f))
  (let ()
    (apply-functor f true and)
    (check-equal? (ao) #t)
    (check-equal? (ao 1 2 3) 3)
    (let ([x #f])
      (check-equal? (ao 1 2 #f (set! x #t)) #f)
      (check-equal? x #f))
    (check-equal? (aomap odd? '(1 2 3)) #f)
    (check-equal? (aomap odd? '(1 2 bad)) #f)
    (check-equal? (aomap odd? '(1 3 5)) #t)))

;; ----------------------------------------
;; Value definitions

(let ()
  (define-functor (f x)
    (define-syntax-rule (mx) x)
    (define xx (+ (mx) (mx))))

  (let ([one 1])
    (apply-functor f one)
    (check-equal? xx 2))
  (let ([two 2])
    (apply-functor f two)
    (check-equal? xx 4)))

;; ----------------------------------------
;; Implicit linking

(let ()
  (define (strict-if a b c) (if a b c))
  (apply-functor myand-imp-functor #:implicit [if strict-if])
  (check-equal? (myand 1 2 3) 3)
  (let ([b #f])
    (check-equal? (myand 1 2 #f (set! b #t)) #f)
    (check-equal? b #t)))

(let ()
  (define (strict-if a b c) (if a b c))
  (apply-functor myand-imp-functor #:implicit [if strict-if] [#%expression list])
  (check-equal? (myand 1 2 3) (list 3))
  (check-equal? (myand 1 #f 3) #f))
