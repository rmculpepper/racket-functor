;; Copyright 2019-2020 Ryan Culpepper
;; Licensed under Apache 2.0.

#lang info

;; ========================================
;; pkg info

(define collection "syntax-functor")
(define deps
  '("base"
    "syntax-functor-lib"
    "rackunit-lib"))
(define implies
  '("syntax-functor-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))

;; ========================================
;; collect info

(define name "syntax-functor")
(define scribblings
  '(["syntax-functor.scrbl" ()]))
