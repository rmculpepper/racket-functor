;; Copyright 2019-2020 Ryan Culpepper
;; Licensed under Apache 2.0.

#lang racket/base
(require racket/syntax syntax/parse
         (for-template racket/base))
(provide (all-defined-out))

;; ============================================================

(define (make-intdefs-syntax-introducer [ctx (syntax-local-make-definition-context)])
  (lambda (stx [mode 'add]) (internal-definition-context-introduce ctx stx mode)))

;; ============================================================

;; extract-syntax-constants : Syntax[ExpandedExpr[X]] Nat
;;                         -> Syntax[Expr[(Syntax -> Syntax) -> X]]
;; Takes an expression and a phase (which must be either -1 or 0 relative to this module, which
;; is 0 or +1 relative to a macro that uses this helper), extracts the syntax constants, and
;; produces an expression for a function that takes a syntax-introducer function, applies it to
;; the syntax constants, and computes the original expression with the updated syntax constants.
(define (extract-syntax-constants ee phase)
  (define insp (variable-reference->module-declaration-inspector (#%variable-reference)))
  (define racc null) ;; mutated, (Listof (list Identifier Expr[Syntax]))
  (define (loop ee0)
    (define-syntax-class rec #:attributes (rec) ;; -- recur and bind .rec to result
      (pattern e #:with rec (loop #'e)))
    (define-syntax-class rec1 #:attributes (rec) ;; -- don't recur for higher-phase expr
      (pattern e #:with rec #'e))
    (define ee (syntax-disarm ee0 insp))
    (define (c newstx) (datum->syntax ee (syntax-e newstx) ee ee))
    (syntax-parse ee
      #:literal-sets ([kernel-literals #:phase phase])
      [(k:#%plain-lambda formals body:rec ...)
       (c #'(k formals body.rec ...))]
      [(k:case-lambda [formals body:rec ...] ...)
       (c #'(k [formals body.rec ...] ...))]
      [(k:if e1:rec e2:rec e3:rec)
       (c #'(k e1.rec e2.rec e3.rec))]
      [(k:begin e:rec ...)
       (c #'(k e.rec ...))]
      [(k:begin0 e0:rec e:rec ...)
       (c #'(k e0.rec e.rec ...))]
      [(k:let-values ([(x ...) rhs:rec] ...) body:rec ...)
       (c #'(k ([(x ...) rhs.rec] ...) body.rec ...))]
      [(k:letrec-values ([(x:id ...) rhs:rec] ...) body:rec ...)
       (c #'(k ([(x ...) rhs.rec] ...) body.rec ...))]
      [(k:set! x rhs:rec)
       (c #'(k x rhs.rec))]
      [(k:with-continuation-mark e1:rec e2:rec e3:rec)
       (c #'(k e1.rec e2.rec e3.rec))]
      [(k:#%plain-app e:rec ...)
       (c #'(k e.rec ...))]
      ;; quote-syntax
      [(k:quote-syntax stx)
       (define tmp (car (generate-temporaries '(stxconst))))
       (set! racc (cons (list tmp this-syntax) racc))
       tmp]
      [(k:quote-syntax stx #:local)
       ;; FIXME: does this have the right behavior for #:local?
       (define tmp (car (generate-temporaries '(stxconst))))
       (set! racc (cons (list tmp this-syntax) racc))
       tmp]
      ;; lsv
      [(k:letrec-syntaxes+values ([(sx ...) srhs:rec1] ...) ([(vx ...) vrhs:rec] ...) body:rec ...)
       (c #'(k ([(sx ...) srhs.rec] ...) ([(vx ...) vrhs.rec] ...) body.rec ...))]
      ;; Leaf forms: quote, #%top, #%variable-reference, varref
      [e #'e]))
  (with-syntax ([expr (loop ee)])
    (with-syntax ([((stxvar stxconstexp) ...) (reverse racc)])
      (syntax-protect
       #`(#%plain-lambda (intro)
           (let-values ([(stxvar) (#%plain-app intro stxconstexp)] ...)
             expr))))))