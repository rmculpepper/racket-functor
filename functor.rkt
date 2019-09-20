#lang racket/base
(require (for-syntax racket/base syntax/parse racket/match syntax/stx racket/list "functor-util.rkt"))
(provide (all-defined-out))

;; ============================================================

;; FIXME: catch lifts?

;; A (Syntax) Functor is written
;; - (define-functor (f <sig-id>) def ...)
;; - (apply-functor f <link-id> ... #:and [<extra-link-id> <extra-id>] ...)

;; TODO:
;; - add #:copy section to functor body and do export linking lazily?
;; - add by-name application?
;; - add optional parameters (beware letrec-scoping for [id id] params!)

(begin-for-syntax
  (struct functor (implctx imports parts)))

(define-syntax define-functor
  (syntax-parser
    [(_ (f:id import:id ...) body:expr ...)
     (define implctx (datum->syntax this-syntax 'import-here))
     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (syntax->list #'(import ...)) #f ctx)
     (define intro1 (make-intdefs-syntax-introducer ctx))
     (define parts (process-functor-body (syntax->list #'(body ...)) ctx))
     (with-syntax ([(part ...) parts])
       #`(define-syntax f
           (functor (quote-syntax #,(intro1 implctx 'add))
                    (quote-syntax #,(intro1 #'(import ...) 'add))
                    (list part ...))))]))

(begin-for-syntax
  (define-splicing-syntax-class extra-imports
    (pattern (~seq #:and [link:id import:id] ...))
    (pattern (~seq) #:with (link ...) '() #:with (import ...) '())))

(define-syntax apply-functor
  (syntax-parser
    [(_ (~var fname (static functor? 'functor)) link:id ... extra:extra-imports)
     (define f (attribute fname.value))
     (define implctx (syntax-local-introduce (functor-implctx f)))
     (define imports (syntax-local-introduce (functor-imports f)))
     (define intro2 (make-intdefs-syntax-introducer))
     (define imports-here (intro2 imports))
     (define extra-imports-here (intro2 (datum->syntax implctx (syntax->datum #'(extra.import ...)))))
     (with-syntax ([implctx-here (intro2 implctx)]
                   [(import-here ...) imports-here]
                   [(extra-import-here ...) extra-imports-here]
                   [explctx (datum->syntax this-syntax 'export-here)]
                   [(index ...) (range (length (functor-parts f)))])
       #`(begin
           ;; Compared to imports, imports-here has scope from intro2 but also gets scope from
           ;; enclosing module (or local definition context, etc). See also apply-functor-part.
           (define-syntaxes (import-here ...)
             (values (make-rename-transformer (quote-syntax link)) ...))
           (define-syntaxes (extra-import-here ...)
             (values (make-rename-transformer (quote-syntax extra.link)) ...))
           (apply-functor-part fname explctx implctx-here index) ...))]))

(define-syntax apply-functor-part
  (syntax-parser
    [(_ (~var fname (static functor? 'functor)) explctx implctx-here index)
     (define f (attribute fname.value))
     (define implctx (syntax-local-introduce (functor-implctx f)))
     (define intro (make-syntax-delta-introducer #'implctx-here implctx))
     (match (list-ref (functor-parts f) (syntax-e #'index))
       [(stx-defs exports proc)
        (with-syntax ([(export-here ...) (intro (syntax-local-introduce exports))]
                      [(export* ...) (datum->syntax #'explctx (syntax->datum exports))])
          #`(begin
              (define-syntaxes (export-here ...)
                (link-stx-defs (quote-syntax fname) (quote-syntax implctx-here) (quote index)))
              (define-syntaxes (export* ...)
                (values (make-rename-transformer (quote-syntax export-here)) ...))))]
       [(val-defs exports def)
        (with-syntax ([(export-here ...) (intro (syntax-local-introduce exports))]
                      [(export* ...) (datum->syntax #'explctx (syntax->datum exports))])
          #`(begin
              #,(intro (syntax-local-introduce def))
              (define-syntaxes (export* ...)
                (values (make-rename-transformer (quote-syntax export-here)) ...))))])]))

(begin-for-syntax

  (define (link-stx-defs functor-id implctx-here index)
    (define f (syntax-local-value functor-id))
    (define part (list-ref (functor-parts f) index))
    (define intro (make-syntax-delta-introducer implctx-here (functor-implctx f)))
    ((stx-defs-proc part) intro))

  )
