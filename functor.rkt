#lang racket/base
(require (for-syntax racket/base syntax/parse racket/match syntax/stx racket/list "functor-util.rkt"))
(provide (all-defined-out))

;; ============================================================

;; This library implements syntax functors: mappings from bindings to macro definitions.

;; It implements two kinds of parameterization: explicit and implicit. Explicit parameters
;; are listed as arguments when the functor is defined and references must be supplied when
;; the functor is applied. Implicit parameters are not listed; they given symbolically
;; when the functor is applied. It's probably a bad idea to allow implicit parameterization
;; by default; a functor should have to opt in to allow them.

;; A (Syntax) Functor is written
;; - (define-functor (f <sig-id>) def ...)
;; - (apply-functor f <link-id> ... #:and [<extra-link-id> <extra-id>] ...)

;; TODO:
;; - add by-name application?
;; - add optional parameters (beware letrec-scoping for [id id] params!)
;; - fix export renaming (eg, marked export should not be available)

(begin-for-syntax
  ;; A Functor is (functor Id (Listof Id) (Listof FunctorPart) Syntax)
  (struct functor (implctx imports parts copy-form))

  ;; A FunctorPart is (stx-defs (Listof Identifier) ((Syntax -> Syntax) -> Syntax[Expr]))
  ;; where the proc represents the define-syntaxes RHS, processed with extract-syntax-constants.
  (struct stx-defs (exports proc) #:prefab)

  ;; process-body : (Listof Syntax) IntDefCtx -> (Listof FunctorPart)
  (define (process-body body-forms ctx fstx)
    (define ectx (list (gensym)))
    (let loop ([body-forms body-forms])
      (cond [(null? body-forms) null]
            [else
             (define ee (local-expand (car body-forms) ectx #f ctx))
             (syntax-parse ee
               #:literal-sets (kernel-literals)
               [(begin ~! e ...) (loop (append (syntax->list #'(e ...)) (cdr body-forms)))]
               [(k:define-syntaxes ~! (x:id ...) rhs:expr)
                (define erhs (local-transformer-expand #'rhs 'expression null ctx))
                (syntax-local-bind-syntaxes (syntax->list #'(x ...)) erhs ctx)
                (define erhs* (extract-syntax-constants erhs (add1 (syntax-local-phase-level))))
                (cons #`(stx-defs (quote-syntax (x ...)) #,erhs*)
                      (loop (cdr body-forms)))]
               [_ (raise-syntax-error #f "only syntax definitions allowed in functor body" fstx ee)])]))))

(define-syntax (define-functor stx)
  (define-splicing-syntax-class copy-section
    (pattern (~seq #:copy form:expr ...))
    (pattern (~seq) #:with (form ...) null))
  (syntax-parse stx
    [(_ (f:id import:id ...) body:expr ... copy:copy-section)
     (define implctx (datum->syntax this-syntax 'import-here))
     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (syntax->list #'(import ...)) #f ctx)
     (define intro1 (make-intdefs-syntax-introducer ctx))
     (define parts (process-body (syntax->list #'(body ...)) ctx stx))
     (with-syntax ([(part ...) parts])
       #`(define-syntax f
           (functor (quote-syntax #,(intro1 implctx 'add))
                    (quote-syntax #,(intro1 #'(import ...) 'add))
                    (list part ...)
                    (quote-syntax #,(intro1 #'(begin copy.form ...) 'add)))))]))

(define-syntax (apply-functor stx)
  (define-splicing-syntax-class extra-imports
    (pattern (~seq #:and [link:id import:id] ...))
    (pattern (~seq) #:with (link ...) '() #:with (import ...) '()))
  (syntax-parse stx
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
                   [(index ...) (range (length (functor-parts f)))]
                   [copy-form (intro2 (syntax-local-introduce (functor-copy-form f)))])
       #`(begin
           ;; Compared to imports, imports-here has scope from intro2 but also gets scope from
           ;; enclosing module (or local definition context, etc). See also apply-functor-part.
           (define-syntaxes (import-here ...)
             (values (make-rename-transformer (quote-syntax link)) ...))
           (define-syntaxes (extra-import-here ...)
             (values (make-rename-transformer (quote-syntax extra.link)) ...))
           (apply-functor-part fname explctx implctx-here index) ...
           (copy-functor-part fname explctx implctx-here copy-form)))]))

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
                (values (make-rename-transformer (quote-syntax export-here)) ...))))])]))

(begin-for-syntax
  (define (link-stx-defs functor-id implctx-here index)
    (define f (syntax-local-value functor-id))
    (define part (list-ref (functor-parts f) index))
    (define intro (make-syntax-delta-introducer implctx-here (functor-implctx f)))
    ((stx-defs-proc part) intro)))

(define-syntax (copy-functor-part stx)
  (syntax-parse stx
    [(_ (~var fname (static functor? 'functor)) explctx implctx-here copy-form)
     (define ee (local-expand #'copy-form (syntax-local-context) #f))
     (syntax-parse ee
       #:literal-sets (kernel-literals)
       [(k:begin ~! form:expr ...)
        #'(begin (copy-functor-part fname explctx implctx-here form) ...)]
       [(k:define-syntaxes ~! (x:id ...) rhs:expr)
        #`(begin #,ee (rename-exports explctx (x ...)))]
       [(k:define-values ~! (x:id ...) rhs:expr)
        #`(begin #,ee (rename-exports explctx (x ...)))]
       ;; FIXME: detect require, provide, etc and error?
       [e:expr
        #'e])]))

(define-syntax (rename-exports stx)
  (syntax-parse stx
    [(_ explctx (export ...))
     (with-syntax ([(export* ...) (datum->syntax #'explctx (syntax->datum #'(export ...)))])
       #`(define-syntaxes (export* ...)
           (values (make-rename-transformer (quote-syntax export)) ...)))]))
