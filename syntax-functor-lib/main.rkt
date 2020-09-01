;; Copyright 2019-2020 Ryan Culpepper
;; Licensed under Apache 2.0.

#lang racket/base
(require (for-syntax racket/base racket/match racket/list
                     syntax/parse syntax/stx
                     "private/functor-util.rkt"))
(provide (all-defined-out))

;; ============================================================

;; This library implements syntax functors: mappings from bindings to macro definitions.

;; It implements two kinds of parameterization: explicit and implicit. Explicit parameters
;; are listed as arguments when the functor is defined and references must be supplied when
;; the functor is applied. Implicit parameters are not listed; they given symbolically
;; when the functor is applied. It's probably a bad idea to allow implicit parameterization
;; by default; a functor should have to opt in to allow them.

;; A (Syntax) Functor is written
;; - (define-functor (f <import-id> ...) def ...)
;; - (apply-functor f <link-id> ... #:implicit [<imp-import-id> <imp-link-id>] ...)

;; TODO:
;; - add by-name application?
;; - add optional parameters (beware letrec-scoping for [id id] params!)
;; - fix export renaming (eg, marked export should not be available)

(begin-for-syntax
  ;; A Functor is (functor Id (Listof Id) Boolean (Listof FunctorPart))
  (struct functor (implctx imports allow-implicit? parts) #:transparent)

  ;; A FunctorPart is one of
  ;; - (stx-defs (Listof Identifier) ((Syntax -> Syntax) -> Syntax[Expr]))
  ;;   where the proc is the define-syntaxes RHS, processed with extract-syntax-constants
  ;; - (copy-form Syntax) -- for define-values, expressions
  (struct stx-defs (exports proc) #:prefab)
  (struct copy-form (stx) #:prefab)

  ;; process-body : (Listof Syntax) IntDefCtx -> (Listof Syntax[Expr[FunctorPart]])
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
               [(k:define-values ~! (x:id ...) rhs:expr)
                (syntax-local-bind-syntaxes (syntax->list #'(x ...)) #f ctx)
                (cons #`(copy-form (quote-syntax #,ee))
                      (loop (cdr body-forms)))]
               [expr
                (cons #`(copy-form (quote-syntax (#%expression expr)))
                      (loop (cdr body-forms)))]
               ;; Old case, now unreachable:
               [_ (raise-syntax-error #f "only syntax definitions allowed in functor body"
                                      fstx ee)])]))))

;; Functor definition:
;; - initial scope set DefInitScs
;; - creates fresh (intdef) scope DefSc
;; - imports are defined as variables in DefSc
;;   (note: imports may have scopesets different from DefInitScs!)
;; - DefSc is applied to the functor's forms
;;   (note: forms may have scopesets different from DefInitScs!)
;; - implctx is created with scopeset = DefInitScs + DefSc

(define-syntax (define-functor stx)
  (define-splicing-syntax-class implicit-decl
    (pattern (~seq #:allow-implicit) #:with allow-implicit? #'#t)
    (pattern (~seq) #:with allow-implicit? #'#f))
  (define-splicing-syntax-class copy-section
    (pattern (~seq #:copy form:expr ...))
    (pattern (~seq) #:with (form ...) null))
  (syntax-parse stx
    [(_ (f:id import:id ... ai:implicit-decl) body:expr ... copy:copy-section)
     (define implctx (datum->syntax this-syntax 'import-here))
     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (syntax->list #'(import ...)) #f ctx)
     (define intro1 (make-intdefs-syntax-introducer ctx))
     (define parts (process-body (syntax->list #'(body ...)) ctx stx))
     (with-syntax ([(part ...) parts]
                   [to-copy (intro1 #'(begin copy.form ...) 'add)])
       #`(define-syntax f
           (functor (quote-syntax #,(intro1 implctx 'add))
                    (quote-syntax #,(intro1 #'(import ...) 'add))
                    (quote ai.allow-implicit?)
                    (list part ... (copy-form (quote-syntax to-copy))))))]))

;; Functor application:
;; - initial scopeset AppInitScs
;; - creates a new (intdef) scope AppSc
;; - fetch implctx, imports from functor (have their original scopes + DefSc)
;; - create implctx-here = apply AppSc to implctx
;;   Can recover AppSc as delta of implctx-here from implctx.
;; - create imports-here = apply AppSc to imports
;;   If AppSc is also applied to functor body, then binding import-here will
;;   capture references to import in body.
;; - exports-here have scopes DefInitScs + AppSc (??)
;; - create exports w/ scopes AppInitSc

;; Note: imports-here also get a scope from enclosing module/defctx!
;; So delay scope math until expander adds scope --- that is, delay
;; until rhs of define-syntaxes.

(define-syntax (apply-functor stx)
  (define-splicing-syntax-class extra-imports
    (pattern (~seq #:implicit [import:id link:id] ...))
    (pattern (~seq) #:with (link ...) '() #:with (import ...) '()))
  (syntax-parse stx
    [(_ (~var fname (static functor? 'functor)) link:id ... extra:extra-imports)
     (define f (attribute fname.value))
     (define implctx (syntax-local-introduce (functor-implctx f)))
     (define imports (syntax-local-introduce (functor-imports f)))
     (unless (or (functor-allow-implicit? f) (null? (syntax->list #'(extra.import ...))))
       (raise-syntax-error #f "functor does not allow implicit linkage" stx #'fname))
     (define intro2 (make-intdefs-syntax-introducer))
     (define imports-here (intro2 imports))
     (define extra-imports-here
       (intro2 (datum->syntax implctx (syntax->datum #'(extra.import ...)))))
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
       [(copy-form form-stx)
        (with-syntax ([form (intro (syntax-local-introduce form-stx))])
          #`(copy-functor-part fname explctx implctx-here form))])]))

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
        #'(#%expression e)])]))

(define-syntax (rename-exports stx)
  (syntax-parse stx
    [(_ explctx (export ...))
     (with-syntax ([(export* ...) (datum->syntax #'explctx (syntax->datum #'(export ...)))])
       #`(define-syntaxes (export* ...)
           (values (make-rename-transformer (quote-syntax export)) ...)))]))
