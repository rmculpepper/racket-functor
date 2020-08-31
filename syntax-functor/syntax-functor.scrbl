#lang scribble/manual
@(require racket/list scribble/example
          (for-label racket/base))

@title[#:tag "syntax-functor"]{Syntax Functors: Parameterized Sets of Macro Definitions}

@(define the-eval (make-base-eval))
@(the-eval '(require syntax-functor))

@defmodule[syntax-functor]

A @deftech{syntax functor} is a named set of macro definitions
parameterized over the bindings of a given set of identifiers.

For example, here is a syntax functor named @racket[myand-functor]
parameterized by the identifier named @racket[myif]:

@examples[#:eval the-eval #:label #f
(define-functor (myand-functor myif)
  (define-syntax myand
    (syntax-rules ()
      [(_ e) (#%expression e)]
      [(_ e1 e ...) (myif e1 (myand e ...) #f)])))
]

Here is an example of applying the functor, where @racket[myif] is
bound to Racket's @racket[if] form:

@examples[#:eval the-eval #:label #f
(let ()
  (apply-functor myand-functor if)
  (list (myand 1 2 3)
        (myand 1 2 #f (println 'here!))))
]

Here is another example, where @racket[myif] is bound to a function
that acts like a strict version of @racket[if]:

@examples[#:eval the-eval #:label #f
(let ()
  (define (if* a b c) (if a b c))
  (apply-functor myand-functor if*)
  (list (myand 1 2 3)
        (myand 1 2 #f (println 'here!))))
]

@bold{Recursive linking} A functor import can be satisfied by one of
its own exports. For example, here is a variant of the previous
functor that recurs through a separate identifier, @racket[recur]. By
linking the exported @racket[myand] macro with the @racket[recur]
import, we create a recursive macro:

@examples[#:eval the-eval #:label #f
(define-functor (myand-rec-functor myif recur)
  (define-syntax myand
    (syntax-rules ()
      [(_ e) (#%expression e)]
      [(_ e1 e ...) (myif e1 (recur e ...) #f)])))
(let ()
  (apply-functor myand-rec-functor if myand)
  (list (myand 1 2 3)
        (myand 1 2 #f (println 'here!))))
]

@bold{Implicit linking} A functor can support implicit linking with
the @racket[#:allow-implicit] keyword. Then an application site can
choose any identifier to be linked as if it were an import.

@examples[#:eval the-eval #:label #f
(define-functor (myand-imp-functor #:allow-implicit)
  (define-syntax myand
    (syntax-rules ()
      [(_ e) (#%expression e)]
      [(_ e1 e ...) (if e1 (myand e ...) #f)])))
(let ()
  (define (if* a b c) (if a b c))
  (apply-functor myand-imp-functor #:implicit [if if*] [#%expression list])
  (list (myand 1 2 3)
        (myand 1 2 #f (println 'here!))))
]


@; ------------------------------------------------------------

@defform[(define-functor (functor-id import-id ... maybe-implicit)
           macro-def ...
           maybe-copy)
         #:grammar ([maybe-implicit (code:line) (code:line #:allow-implicit)]
                    [maybe-copy (code:line) (code:line #:copy copy-form ...)])]{

Defines @racket[functor-id] as a @tech{syntax functor} parameterized
over the bindings of the @racket[import-id]s. If
@racket[#:allow-implicit] is included after the import list, then the
functor allows an application site to treat @emph{any} identifier as
an implicit import.

The @racket[macro-def]s are the functor's macro definitions. Each
@racket[macro-def] is fully expanded when the functor is
defined. References to imported identifiers are only allowed in syntax
templates, and the expansion of the @racket[macro-def]s must not
depend on the imported bindings. For example, the following two
functor definitions are not allowed:

@racketblock[
(define-functor (bad-functor define-macro) (code:comment "BAD")
  (define-macro (thunk e) (lambda () e)))
(define-functor (also-bad-functor define-thing) (code:comment "BAD")
  (define-syntax-rule (mydefine x e) (define-thing x e))
  (mydefine x 12))
]

Following the macro definitions is an optional ``copy'' section; any
forms following the @racket[#:copy] keyword are copied to the
functor's application sites.
}

@defform[(apply-functor functor-id link-id ... maybe-implicit-link)
         #:grammar ([maybe-implicit-link
                     (code:line)
                     (code:line #:implicit [implicit-id implicit-link-id] ...)])]{

Imports @racket[functor-id]'s definitions, where each of the functor's
@racket[import-id]s is linked with the application's
@racket[link-id]. If the functor allows implicit linking, then each
@racket[implicit-id] is linked with the corresponding
@racket[implicit-link-id] as if @racket[implicit-id] were listed in
the functor's import list. If the functor does not allow implicit
linking, then a syntax error is raised if there are any implicit
linking clauses.
}
