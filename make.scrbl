#lang scribble/manual
@(require (for-label (except-in racket/base file) racket/contract racket/unit
                     make make/make-unit make/make-sig
                     make/collection make/collection-sig make/collection-unit
                     dynext/file-sig compiler/sig)
          scribble/core scribble/decode)

@(define (into-nested s)
   (nested-flow (make-style "leftindent" '())
                (if (splice? s)
                    (decode-flow (splice-run s))
                    (list s))))
@(define-syntax-rule (defsubproc . rest)
   (into-nested (defproc . rest)))
@(define-syntax-rule (defsubtogether . rest)
   (into-nested (deftogether . rest)))

@(define raco-manual @other-manual['(lib "scribblings/raco/raco.scrbl")])

@title{Make: Dependency Manager}

The @racketmodname[make] library provides a Racket version of the
popular @exec{make} utility.  Its syntax is intended to imitate the
syntax of @exec{make}, only in Racket.

@table-of-contents[]

@; ----------------------------------------------------------------------

@section[#:tag "overview"]{Overview}

@margin-note{If you want to build Racket modules with automatic
dependency tracking, just use @exec{raco make} as described in
@|raco-manual|.}

If you are already familiar with @exec{make}, skip to the precise
details of the @racketmodname[make] library in @secref["make"]. This
section contains a brief overview of @exec{make} for everyone
else.

When you use @exec{make}, the idea is that you explain how to generate
files in a project from a collection of source files that go through
several stages of processing.

For example, say that you are writing a project that has three input
files (which you create and maintain) called @filepath{a.input},
@filepath{b.input}, and @filepath{c.input}.  Further, there are two
stages of processing: first you run a particular tool
@exec{make-output} that takes an input file and produces an output
file, and then you combine the input files into a single file using
@exec{combine-files}.  Using @exec{make}, you might describe this as:

@verbatim[#:indent 2]{
a.output: a.input
        make-output a.input a.output
b.output: b.input
        make-output b.input b.output
c.output: c.input
        make-output c.input c.output
total: a.output b.output c.output
        combine-files a.output b.output c.output
}

Once you've put this description in a file called @filepath{Makefile}
you can issue the command:

@commandline{make total}

to build your entire project.  The @filepath{Makefile} consists of
several rules that tell @exec{make} how to create each piece of your
project.  For example, the rule that is specified in the first two
lines say that @filepath{a.output} depends on @filepath{a.input} and
the command for making @filepath{a.output} from @filepath{a.input} is

@commandline{make-output a.input a.output}

The main feature of @exec{make} is that it uses the time stamps of
files to determine when a certain step is necessary.  The @exec{make}
utility uses existing programs to build your project --- each rule has
a shell command line.

The @racketmodname[make] library provides similar functionality,
except that the description is in Racket, and the steps that are
needed to build target files are implemented as Racket functions.

Here's a Racket program that is equivalent to the above:

@racketblock[
(require make)

(define (make-output in out)
  ....)

(define (combine-files . args)
  ....)

(make
  (("a.output" ("a.input") (make-output "a.input" "a.output"))
   ("b.output" ("b.input") (make-output "b.input" "b.output"))
   ("c.output" ("c.input") (make-output "c.input" "c.output"))
   ("total" ("a.output" "b.output" "c.output")
            (combine-files "a.output" "b.output" "c.output"))))
]

If you were to fill in the ellipses above with calls to
@racket[system], you'd have the exact same functionality as the
original @filepath{Makefile}.  In addition, you can use
@racket[make/proc] to abstract over the various lines.  For example,
the @filepath{a.output}, @filepath{b.output}, and @filepath{c.output}
lines are very similar so you can write the code that generates those
lines:

@racketblock[
(require make)

(define (make-output in out)
  ....)

(define (combine-files . args)
  ....)

(define files '("a" "b" "c"))
(define inputs  (map (lambda (f) (string-append f ".input")) files))
(define outputs (map (lambda (f) (string-append f ".output")) files))

(define (line file)
  (let ([i (string-append file ".input")]
        [o (string-append file ".output")])
    `(,o (,i) )
    (list o (list i) (lambda () (make-output o i)))))

(make/proc
  `(,@(map (lambda (i o) `(o (,i) ,(lambda () (make-output i o))))
           inputs outputs)
    ("total" ,outputs ,(lambda () (apply combine-files outputs)))))
]

@; ----------------------------------------------------------------------

@section[#:tag "make"]{Make from Dependencies}

@defmodule[make]

@defform[(make ((target-expr (depend-expr ...)
                  command-expr ...)
                ...)
               argv-expr)]{

Expands to

@racketblock[
  (make/proc
   (list (list target-expr (list depend-expr ...)
               (lambda () command-expr ...))
         ...)
   argv-expr)
]}

@defproc[(make/proc [spec (listof
                           (cons/c (or/c (or/c path-string? dependency?)
                                         (listof (or/c path-string? dependency?)))
                                   (cons/c (listof (or/c path-string? dependency?))
                                           (or/c null?
                                                 (list/c (-> any))))))]
                    [argv (or/c string? (vectorof string?) (listof string?))])
         void?]{

Performs a make according to @racket[spec] and using @racket[argv] as
command-line arguments selecting one or more targets.

Each target and dependency can either be a @racket[path-string?] or a
@racket[dependency?]. If it is a @racket[path-string?], it will be
implicitly treated as a filesystem dependency, and it will be wrapped
with @racket[file].

Each element of the @racket[spec] list is a target. A target element
that starts with a list of targets is the same as multiple elements,
one for each dependency. The second element of each target is a list of
dependencies, and the third element (if any) of a target is the
optional command thunk.

To make a target, @racket[make/proc] is first called recursively on
each of the target's dependencies. If a target is not in @racket[spec]
and it exists as a file, then the target is considered made. If a
target's modification date is older than any of its dependencies'
modification dates, the corresponding command thunk is called. If the
dependency has no command thunk then no action is taken; such a target
is useful for triggering the make of other targets (i.e., the
dependencies).

While running a command thunk, @racket[make/proc] catches exceptions
and wraps them in an @racket[exn:fail:make] structure, the raises the
resulting structure.

@history[
 #:changed "1.1" @elem{Added support for @racket[dependency?]
                       targets/dependencies.}]}

@defstruct[(exn:fail:make exn:fail)
           ([targets (listof path-string?)]
            [orig-exn any/c])]{

The @racket[targets] field is a list of strings naming the
target(s), and the @racket[orig-exn] field is the original raised
value.}


@defboolparam[make-print-checking on?]{

A parameter that controls whether @racket[make/proc] prints a message
when making a target. The default is @racket[#t].}


@defboolparam[make-print-dep-no-line on?]{

A parameter that controls whether @racket[make/proc] prints
``checking...'' lines for dependencies that have no target in the
given k@racket[_spec]. The default is @racket[#f].}


@defboolparam[make-print-reasons on?]{

A parameter that controls whether @racket[make/proc] prints the reason
that a command thunk is called. The default is @racket[#t].}

@subsection{Dependency Types}

@subsubsection{Built-In Dependency Types}

@defproc[(file [path path-string?]) dependency?]{

The default dependency type used by @racket[make] and
@racket[make/proc], expresses a dependency on a particular file or
directory on the filesystem. Files’ timestamps are used to determine
whether or not they need to be rebuilt.

@history[#:added "1.1"]}

@defproc[(label [name string?]) dependency?]{

A dependency type that does not represent any particular resource and
will always be built if something depends on it. These are similar to
“phony” targets in @exec{make}.

@history[#:added "1.1"]}

@subsubsection{Custom Dependency Types}

@deftogether[(@defidform[#:kind "generic inferface" gen:dependency]
              @defproc[(dependency? [v any/c]) boolean?])]{

A generic interface for dependency types that can be used with
@racket[make] and @racket[make/proc].

The only @emph{required} method is
@racket[dependency-modification-timestamp]; the others are optional
but overriding them can affect aspects of @racket[make]’s behavior.

In addition to implementing @racket[gen:dependency], it can be
helpful to implement @racket[gen:equal+hash], since @racket[make]
uses @racket[equal?] to determine if a target matches a dependency.

@defsubproc[(dependency-modification-timestamp [dep dependency?])
            (or/c exact-integer? #f)]{

Required. Should return the time the dependency was last modified
in seconds since midnight UTC, January 1, 1970, or @racket[#f] if the
dependency does not exist.}

@defsubproc[(dependency->string [dep dependency?]) string?]{

Optional. Used by @racket[make] and @racket[make/proc] to print the
dependency in diagnostic information that will be seen by the user.

If this method is not implemented, but @racket[dependency-label] is,
this will defer to @racket[dependency-label] to produce a result.
Otherwise, it will return the result of @racket[(format "~a" dep)].}

@defsubproc[(dependency-label [dep dependency?]) string?]{

Optional. Used when determining whether or not a particular string
matches a particular target, which is used by task dependencies as
well as the @racket[argv] argument provided to @racket[make] and
@racket[make/proc].

If this method is not implemented, it will defer to
@racket[dependency->string] to produce a result.}

@defsubtogether[(@defproc[(dependency-pre-build [dep dependency?]) any]
                 @defproc[(dependency-post-build [dep dependency?]) any])]{

Optional. By default, these methods do nothing, but they are called by
@racket[make] and @racket[make/proc] just before and just after a target
is built. They are intended to be used as hooks that dependency types can
use to be informed about parts of the build process.}

@history[#:added "1.1"]}

@; ----------------------------------------

@subsection[#:tag "make-signature"]{Signature}

@margin-note{@racket[make^] and @racket[make@] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[make] module.}

@defmodule[make/make-sig]

@defsignature[make^ ()]{

Includes all of the names provided by @racketmodname[make].}

@; ----------------------------------------

@subsection[#:tag "make-unit"]{Unit}

@defmodule[make/make-unit]

@defthing[make@ unit?]{

A unit that imports nothing and exports @racket[make^].}

@; ----------------------------------------------------------------------

@section[#:tag "setup-extension"]{Building Native-Code Extensions}

@defmodule[make/setup-extension]

The @racketmodname[make/setup-extension] library helps compile C code
via Setup PLT's ``pre-install'' phase (triggered by a
@racketidfont{pre-install-collection} item in @filepath{info.rkt}; see
also @secref[#:doc '(lib "scribblings/raco/raco.scrbl")
"setup-info"]).

The @racket[pre-install] function takes a number of arguments that
describe how the C code is compiled---mainly the libraries that it
depends on. It then drives a C compiler via the
@racketmodname[dynext/compile] and @racketmodname[dynext/link]
functions.

Many issues can complicate C compilation, and the @racket[pre-install]
function helps with a few:

@itemize[

   @item{finding non-standard libraries and header files,}

   @item{taming to some degree the differing conventions of Unix and
     Windows, }

   @item{setting up suitable dependencies on Racket headers, and}

   @item{using a pre-compiled binary when a @filepath{precompiled}
     directory is present.}

]

Many extension installers will have to sort out addition platform
issues manually, however. For example, an old @filepath{readline}
installer used to pick whether to link to @filepath{libcurses} or
@filepath{libncurses} heuristically by inspecting
@filepath{/usr/lib}. More generally, the ``last chance'' argument to
@racket[pre-install] allows an installer to patch compiler and linker
options (see @racketmodname[dynext/compile] and
@racketmodname[dynext/link]) before the C code is compiled or linked.

@defproc[(pre-install
               [plthome-dir path-string?]
               [collection-dir path-string?]
               [c-file path-string?]
               [default-lib-dir path-string?]
               [include-subdirs (listof path-string?)]
               [find-unix-libs (listof string?)]
               [find-windows-libs (listof string?)]
               [unix-libs (listof string?)]
               [windows-libs (listof string?)]
               [extra-depends (listof path-string?)]
               [last-chance-k ((-> any) . -> . any)]
               [3m-too? any/c #f])
          void?]{

The arguments are as follows:

@itemize[

  @item{@racket[plthome-dir] --- the directory provided to a `pre-installer'
    function.}

  @item{@racket[collection-dir] --- a directory to use as the current directory
    while building.}

  @item{@racket[c-file] --- the name of the source file (relative to
    @racket[collection-dir]). The output file will be the same, except
    with a @filepath{.c} suffix replaced with @racket[(system-type
    'so-suffix)], and the path changed to @racket[(build-path
    "compiled" "native" (system-library-subpath))].

    If @racket[(build-path "precompiled" "native"
    (system-library-subpath) (path-replace-suffix c-file (system-type
    'so-suffix)))] exists, then @racket[c-file] is not used at all,
    and the file in the @filepath{precompiled} directory is simply
    copied.}

  @item{@racket[default-lib-dir] --- a default directory for finding
    supporting libraries, often a subdirectory of
    @filepath{collection-dir}. The user can supplement this path by
    setting the @indexed-envvar{PLT_EXTENSION_LIB_PATHS} environment
    variable, which applies to all extensions manged by
    @racket[pre-install].}

  @item{@racket[include-subdirs] --- a list of relative paths in which
    @tt{#include} files will be found; the path will be determined
    through a search, in case it's not in a standard place like
    @filepath{/usr/include}.

    For example, the list used to be @racket['("openssl")] for the
    @filepath{openssl} collection, because the source uses
    @tt{#include <openssl/ssl.h>} and @tt{#include <openssl/err.h>}.}

  @item{@racket[find-unix-libs] --- like @racket[include-subdirs], but
    a list of library bases. Leave off the @filepath{lib} prefix and
    any suffix (such as @filepath{.a} or @filepath{.so}). For
    @filepath{openssl}, the list used to be @racket['("ssl"
    "crypto")]. Each name will essentially get a @tt{-l} prefix for
    the linker command line.}

  @item{@racket[find-windows-libs] --- like @racket[find-unix-libs],
    but for Windows.  The library name will be suffixed with
    @filepath{.lib} and supplied directly to the linker.}

  @item{@racket[unix-libs] --- like @racket[find-unix-libs], except
    that the installer makes no attempt to find the libraries in a
    non-standard place. For example, the @filepath{readline} installer
    used to supply @racket['("curses")].}

  @item{@racket[windows-libs] --- like @racket[unix-libs], but for
    Windows. For example, the @filepath{openssl} installer used to
    supply @racket['("wsock32")].}

  @item{@racket[extra-depends] --- a list of relative paths to treat as
    dependencies for compiling @filepath{file.c}. Often this list will
    include @filepath{file.c} with the @filepath{.c} suffix replaced by
    @filepath{.rkt}.  For example, the "openssl" installer supplies
    @racket['("mzssl.rkt")] to ensure that the stub module
    @filepath{mzssl.rkt} is never used when the true extension can be
    built.}

  @item{@racket[last-chance-k] --- a procedure of one argument, which
    is a thunk.  This procedure should invoke the thunk to make the
    file, but it may add parameterizations before the final build. For
    example, the @filepath{readline} installer used to add an
    AIX-specific compile flag in this step when compiling on AIX.}

  @item{@racket[3m-too?]--- a boolean. If true, when the 3m variant is
    installed, use the equivalent to @exec{raco ctool --xform} to transform
    the source file and then compile and link for 3m. Otherwise, the
    extension is built only for CGC when the CGC variant is installed.}

]}

@; ----------------------------------------------------------------------

@section[#:tag "collection"]{Making Collections}

@defmodule[make/collection]

@defproc[(make-collection [collection-name any/c]
                          [collection-files (listof path-string?)]
                          [argv (or/c string? (vectorof string?))])
         void?]{

Builds bytecode files for each file in @racket[collection-files],
writing each to a @filepath{compiled} subdirectory and automatically
managing dependencies. Supply @racket['#("zo")] as @racket[argv] to
compile all files. The @racket[collection-name] argument is used only
for printing status information.

Compilation is performed as with @exec{raco make} (see
@|raco-manual|).}

@subsection{Signature}

@margin-note{@racket[make:collection^] and @racket[make:collection@] are
deprecated. They exist for backward-compatibility and will likely be
removed in the future. New code should use the
@racketmodname[make/collection] module.}

@defmodule[make/collection-sig]

@defsignature[make:collection^ ()]{

Provides @racketidfont{make-collection}.}

@subsection{Unit}

@defmodule[make/collection-unit]

@defthing[make:collection@ unit?]{

Imports @racket[make^], @racket[dynext:file^], and @racket[compiler^],
and exports @racket[make:collection^].}
