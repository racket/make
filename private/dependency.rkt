#lang racket/base

(require racket/contract
         racket/generic)

(provide gen:dependency dependency? dependency/c
         dependency-modification-timestamp dependency->string dependency-label
         dependency-pre-build dependency-post-build
         (contract-out [file (path-string? . -> . any)]
                       [label (string? . -> . any)]))

(define-generics dependency
  ; dependency -> (or/c exact-integer #f)
  (dependency-modification-timestamp dependency)
  ; dependency -> string
  (dependency->string dependency)
  ; dependency -> string
  (dependency-label dependency)

  ; dependency -> any
  (dependency-pre-build dependency)
  (dependency-post-build dependency)

  #:defined-predicate dependency-method-implemented?

  #:fallbacks
  [(define/generic -dependency->string dependency->string)
   (define/generic -dependency-label dependency-label)
   (define (dependency->string dep)
     (if (dependency-method-implemented? dep 'dependency-label)
         (-dependency-label dep)
         (format "~a" dep)))
   (define (dependency-label dep)
     (-dependency->string dep))

   (define dependency-pre-build void)
   (define dependency-post-build void)])

;; dependency on a file
(struct file (path)
  #:transparent
  #:methods gen:dependency
  [(define (dependency-modification-timestamp dep)
     (let ([path (file-path dep)])
       (and (or (directory-exists? path) (file-exists? path))
            (file-or-directory-modify-seconds path))))

   (define (dependency->string dep)
     (let ([path (file-path dep)])
       (if (path? path)
           (path->string path)
           path)))])

;; “phony” / transient dependency
(struct label (name [timestamp #:mutable])
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-label
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?)
     (equal? (label-name a) (label-name b)))
   (define (hash-proc l hash-code)
     (hash-code (label-name l)))
   (define (hash2-proc l hash-code)
     (hash-code (label-name l)))]
  #:methods gen:dependency
  [(define (dependency-modification-timestamp l)
     (label-timestamp l))
   (define (dependency-post-build l)
     (set-label-timestamp! l (current-seconds)))
   (define (dependency-label l)
     (label-name l))])

(define (label name)
  (make-label name #f))
