#lang racket/unit

(require "make-sig.rkt"
         "private/dependency.rkt")

(import)
(export make^)

(define make-print-checking    (make-parameter #t))
(define make-print-dep-no-line (make-parameter #t))
(define make-print-reasons     (make-parameter #t))
(define make-notify-handler    (make-parameter void))

(define-struct line (targets      ; (list-of string)
                     dependencies ; (list-of dependency)
                     command))    ; (union thunk #f)
(define-struct (exn:fail:make exn:fail) (target orig-exn))

;; check-spec : TST -> (non-empty-list-of line)
;; throws an error on bad input
(define (spec->lines spec)
  (define (path/string/dependency? x)
    (or (path-string? x) (dependency? x)))
  (define (->dependency x)
    (if (or (string? x) (path? x)) (file x) x))
  (define (->dependency/string x)
    (if (or (path? x)) (file x) x))
  (define (err s p) (error 'make/proc "~a: ~e" s p))
  (unless (and (list? spec) (pair? spec))
    (err "specification is not a non-empty list" spec))
  (for/list ([line spec])
    (unless (and (list? line) (<= 2 (length line) 3))
      (err "line is not a list with 2 or 3 parts" line))
    (let* ([name (car line)]
           [tgts (if (list? name) name (list name))]
           [deps (cadr line)]
           [thunk (and (pair? (cddr line)) (caddr line))])
      (define (err s p) (error 'make/proc "~a: ~e for line: ~a" s p name))
      (unless (andmap path/string/dependency? tgts)
        (err "line does not start with a path/string/dependency or list of paths/string/dependencies"
             line))
      (unless (list? deps) (err "second part of line is not a list" deps))
      (for ([dep deps])
        (unless (path/string/dependency? dep)
          (err "dependency item is not a path/string/dependency" dep)))
      (unless (or (not thunk)
                  (and (procedure? thunk) (procedure-arity-includes? thunk 0)))
        (err "command part of line is not a thunk" thunk))
      (make-line (map ->dependency tgts) (map ->dependency/string deps) thunk))))

;; (union path/string/dep (vector-of path/string/dep) (list-of path/string/dep))
;; -> (list-of (union string dependency))
;; throws an error on bad input
(define (argv->args x)
  (let ([args (cond [(list? x) x]
                    [(vector? x) (vector->list x)]
                    [else (list x)])])
    (map (lambda (a)
           (cond [(or (string? a) (dependency? a)) a]
                 [(path? a) (file a)]
                 [else (raise-type-error
                        'make/proc "path/string/dependency or path/string/dependency vector or list"
                        x)]))
         args)))

;; make/proc :
;; spec (union path/string/dep (vector-of path/string/dep) (list-of path/string/dep))
;; -> void
;; effect : make, according to spec and argv. See docs for details
(define (make/proc spec [argv '()])
  (define made null)
  (define lines (spec->lines spec))
  (define args (argv->args argv))
  (define (->string s)
    (if (string? s) s (dependency->string s)))
  (define (make-dependency s indent)
    (define dep+line
      (for/or ([line (in-list lines)])
        (let ([dep (for/or ([dep (in-list (line-targets line))])
                     (and (or (equal? s dep)
                              (and (string? s)
                                   (string=? s (dependency-label dep))))
                          dep))])
          (and dep (cons dep line)))))
    (define-values [dep line] (if dep+line
                                  (values (car dep+line) (cdr dep+line))
                                  (values (and (dependency? s) s) #f)))
    (define date (and dep (dependency-modification-timestamp dep)))
    (when (and (make-print-checking) (or line (make-print-dep-no-line)))
      (printf "make: ~achecking ~a\n" indent (->string s))
      (flush-output))
    (if (not line)
      (unless date (error 'make "don't know how to make ~a" (->string s)))
      (let* ([deps (line-dependencies line)]
             [command (line-command line)]
             [indent+ (string-append indent " ")]
             [dep-dates (for/list ([s* deps])
                          (define dep* (make-dependency s* indent+))
                          (or (dependency-modification-timestamp dep*)
                              (error 'make "dependency ~a was not made\n"
                                     (dependency->string dep*))))]
             [reason (or (not date)
                         (ormap (lambda (dep ddate) (and (> ddate date) dep))
                                deps dep-dates))])
        (when (and reason command)
          (set! made (cons dep made))
          ((make-notify-handler) dep)
          (printf "make: ~amaking ~a~a\n"
                  (if (make-print-checking) indent "")
                  (dependency->string dep)
                  (cond [(not (make-print-reasons)) ""]
                        [(not date) (format " because ~a does not exist" (dependency->string dep))]
                        [else (format " because ~a changed" (->string reason))]))
          (flush-output)
          (with-handlers ([exn:fail?
                           (lambda (exn)
                             (raise (make-exn:fail:make
                                     (format "make: failed to make ~a; ~a"
                                             (dependency->string dep) (exn-message exn))
                                     (exn-continuation-marks exn)
                                     (line-targets line)
                                     exn)))])
            (dependency-pre-build dep)
            (command)
            (dependency-post-build dep)))))
    dep)
  (for ([f (if (null? args) (list (car (line-targets (car lines)))) args)])
    (make-dependency f ""))
  (for ([item (reverse made)]) (printf "make: made ~a\n" (dependency->string item)))
  (flush-output))
