#lang racket/base

;; Responsible for preparing ephemeral dynamic modules.
;; Right now I leverage the file system until I learn
;; how to generate modules that use #lang in memory.

(provide lines->file/clobber
         instantiate-ephemeral-module
         with-ephemeral-module)

(require racket/file
         "../paths.rkt")

(define (dynamic-require/multi module-path syms)
 (foldl
   (lambda (s r) (cons
                   (if (pair? s)
                       (dynamic-require module-path (car s) (cdr s))
                       (dynamic-require module-path s))
                   r))
   '()
   (reverse syms)))

(define (lines->file/clobber path lines)
  (display-lines-to-file #:exists 'truncate/replace
                         #:mode 'text
                         lines
                         path))

(define (instantiate-ephemeral-module path #:input [make-input void])
  (define-values (oi oo) (make-pipe #f))
  (define-values (ii io) (make-pipe #f))
  (define-values (ei eo) (make-pipe #f))
  (parameterize ([current-output-port oo]
                 [current-input-port ii]
                 [current-error-port eo])
    (dynamic-wind
      void
      (λ ()
         (make-input io) ; In case a top-level form blocks for input.
         (dynamic-require path #f)
         (values oi io ei))
      (λ ()
        (close-output-port oo)
        (close-output-port io)
        (close-output-port eo)))))

(define (with-ephemeral-module path syms proc #:input [make-input void])
  (define-values (oi io ei) (instantiate-ephemeral-module path #:input make-input))
  (define bound (dynamic-require/multi path syms))
  (apply proc (append bound (list oi io ei))))

(define (with-lines->file path lines proc)
  (dynamic-wind
    (λ () (lines->file/clobber path lines))
    proc
    (λ () (delete-file path))))
