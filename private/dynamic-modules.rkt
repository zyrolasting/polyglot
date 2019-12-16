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

(module+ test
  (require rackunit)

  (test-case "Ephemeral dynamic modules from string content"
    (define module-cdata
        '("#lang racket/base"
          "(provide (all-defined-out))"
          "(define b (read))"
          "(define a 1)"
          "(define c 3)"
          "(write \"o\" (current-output-port))"
          "(write \"e\" (current-error-port))"))

    (define path (make-temporary-file))
    (with-lines->file
      path
      module-cdata
      (lambda ()
        (with-ephemeral-module
          path
          #:input (lambda (port) (write "2" port))
          `(b c a (d . ,(λ _ "thunked")))
          (lambda (b c a d output-port input-port error-port)
            (check-equal? b "2")
            (check-equal? c 3)
            (check-equal? a 1)
            (check-equal? d "thunked")
            (check-equal? (read error-port) "e")
            (check-equal? (read output-port) "o")))))))
