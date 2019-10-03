#lang racket/base

(provide (all-defined-out))

(require racket/dict
         unlike-assets/logging
         "../../workflows/imperative.rkt")

(define polyglot-class (make-parameter polyglot/imperative%))

(define (log-exn e)
  (define string-port (open-output-string))
  (parameterize ([current-error-port string-port])
    ((error-display-handler) (exn-message e) e))

  (<error (get-output-string string-port)))

(define (report+summary proc)
  (define counts (with-report/counts proc))
  (define nwarnings (dict-ref counts 'warning 0))
  (define nerrors (+ (dict-ref counts 'error 0)
                     (dict-ref counts 'fatal 0)))
  (with-report/void
    (λ ()
      (<info "# warnings: ~a" nwarnings)
      (<info "# errors:   ~a" nerrors)))

  (exit (if (> nerrors 0) 1 0)))
