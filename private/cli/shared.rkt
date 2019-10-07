#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/dict
         racket/file
         racket/rerequire
         unlike-assets/logging
         "../../main.rkt"
         "../fs.rkt")

(define polyglot-class/cli-asserted (make-parameter #f))

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


(define (clear-distribution!)
  (make-directory* (dist-rel))
  (empty-directory (dist-rel)))

(define (make-compiler)
  (new (or (polyglot-class/cli-asserted)
           (let ([rcfile (project-rel ".polyglotrc.rkt")])
             (dynamic-rerequire rcfile)
             (dynamic-require rcfile
                              'polyglot+%
                              (λ _ polyglot/imperative%))))))
