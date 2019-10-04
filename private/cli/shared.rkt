#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/dict
         racket/file
         unlike-assets/logging
         "../../main.rkt"
         "../fs.rkt")

(define polyglot-class/cli-asserted (make-parameter #f))
(define test-project-path (system-temp-rel "polyglot-test"))

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


(define (fresh-test-project skel)
  (when (directory-exists? test-project-path)
    (delete-directory/files test-project-path))
  (copy-skeleton skel test-project-path))

(define (make-compiler)
  (new (or (polyglot-class/cli-asserted)
           (dynamic-require (project-rel ".polyglotrc.rkt")
                            'polyglot+%
                            (λ _ polyglot/imperative%)))))

(module+ test
  (require racket/class
           rackunit)

  (for ([workflow '("functional" "imperative")]
        [cls (list polyglot/functional% polyglot/imperative%)])
    (test-case
      (format "Can build test project (~a)" workflow)
      (with-handlers ([exn? (λ (e) ((error-display-handler) (exn-message e) e))])
        (define test-project-path (fresh-test-project workflow))
        (parameterize ([polyglot-project-directory test-project-path])
          (define compiler (make-compiler))
          (clear-distribution!)
          (send compiler add! (assets-rel "index.md"))
          (send compiler compile!)
          (check-equal? (length (directory-list (assets-rel)))
                        (length (directory-list (dist-rel)))))))))
