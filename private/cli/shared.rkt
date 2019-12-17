#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/dict
         racket/file
         racket/rerequire
         unlike-assets/logging
         "../../main.rkt")

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

(define (make-compiler project)
  (make-polyglot-workflow-object project
                                 #:live? #t
                                 #:force-workflow (polyglot-class/cli-asserted)
                                 #:fallback-workflow polyglot/imperative%))

(define (get-entry-asset project path)
  (if (send project asset-path? path)
      (path->string (simplify-path path))
      "index.md"))

(define (init-from-user-path path)
  (define normalized (path->complete-path (resolve-path path)))
  (define project (find-closest-project normalized))
  (define directory (get-field directory project))
  (unless project
    (displayln (format "Cannot find project from path:~n  path: ~a" path)
               (current-error-port))
    (exit 1))

  (values
   directory
   project
   (make-polyglot-builder
    directory
    #:entry-assets (list (get-entry-asset project normalized)))))
