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


(define (make-compiler project)
  (new (or (polyglot-class/cli-asserted)
           (send project
                 get-workflow-class
                 (λ _ polyglot/imperative%)
                 #:live? #t))))

(define (get-entry-asset project compiler path)
  (if (send project asset-path? path)
      path
      (send compiler clarify "index.md")))

(define (init-by-user-path! path)
  (define normalized (path->complete-path (resolve-path path)))
  (define project (find-closest-project normalized))
  (unless project
    (displayln (format "Cannot find project from path:~n  path: ~a" path)
               (current-error-port))
    (exit 1))

  (polyglot-project-directory (get-field directory project))
  (send project ensure-empty-distribution!)
  (define compiler (make-compiler project))
  (values project
          compiler
          (get-entry-asset project compiler normalized)))
