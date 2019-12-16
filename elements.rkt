#lang racket/base

;;; Integrate <script> nodes in Tagged X-expressions with Racket modules.

(provide load-script
         write-script
         script-of-type?
         app-script?
         lib-script?
         make-temp-ephmod-directory)

(require racket/list
         racket/port
         racket/string
         racket/function
         racket/file
         unlike-assets/logging
         "./private/dynamic-modules.rkt"
         "./paths.rkt"
         "./txexpr.rkt")

(define (make-temp-ephmod-directory)
  ;; Creates a temp directory in the temporary file system containing
  ;; a symbolic link to the project directory. This lets dynamic modules
  ;; use the project while leveraging tempfs and OS-specific cleanup jobs.
  (define temp-dir (make-temporary-file "ephmod~a" 'directory (system-temp-rel)))
  (make-file-or-directory-link (project-rel)
                               (build-path temp-dir "project"))
  (path-rel temp-dir))

(define (script->path script dir)
  (define path
    (with-handlers ([exn:fail? (λ _ (make-temporary-file "script-element-cdata~a" #f dir))])
                   (build-path dir (string->path (attr-ref script 'id)))))
  (path-replace-extension path #".rkt"))

(define (script-of-type? type x)
  (and (list? x)
       (equal? (get-tag x) 'script)
       (equal? type (attr-ref x 'type #f))))

(define (app-script? x)
  (or (script-of-type? "application/rackdown" x)
      (script-of-type? "application/racket" x)))

(define lib-script? (curry script-of-type? "text/racket"))

(define (app-or-lib-script? x)
  (or (lib-script? x)
      (app-script? x)))

(define (load-script path)
  (define-values (readable-stdout _ readable-stderr)
    (instantiate-ephemeral-module path))
    (values (port->list read readable-stdout)
            (port->list read readable-stderr)))

(define (write-script script dir)
  (let ([path (script->path script dir)])
    (lines->file/clobber path (get-text-elements script))
    (<info "Wrote script: ~a" path)
    path))

(module+ test
  (require rackunit
           "./paths.rkt")

  (test-case "load-script"
    (define element
      '(script ((id "blah"))
               "#lang racket"
               "(write \"x\" (current-output-port))"
               "(write \"y\" (current-output-port))"
               "(write \"a\" (current-error-port))"
               "(write \"b\" (current-error-port))"))
    (define spath (script->path element (system-temp-rel)))
    (write-script element (system-temp-rel))
    (define-values (fragment errors) (load-script spath))
    (check-equal? fragment '("x" "y"))
    (check-equal? errors '("a" "b"))

    (check-true (file-exists? spath))
    (delete-file spath))

  (test-equal? "Derive filesystem paths from script txexpr"
    (script->path '(script ((id "coolio"))) (project-rel))
    (project-rel "coolio.rkt")))
