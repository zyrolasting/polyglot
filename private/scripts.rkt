#lang racket/base

;;; Integrate <script> nodes in Tagged X-expressions with Racket modules.

(provide
  load-script
  write-script
  script-of-type?
  app-script?
  lib-script?)

(require
  racket/list
  racket/string
  racket/function
  racket/file
  "./dynamic-modules.rkt"
  "../txexpr.rkt")

(define get-script-cdata (curry filter string?))

(define (script->path script rel)
  (define path
    (with-handlers ([exn:fail? (λ _ (make-temporary-file "script-element-cdata~a" #f (rel)))])
                   (rel (string->path (attr-ref script 'id)))))
  (path-replace-extension path #".rkt"))


(define (script-of-type? type x)
  (and (list? x)
       (equal? (get-tag x) 'script)
       (equal? type (attr-ref x 'type #f))))

(define (app-script? x)
  (or (script-of-type? "application/rackdown" x)
      (script-of-type? "application/racket" x)))

(define lib-script? (curry script-of-type? "text/racket"))

(define (pipe-port->data port)
  (reverse
    (let loop ([next (read port)] [data null])
      (if (not (eof-object? next))
          (loop (read port) (cons next data))
          data))))

(define (load-script path)
  (define-values (readable-stdout _ readable-stderr)
    (instantiate-ephemeral-module path))
    (values (pipe-port->data readable-stdout)
            (pipe-port->data readable-stderr)))

(define (write-script script rel)
  (let ([path (script->path script rel)])
    (lines->file/clobber path (get-script-cdata script))
    path))

(module+ test
  (require
    rackunit
    "./paths.rkt")

  (test-case "load-script"
    (define element
      '(script ((id "blah"))
               "#lang racket"
               "(write \"x\" (current-output-port))"
               "(write \"y\" (current-output-port))"
               "(write \"a\" (current-error-port))"
               "(write \"b\" (current-error-port))"))
    (define spath (script->path element system-temp-rel))
    (write-script element system-temp-rel)
    (define-values (fragment errors) (load-script spath))
    (check-equal? fragment '("x" "y"))
    (check-equal? errors '("a" "b"))

    (check-true (file-exists? spath))
    (delete-file spath))

  (test-equal? "Derive filesystem paths from script txexpr"
    (script->path '(script ((id "coolio"))) project-rel)
    (project-rel "coolio.rkt")))
