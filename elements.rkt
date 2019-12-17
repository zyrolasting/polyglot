#lang racket/base

;;; Integrate <script> nodes in Tagged X-expressions with Racket modules.

(require racket/contract)
(provide
 (contract-out
  [script-element? (-> any/c boolean?)]
  [load-script (-> path? (values input-port? output-port? input-port?))]
  [write-script (-> script-element? path? path?)]
  [script-element-of-type? (-> string? any/c boolean?)]
  [app-element? (-> any/c boolean?)]
  [lib-element? (-> any/c boolean?)]))

(require racket/list
         racket/string
         racket/function
         racket/file
         unlike-assets/logging
         "./private/dynamic-modules.rkt"
         "./paths.rkt"
         "./txexpr.rkt")

(define (script-element? x)
  (tag-equal? 'script x))

(define (script->path script dir)
  (define path
    (with-handlers ([exn:fail? (λ _ (make-temporary-file "script-element-cdata~a" #f dir))])
                   (build-path dir (string->path (attr-ref script 'id)))))
  (path-replace-extension path #".rkt"))

(define (script-element-of-type? type x)
  (and (list? x)
       (equal? (get-tag x) 'script)
       (equal? type (attr-ref x 'type #f))))

(define (app-element? x)
  (or (script-element-of-type? "application/rackdown" x)
      (script-element-of-type? "application/racket" x)))

(define lib-element? (curry script-element-of-type? "text/racket"))

(define (app-or-lib-element? x)
  (or (lib-element? x)
      (app-element? x)))

(define (load-script path [make-input void])
  (instantiate-ephemeral-module path #:input make-input))

(define (write-script script dir)
  (let ([path (script->path script dir)])
    (lines->file/clobber path (get-text-elements script))
    (<info "Wrote script: ~a" path)
    path))

(module+ test
  (require rackunit
           racket/port
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
    (define-values (o i e) (load-script spath))
    (define-values (fragment errors) (values (port->list read o)
                                             (port->list read e)))
    (check-equal? fragment '("x" "y"))
    (check-equal? errors '("a" "b"))

    (check-true (file-exists? spath))
    (delete-file spath))

  (test-equal? "Derive filesystem paths from script txexpr"
    (script->path '(script ((id "coolio"))) (project-rel))
    (project-rel "coolio.rkt")))
