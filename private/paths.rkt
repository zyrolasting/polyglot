#lang racket/base

(require racket/contract
         racket/runtime-path
         unlike-assets)
(provide (all-defined-out))

(define polyglot-project-directory (make-parameter (current-directory)))

; Use to bind path building functions
(define (path-rel . head)
  (λ tail (simplify-path
             (cleanse-path (apply build-path (append head tail)))
             #f)))

(define-runtime-path polyglot-runtime-path "..")

(define (project-path-builder next)
  (λ rest (apply (path-rel (polyglot-project-directory) next)
                 rest)))

; No enforcement here since that already comes with build-path
; Provide for convenience in documentation and user-authored contracts.
(define path-el/c (and/c (or/c path-for-some-system?
                               path-string?)
                         (not/c complete-path?)))

(define project-rel     (project-path-builder "."))
(define assets-rel      (project-path-builder "assets"))
(define dist-rel        (project-path-builder "dist"))
(define polyglot-rel    (path-rel polyglot-runtime-path))
(define system-temp-rel (path-rel (find-system-path 'temp-dir)))
