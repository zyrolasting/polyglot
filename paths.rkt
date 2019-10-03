#lang racket/base

(require racket/contract
         racket/runtime-path
         unlike-assets)

(define path-el/c (and/c (or/c path-for-some-system?
                               path-string?)
                         (not/c complete-path?)))
(define path-builder/c (->* () #:rest (listof path-el/c) complete-path?))
(provide path-el/c
         (contract-out
          [path-rel (->* () #:rest (listof path-for-some-system?) path-builder/c)]
          [polyglot-project-directory (parameter/c (and/c complete-path? directory-exists?))]
          [polyglot-temp-directory (parameter/c (and/c complete-path? directory-exists?))]
          [project-rel path-builder/c]
          [assets-rel path-builder/c]
          [dist-rel path-builder/c]
          [polyglot-rel path-builder/c]
          [system-temp-rel path-builder/c]))

(define polyglot-project-directory (make-parameter (current-directory)))
(define polyglot-temp-directory (make-parameter (find-system-path 'temp-dir)))

; Use to bind path building functions
(define (path-rel . head)
  (λ tail (simplify-path
             (cleanse-path (apply build-path (append head tail)))
             #f)))

(define-runtime-path polyglot-runtime-path "..")

(define (path-builder base-rel next)
  (λ rest (apply (path-rel (base-rel) next)
                 rest)))

(define (project-path-builder next)
  (path-builder polyglot-project-directory next))
(define (temp-path-builder next)
  (path-builder polyglot-temp-directory next))

(define project-rel     (project-path-builder "."))
(define assets-rel      (project-path-builder "assets"))
(define dist-rel        (project-path-builder "dist"))
(define polyglot-rel    (path-rel polyglot-runtime-path))
(define system-temp-rel (temp-path-builder "."))
