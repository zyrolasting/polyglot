#lang racket/base

(require racket/class
         racket/file
         racket/path
         unlike-assets/logging)
(provide (all-defined-out))

(define (unclear-asset-path->dist-path compiler unclear-asset-path)
  (send compiler
        lookup
        (send compiler
              clarify
              unclear-asset-path)))

(define (find-dist-relative-path compiler dist-resource-path unclear-asset-path)
  (let-values ([(base name _) (split-path dist-resource-path)])
    (path->string (find-relative-path base
                                      (unclear-asset-path->dist-path compiler
                                                                     unclear-asset-path)))))

(define (with-output-to-dist-file path proc)
  (make-parent-directory* path)
  (with-output-to-file path #:exists 'replace proc)
  (<info "Wrote ~a" path)
  path)
