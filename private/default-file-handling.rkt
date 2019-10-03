#lang racket/base

(provide copy-hashed)

;; Use copy-hashed to copy a file to a distribution
;; with a basename equal to the first 8 characters
;; of the SHA-1 hash of the file's contents. This
;; acts as a fallback handling method when no
;; special processing is warranted.

(require
  racket/file
  racket/path
  file/sha1
  unlike-assets
  "../paths.rkt")

(define (get-output-path path)
  (call-with-input-file
    path
    (λ (port)
      (dist-rel
        (path-replace-extension
          (substring (sha1 port) 0 8)
          (path-get-extension path))))))

(define (copy-hashed src avail)
  (with-handlers ([exn:fail:filesystem? (λ (e) e)])
    (let* ([dst (get-output-path src)])
      (unless (file-exists? dst)
        (copy-file src dst))
      dst)))
