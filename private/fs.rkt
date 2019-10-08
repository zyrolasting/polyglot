#lang racket/base

(require racket/contract
         "../paths.rkt")
(provide
 (contract-out
  [copy-skeleton (-> path-string?
                     (or/c path-string?
                           path?)
                     (or/c path-string?
                           path?))]
  [basename (-> path? string?)]
  [extname  (-> path? string?)]
  [empty-directory (-> path? any/c)]))

(require racket/file
         racket/path)

(define (basename path)
  (path->string (file-name-from-path path)))

(define (extname path)
  (bytes->string/utf-8 (path-get-extension path)))

(define (empty-directory path)
  (delete-directory/files path #:must-exist? #f)
  (make-directory path))

(define (copy-skeleton name dest)
  (copy-directory/files (polyglot-rel "private"
                                      "skel"
                                      name)
                        dest)
  dest)
