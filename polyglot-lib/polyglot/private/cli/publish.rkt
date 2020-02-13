#lang racket/base

(provide publish)
(require
  racket/port
  racket/set
  racket/list
  racket/cmdline
  raco/command-name
  unlike-assets/logging
  aws/s3
  aws/keys
  "../../main.rkt"
  "../../paths.rkt"
  "shared.rkt")

(define bucket (make-parameter "default-bucket"))
(define dry-run? (make-parameter #f))
(define delete-diff? (make-parameter #f))
(define region (make-parameter "us-east-2"))

(define (bucket-rel key) (format "~a/~a" (bucket) key))
(define (path->bucket+key path)
  (define-values (_ name must-be-dir?) (split-path path))
  (bucket-rel name))

(define (put-file path)
  (define media-type ((path->mime-proc) path))
  (define bucket+key (path->bucket+key path))
  (put bucket+key
       (λ (to-amazon)
          (call-with-input-file
            path
            (λ (from-disk)
               (copy-port from-disk to-amazon))))
        (file-size path)
        media-type
        (hasheq 'Content-Type media-type
                'x-amz-acl 'public-read
                'Content-Disposition "inline"
                'Cache-Control (format "max-age=~e"
                                       (if (equal? media-type "text/html") 0 31536000)))))

(define (compute-∃-diff local remote)
  (set->list
    (set-subtract (list->set remote)
                  (list->set local))))

(define (delete-unused keys)
  (unless (dry-run?) (delete-multiple (bucket) keys))
  (for ([key keys])
    (<info "DELETE ~a" key)))

(define (upload-distribution local)
  (for ([file-name local])
    (define path (dist-rel file-name))
    (unless (dry-run?) (put-file path))
    (<info "PUT ~a" (path->bucket+key path))))

(define (publish-website)
  (define remote (ls (bucket-rel "")))
  (define local (directory-list (dist-rel)))
  (define ∃-diff (compute-∃-diff (map path->string local) remote))
  (upload-distribution local)
  (when (and (delete-diff?) (not (empty? ∃-diff)))
    (delete-unused ∃-diff)))

(define (publish)
  (command-line
    #:program "publish"
    #:usage-help
    "Publish website."
    #:once-each
    [("-d" "--dry-run") ("Don't send PUT or DELETE requests to S3, but log what they would be.")
                        (dry-run? #t)]
    [("--delete-diff")  ("DELETE assets on the bucket that are not built locally")
                        (delete-diff? #t)]
    [("-r" "--region")  reg ("Set AWS S3 region. Default: us-east-2")
                        (region reg)]
    #:args (dir s3-bucket-name)
      (polyglot-project-directory (path->complete-path (simplify-path dir)))
      (bucket s3-bucket-name))

  (read-keys/aws-cli)
  (s3-region (region))

  (exit (with-handlers ([exn? (λ (e) (log-exn e) 1)])
          (with-report/void publish-website)
          0)))
