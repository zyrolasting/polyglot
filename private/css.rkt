#lang racket/base

;; Responsible for default handling of CSS, in which
;; url() expressions are dependency references.

(provide process-css)
(require racket/class
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         file/sha1
         unlike-assets/policy
         unlike-assets/logging
         "../paths.rkt")

(define (process-css clear compiler)
  (define rx-matches (call-with-input-file #:mode 'text clear discover-dependencies))
  (define advance (chain write-css clear compiler rx-matches))
  (define unclear-dependencies (map cadr rx-matches))
  (for ([unclear unclear-dependencies])
    (send compiler add! (send compiler clarify unclear) clear (λ _ advance)))
  advance)

(define (write-css clear compiler rx-matches)
  (define css-text
    (for/fold ([css (file->string clear)])
              ([rx-match rx-matches])
      (string-replace css
                      (car rx-match)
                      (format "url('~a')"
                              (make-dist-path-string
                               (send compiler
                                     lookup
                                     (send compiler
                                           clarify
                                           (cadr rx-match))))))))
  (define dst
    (call-with-input-string
      css-text
      (λ (port)
        (dist-rel
         (path-replace-extension
          (substring (sha1 port) 0 8)
          #".css")))))
  (with-output-to-file dst #:exists 'replace
    (λ () (displayln css-text)))
  (<info "Wrote ~a" dst)
  dst)

(define (discover-dependencies in)
  (filter-map
   (λ (match-list)
     (define encoded (if (bytes? (car match-list))
                         (map bytes->string/utf-8 match-list)
                         match-list))
     (and (local-asset-url? (cadr encoded))
          encoded))
   (regexp-match* #px"url\\([\"']?([^\\)\"']+)[\"']?\\)"
                  in
                  #:match-select values)))
