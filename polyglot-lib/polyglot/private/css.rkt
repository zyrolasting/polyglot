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
         "../paths.rkt"
         "./dist.rkt")

(define (process-css clear compiler)
  (define rx-matches (call-with-input-file #:mode 'text clear discover-dependencies))
  (define advance (chain write-css clear compiler rx-matches))
  (define unclear-dependencies (map cadr rx-matches))
  (for ([unclear unclear-dependencies])
    (send compiler add! (send compiler clarify unclear) clear (λ _ advance)))
  advance)

(define (write-css clear compiler rx-matches)
  (define css-text (file->string clear))
  (define css-file-name
    (call-with-input-string
      css-text
      (λ (port)
         (path-replace-extension
          (substring (sha1 port) 0 8)
          #".css"))))

  (define replaced-name
    (apply build-path
           (let ([elements (reverse (explode-path clear))])
             (reverse (cons css-file-name
                            (cdr elements))))))

  (define dst (dist-rel (make-dist-path-string replaced-name (assets-rel))))

  (define css-text/rewritten
    (for/fold ([css css-text])
              ([rx-match rx-matches])
      (string-replace css
                      (car rx-match)
                      (format "url('~a')"
                              (find-dist-relative-path/unclear compiler
                                                               dst
                                                               (cadr rx-match))))))

  (with-output-to-dist-file dst
    (λ () (displayln css-text/rewritten))))

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
