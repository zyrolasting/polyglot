#lang racket/base

;;; Allows author to embed arbitrary languages in Markdown prose.

(provide run-rackdown default-layout)

(require
  racket/file
  racket/list
  racket/string
  racket/function
  unlike-assets/logging
  "../dynamic-modules.rkt"
  "../../txexpr.rkt"
  "./scripts.rkt")

(define (default-layout kids)
  `(html (head (title "Untitled")) (body . ,kids)))

(define (apply-rackdown tmp-rel elements [initial-layout default-layout])
  (define layout initial-layout)
  (define expanded
    (interlace-txexprs
     elements
     app-script?
     (λ (x)
       (define path (write-script x tmp-rel))
       (define-values (fragment errors) (load-script path))
       (<info "<script> ~a yields fragment:" path)
       (<info "~e" fragment)
       (set! layout (dynamic-require path 'layout (thunk layout)))
       (delete-file path)
       (for ([err errors]) (<error err))
       fragment)))
  (layout expanded))

;; Declare all script nodes expressing inline Racket modules.
(define (with-libraries elements proc)
  (define tmp-rel (make-temp-ephmod-directory))
  (dynamic-wind
    void
    (λ ()
      (proc
        tmp-rel
        (interlace-txexprs
            elements
            lib-script?
            (λ (x)
               (write-script x tmp-rel)
               null))))
    (λ ()
       (delete-directory/files (tmp-rel)))))

(define (run-rackdown elements [initial-layout default-layout])
  (define expanded
    (with-libraries elements (λ (tmp-rel elements)
                               (apply-rackdown tmp-rel
                                               elements
                                               initial-layout))))
  (interlace-txexprs
   expanded
   (λ (x) (and (tag-equal? 'p x)
               (= (length (get-elements x)) 0)))
   (λ _ null)))

(module+ test
  (require rackunit markdown)

  (define elements
    '((p (script ((id "boo") (type "text/racket"))
                 "#lang racket/base"
                 "(provide foo)"
                 "(define foo 1)"))
      (p "The result is")
      (script ((type "application/rackdown"))
         "#lang racket"
         "(require \"./boo.rkt\")"
         "(provide layout)"
         "(write `(span ,(number->string (+ foo 1))))"
         "(define layout (λ (kids) `(body . ,kids)))")))

  (test-equal? "Rackdown"
    (with-handlers ([exn? displayln]) (run-rackdown elements))
    '(body (p "The result is") (span "2"))))
