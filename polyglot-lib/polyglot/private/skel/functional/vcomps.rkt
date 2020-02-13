#lang racket/base

(require racket/list polyglot)
(provide (all-defined-out))

(define (page title initial-page-tx)
  `(html (head (title ,title)
               (meta ((charset "utf-8")))
               (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
               ; polyglot counts styles.css as a dependency.
               (link ((rel "stylesheet") (type "text/css") (href "styles.css"))))
         (body
           (h1 ,title)
           . ,(get-elements (findf-txexpr initial-page-tx
                                          (λ (x) (tag-equal? 'body x)))))))

(define (add-newlines lines)
  (map (λ (l) (format "~a~n" l)) lines))

; Note that this returns an application element. `polyglot` will do another pass if
; it sees that application elements produce more application elements. This makes
; it possible to generate more sophisticated content in terms of code...
(define (meta-script id . lines)
  `(script ((type "application/racket") (id ,id))
           . ,lines))

; ...such as code samples combined with their actual output.
(define (rackdown-code-sample name . lines)
  `(div ((class "code-sample"))
        (pre . ,(add-newlines lines))
        (output ,(apply meta-script (cons name lines)))))
