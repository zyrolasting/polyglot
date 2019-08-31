#lang racket/base

(require racket/list)
(provide (all-defined-out))

(define (page title kids)
  `(html (head (title ,title)
               (meta ((charset "utf-8")))
               (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
               (link ((rel "stylesheet") (type "text/css") (href "styles.css")))) ; <-- polyglot counts styles.css as a dependency.
         (body
           (h1 ,title)
           ,@kids)))

(define (add-newlines lines)
  (map (λ (l) (format "~a~n" l)) lines))

; Note that this returns an application element. `polyglot` will do another pass if
; it sees that application elements produce more application elements. This makes
; it possible to generate more sophisticated content in terms of code...
(define (meta-script id . lines)
  `(script ((type "application/rackdown") (id ,id))
           ,@lines))

; ...such as code samples combined with their actual output.
(define (rackdown-code-sample name . lines)
  `(div ((class "code-sample"))
        (pre . ,(add-newlines lines))
        (output ,(apply meta-script (cons name lines)))))
