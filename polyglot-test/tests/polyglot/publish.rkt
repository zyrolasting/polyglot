#lang racket/base

(require rackunit
         aws/s3)

(test-case
  "Mime-type selections"
  (define (test-pair pair)
    (check-equal? ((path->mime-proc) (build-path (car pair))) (cdr pair)))
  (for ([pair '(("a.html" . "text/html")
                ("a.css"  . "text/css")
                ("a.wtf"  . "application/x-unknown-content-type"))])
    (test-pair pair)))
