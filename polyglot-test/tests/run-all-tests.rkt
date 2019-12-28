#lang racket/base

(module+ test
  (require rackunit/text-ui
           "all-polyglot-tests.rkt")
  (run-tests all-web-server-tests))
