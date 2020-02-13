#lang racket/base

(require rackunit
         racket/file
         polyglot/private/dynamic-modules)

(require/expose polyglot/private/dynamic-modules
                (with-lines->file))

(test-case "Ephemeral dynamic modules from string content"
  (define module-cdata
    '("#lang racket/base"
      "(provide (all-defined-out))"
      "(define b (read))"
      "(define a 1)"
      "(define c 3)"
      "(write \"o\" (current-output-port))"
      "(write \"e\" (current-error-port))"))

  (define path (make-temporary-file))
  (with-lines->file
    path
    module-cdata
    (lambda ()
      (with-ephemeral-module
        path
        #:input (lambda (port) (write "2" port))
        `(b c a (d . ,(λ _ "thunked")))
        (lambda (b c a d output-port input-port error-port)
          (check-equal? b "2")
          (check-equal? c 3)
          (check-equal? a 1)
          (check-equal? d "thunked")
          (check-equal? (read error-port) "e")
          (check-equal? (read output-port) "o"))))))
