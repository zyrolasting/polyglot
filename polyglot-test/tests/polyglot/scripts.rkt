#lang racket/base

(require rackunit
         racket/port
         polyglot/paths
         polyglot/elements)

(require/expose polyglot/elements (script->path))

(test-case "load-script"
  (define element
    '(script ((id "blah"))
             "#lang racket"
             "(write \"x\" (current-output-port))"
             "(write \"y\" (current-output-port))"
             "(write \"a\" (current-error-port))"
             "(write \"b\" (current-error-port))"))
  (define spath (script->path element (system-temp-rel)))
  (write-script element (system-temp-rel))
  (define-values (o i e) (load-script spath))
  (define-values (fragment errors) (values (port->list read o)
                                           (port->list read e)))
  (check-equal? fragment '("x" "y"))
  (check-equal? errors '("a" "b"))

  (check-true (file-exists? spath))
  (delete-file spath))

(test-equal? "Derive filesystem paths from script txexpr"
             (script->path '(script ((id "coolio"))) (project-rel))
             (project-rel "coolio.rkt"))
