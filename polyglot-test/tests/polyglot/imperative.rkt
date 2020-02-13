#lang racket/base

(require racket/class
         racket/file
         racket/string
         rackunit
         markdown/parse
         polyglot/imperative
         polyglot/paths
         polyglot/txexpr)

(require/expose polyglot/imperative
                (run-rackdown
                 fallback-provided-name))

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
             '(body (p "The result is") (span "2")))

(test-case
  "Can preprocess tagged X-expressions from asset folder"
  (define provided-name "replacer")
  (define (empty-directory path)
    (delete-directory/files path #:must-exist? #f)
    (make-directory path))

  (define (e2e provided-name)
    (define compiler (new polyglot/imperative%))
    (define module-name (format "macro-~a" provided-name))
    (define tmpdir (system-temp-rel "pg-test"))

    (empty-directory tmpdir)
    (parameterize ([polyglot-project-directory tmpdir])
      (empty-directory (assets-rel))
      (define module-path (assets-rel (format "~a.rkt" module-name)))

      (display-lines-to-file
       `("#lang racket/base"
         "(require txexpr)"
         ,(format "(provide ~a)" provided-name)
         ,(format "(define (~a other)" provided-name)
         "  `((p ,(attr-ref other 'data-foo))))")
       module-path)

      ; We don't want to specify the compiler's fallback name in the input code.
      ; How else would we know if the compiler used the fallback correc
      (define input-code
        `((meta ((itemprop "build")
                 (data-foo "made it")
                 (data-macro ,(format "~a ~a"
                                      module-name
                                      (if (equal? provided-name fallback-provided-name)
                                          ""
                                          provided-name)))))))

      (define changed
        (car (send compiler
                   preprocess-txexprs
                   input-code)))

      (check-true (and (txexpr? changed)
                       (equal? (get-tag changed) 'p)
                       (equal? (car (get-elements changed))
                               "made it")))))

  (e2e "different-name")
  (e2e fallback-provided-name))


(test-case "Can process Racket+Markdown externally"
  (define tmp (make-temporary-file))
  (delete-file tmp)
  (make-directory tmp)
  (parameterize ([current-directory tmp]
                 [polyglot-project-directory tmp])
    (display-lines-to-file
     '("#lang racket/base"
       "(provide datum)"
       "(define datum '(p \"test!\"))")
     "datum.rkt")

    (define rackdown
      (string-join
       '("Hello"
        "<script type=\"text/racket\" id=\"lib\">"
        "#lang racket/base"
        "(require \"project/datum.rkt\")"
        "(provide from-lib)"
        "(define from-lib datum)"
        "</script>"
        "<script type=\"application/rackdown\">"
        "#lang racket/base"
        "(require \"lib.rkt\")"
        "(write from-lib)"
        "</script>")
       "\n"))

    (check-equal?
        (run-txexpr/imperative! (parse-markdown rackdown)
                                (λ (kids)
                                  `(body . ,kids)))
        '(body (p "Hello") (p "test!")))))
