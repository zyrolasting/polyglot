#lang racket/base

(require
  raco/command-name
  racket/cmdline
  racket/function
  racket/vector
  unlike-assets/logging
  unlike-assets
  file-watchers
  "../../main.rkt"
  "../fs.rkt"
  "../paths.rkt"
  "build.rkt"
  "develop.rkt"
  "publish.rkt"
  "demo.rkt")

(show-debug?       #f)
(show-colors?      #t)
(show-prefix?      #f)
(show-all-events?  #f)
(show-level?       #f)
(error-port-levels '(fatal error))
(prescribed-prefix "")
(format-clear      basename)
(format-unclear    identity)

(define help-table `#hash(
  ("develop" . "Build website in response to changes")
  ("publish" . "Publish website to S3 using your credentials file")
  ("demo"    . "Run demo page and place README.html in working directory")
  ("build"   . "Build the website once")))
(define action-table `#hash(
  ("develop" . ,develop)
  ("publish" . ,publish)
  ("demo"    . ,demo)
  ("build"   . ,build)))

(define (show-subcommands . _)
  (printf "Available subcommands:~n")
  (for ([(subcmd-name desc) (in-hash help-table)])
       (printf "  raco polyglot ~a~a~a~n"
               subcmd-name
               (make-string (- 12 (string-length subcmd-name)) #\space)
               desc)))

(define (get-subcommand-args action)
  (define args (current-command-line-arguments))
  (vector-drop args (+ 1 (vector-member action args))))

(void
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-v" "--verbose")       "Include debug-level logging in output"
                              (show-debug? #t)]
    #:args (action . _)
    (parameterize ([current-command-line-arguments (get-subcommand-args action)])
      ((hash-ref action-table action (λ _ show-subcommands))))))
