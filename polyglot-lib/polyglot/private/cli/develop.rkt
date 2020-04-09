#lang racket/base

(provide develop)
(require
  raco/command-name
  racket/class
  racket/function
  racket/list
  racket/cmdline
  racket/file
  setup/getinfo
  unlike-assets/logging
  unlike-assets
  "../../main.rkt"
  "../../paths.rkt"
  "../server.rkt"
  "shared.rkt")

(define (develop)
  (define timeout (make-parameter 500))
  (define port (make-parameter 8080))
  (define start-server? (make-parameter #t))

  (command-line
    #:program "develop"
    #:once-each
    [("-n" "--no-server") "Set to prevent launching dev server."
                          (start-server? #f)]
    [("-p" "--port") user-port
                     "The port to use for the dev server. Default: 8080."
                     (port (string->number user-port))]
    [("--delay") ms
                 "The number of milliseconds to allow between changes "
                 "before trying to compile again. Default: 500."
                 (timeout (string->number ms))]
    #:args (path)
    (polyglot-live? #t)
    (define-values (directory project build!) (init-from-user-path path))

    (define (build-with-report key)
      (with-report/counts
        (λ _ (with-handlers ([exn:fail? log-exn])
               (build! key)))))

    (define (on-break e)
      (displayln "Shutting down"))

    (polyglot-project-directory directory)
    (send project ensure-empty-distribution!)
    (build-with-report null null)

    (define stop
      (if (start-server?)
          (start-server (dist-rel) (port))
          void))

    (define info-version ((get-info/full (polyglot-rel)) 'version))
    (with-handlers ([exn:break? (λ _
                                   (printf "~nStopped.~n")
                                   (stop)
                                   (exit 0))])
      (printf "~n~n~a~nPolyglot ~a~n" (make-string 40 #\=) info-version)
      (printf "Distribution: ~a~n" (dist-rel))
      (when (start-server?)
        (printf "Development server online at http://localhost:~a~n" (port)))
      (printf "Press ^C to stop.~n"))))
