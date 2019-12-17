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
  file-watchers
  "../../main.rkt"
  "../../paths.rkt"
  "../server.rkt"
  "shared.rkt")

(define (develop)
  (define timeout (make-parameter 500))
  (define port (make-parameter 8080))

  (command-line
    #:program "develop"
    #:once-each
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

    (define (changed? activity)
      (equal? (second activity) 'change))

    (define (removed? activity)
      (equal? (second activity) 'removed))

    (define (filter-activity pred activity-log)
      (map third (filter pred activity-log)))

    (define (get-changed activity-log)
      (filter-activity changed? activity-log))

    (define (get-removed activity-log)
      ; Some editors delete files for a brief moment
      ; when saving (e.g. Emacs). Double check that the
      ; files are still gone.
      (filter (negate file-exists?)
              (filter-activity removed? activity-log)))

    (define (build-with-report changed removed)
      (with-report/counts
        (λ _ (with-handlers ([exn:fail? log-exn])
               (build!
                #:changed changed
                #:removed removed)))))

    (define (on-break e)
      (displayln "Shutting down"))

    (polyglot-project-directory directory)
    (send project ensure-empty-distribution!)
    (build-with-report null null)

    (define watcher (robust-watch (assets-rel)))
    (define (aggregate-changes evt-maker)
      (let loop ([collected null])
        (define alarm (alarm-evt (+ (current-inexact-milliseconds) (timeout))))
        (define res (sync/enable-break alarm (evt-maker)))
        (if (eq? alarm res)
            (let ([len (length collected)])
              (when (> len 0)
                (printf "Aggregated ~a file activity events~n" len))
              collected)
            (loop (cons res collected)))))

    (define aggregator
      (thread (λ _ (let loop ()
        (define activity-log
          (aggregate-changes (λ _ (file-activity-channel))))
        (define changed (get-changed activity-log))
        (define removed (get-removed activity-log))
        (when (> (+ (length changed) (length removed)) 0)
          (build-with-report changed removed))
        (loop)))))

    (define stop
      (start-server (dist-rel) (port)))

    (define info-version ((get-info/full (polyglot-rel)) 'version))
    (with-handlers ([exn:break? (λ _
                                   (printf "~nStopped.~n")
                                   (kill-thread aggregator)
                                   (kill-thread watcher)
                                   (stop)
                                   (exit 0))])
      (printf "~n~n~a~nPolyglot ~a~n" (make-string 40 #\=) info-version)
      (printf "Development server online at http://localhost:~a~n" (port))
      (printf "Press ^C to stop.~n")
      (thread-wait aggregator)
      (thread-wait watcher))))
