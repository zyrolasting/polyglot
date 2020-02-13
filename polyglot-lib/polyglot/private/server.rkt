#lang racket

(provide start-server)
(require racket/runtime-path
         net/url
         web-server/web-server
         web-server/http/response-structs
         web-server/http/request-structs
         web-server/dispatchers/filesystem-map
         web-server/private/util
         web-server/private/mime-types
         (prefix-in lifter:
                    web-server/dispatchers/dispatch-lift)
         (prefix-in sequencer:
                    web-server/dispatchers/dispatch-sequencer)
         (prefix-in files:
                    web-server/dispatchers/dispatch-files))

(define-runtime-path mime-types-file "./mime.types")

(define (show-not-found req)
  (define message (format "Cannot find ~v" (url->string (request-uri req))))
  (response/full
   404 #"Not Found"
   (current-seconds) #"text/html; charset=utf-8"
   '()
   (list (string->bytes/utf-8
          (format "<html><head><title>~a</title></head><body>~a</body></html>"
                  message
                  message)))))

(define (start-server static-files-dir [port 8080])
  (serve #:dispatch (sequencer:make
                     (files:make #:url->path (make-url->path static-files-dir)
                                 #:path->mime-type (make-path->mime-type mime-types-file))
                     (lifter:make show-not-found))
         #:port port))
