<script type="application/racket" id="contact-main">
#lang racket/base
(require polyglot
         "project/vcomps.rkt")

(provide replace-page)
(define (replace-page page-tx)
  (page "Send a message" page-tx))
</script>

<form action="/send" method="post">
  <textarea style="width: 100%"></textarea>
  <button style="display: block; padding: 1rem" type="submit">Send</button>
</form>
