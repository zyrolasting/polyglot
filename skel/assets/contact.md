<script type="application/rackdown">
#lang racket/base
(require "project/vcomps.rkt") ; "project" is a symbolic link that points to where the project lives.
(provide layout)
(define layout (λ (kids) (page "Send a message" kids)))
</script>
<form action="/send" method="post">
  <textarea style="width: 100%"></textarea>
  <button style="display: block; padding: 1rem" type="submit">Send</button>
</form>
