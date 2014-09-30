(load "src/core.scm")
(load "src/server.scm")
(load "src/request-handlers.scm")
(load "src/mime-type.scm")

(define (start-default-server)
  (start-server (make-server-config)))

(start-default-server)
