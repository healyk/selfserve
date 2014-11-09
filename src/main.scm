(load "src/core.scm")
(load "src/server.scm")
(load "src/request-handlers.scm")
(load "src/mime-type.scm")

(define config-filename "config.scm")

(define (start-default-server)
  (if (not (regular-file? config-filename))
      (call-with-output-file config-filename
        (lambda (out)
          (write-server-config (make-server-config) out))))

  (let ((server-config
         (call-with-input-file config-filename
           (lambda (in)
             (read in)))))
    (start-server (eval server-config))))

(start-default-server)
