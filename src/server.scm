(require-extension tcp srfi-1 posix)

(define version "0.2")

;;;
;;; Server configuration
;;;
(define-record server-config
  port
  log-filename

  ; Lambda to handle requests.  Takes two arguments: request response
  request-handler

  ; Directory where static documents are stored.
  root-dir)

(define %make-server-config make-server-config)
(define (make-server-config #!key (port 3456)
                                  (log-filename "log/server.log")
                                  (request-handler default-request-handler)
                                  (root-dir "public"))
  (%make-server-config port
                       log-filename
                       request-handler
                       root-dir))

(define listener '())
(define server-running #f)

;;
;; Various log messages and levels
;;
(define longest-level-length (string-length "[critical] "))
(define (log level msg . rest)
  (let ((log-message (string-append "[" (keyword->string level) "] "
                                    (apply stringify (cons msg rest))))
        (out         (log-file)))
    (display (string-append log-message "\n"))
    (if (not (null? out))
        (begin
          (write-line log-message out)
          (flush-output out)))))

(define (log-debug    msg . rest) (apply log (append (list #:debug msg) rest)))
(define (log-verbose  msg . rest) (apply log (append (list #:verbose msg) rest)))
(define (log-info     msg . rest) (apply log (append (list #:info msg) rest)))
(define (log-warning  msg . rest) (apply log (append (list #:warning msg) rest)))
(define (log-error    msg . rest) (apply log (append (list #:error msg) rest)))
(define (log-critical msg . rest) (apply log (append (list #:critical msg) rest)))

(define log-file
  (let ((log-file-port '()))
    (lambda (#!optional (filename '()))
      (if (not (null? filename))
          (let ((fd (file-open filename (+ open/wronly open/append open/creat))))
            (set! log-file-port (open-output-file* fd))))
      log-file-port)))

;;
;; Creates a new server thread
;;
(define (start-server server-config)
  (if (not server-running)
      (begin
        (log-info "=== Starting Server on port " (server-config-port server-config))
        (setup-server server-config)
        (run-server server-config))
      (log-warning "Server already started")))

;;
;; Stops an existing server from running
;;
(define (stop-server)
  (if server-running
      (begin
        (log-info "Shutting down server.")
        (set! server-running #f)
        (tcp-close listener)
        (set! listener '()))))

(define (setup-server server-config)
  (set! server-running #t)
  (log-file (server-config-log-filename server-config))
  (set! listener (tcp-listen (server-config-port server-config))))

;;;
;;; HTTP Requests
;;;
(define-record http-request
  raw-text

  ; This is the method: GET, HEAD, POST, PUT...
  method

  ; Url path requested
  path

  ; Http version, such as "http/1.1"
  version-string
  version

  ; alist of header fields.  Keys are keywords
  ; http://en.wikipedia.org/wiki/List_of_HTTP_header_fields
  headers)

(define http-header-terminator
  (list->string (list #\return #\newline #\return #\newline)))
(define http-line-terminator (list->string (list #\return #\newline)))

;;
;; Turns a string into an http-request
;;
(define (string->http-request str)
  (define (make-headers lst results)
    (if (null? lst)
        results
        (let* ((head  (string-split (car lst) ": "))
               (rest  (cdr lst))
               (key   (string->keyword (car head)))
               (value (cadr head)))
          (make-headers rest
                        (cons (list key value)
                              results)))))
  
  (let ((lines (string-split str http-line-terminator)))
    (let ((first-line (string-split (car lines) " "))
          (headers    (drop lines 1)))
      (make-http-request str
                         (string->keyword (downcase (car first-line)))
                         (cadr first-line)
                         (caddr first-line)
                         (string->number (cadr (string-split (caddr first-line) "/")))
                         (make-headers headers '())))))

;;
;; Reads in an http request from an input port.  Will return a http-request.
;;
(define (read-http-request in)
  (define terminating-seq (reverse (string->list http-header-terminator)))
  
  (define (end-of-request? lst current)
    (if (> (length lst) 2)
        (equal? terminating-seq
                (cons current (take lst 3)))
        #f))

    (let loop ((results '())
               (current-char (read-char in)))
      (if (and (not (eof-object? current-char))
               (not (end-of-request? results current-char)))
          (loop (cons current-char results)
                (read-char in))
          (string->http-request (list->string (reverse results))))))

;;;
;;; HTTP Response
;;;
(define status:ok '(200 "OK"))
(define status:not-found '(404 "Not Found"))
(define status:method-not-allowed '(405 "Method Not Allowed"))
(define status:internal-server-error '(500 "Internal Server Error"))

(define-record http-response
  version-string

  ; Status codes: http://en.wikipedia.org/wiki/List_of_HTTP_status_codes
  ; Needs to be a 2 element list: first the status number and then the
  ; name.  Example: '(200 "OK")
  status

  ; Headers to send back.  alist with keyword - string pairs.
  ; http://en.wikipedia.org/wiki/List_of_HTTP_header_fields
  headers

  ; Body of the response
  body)

(define %make-http-response make-http-response)
(define (make-http-response #!optional (status status:not-found))
  (%make-http-response "HTTP/1.1"
                       status
                       (list (list Server: (string-append "SelfServe " version))
                             (list Content-Type: "text/html; charset=UTF-8")
                             (list Date: (seconds->string)))
                       ""))

(define %http-response-body-set! http-response-body-set!)
(define (http-response-body-set! response new-body)
  (let ((len (cond
              ((string? new-body)   (string-length new-body))
              ((u8vector? new-body) (u8vector-length new-body))
              (else
               (log-critical "Can't get length of " new-body)
               (abort "Bad Response Body")))))
    (%http-response-body-set! response new-body)
    (http-response-header-set! response Content-Length: (number->string len))))

(define (write-http-response out response)
  (define (header->string header)
    (string-append (keyword->string (car header))
                   ": "
                   (cadr header)))

  (define (join-headers headers)
    (reduce (lambda (result header)
              (string-append result "\r\n" header))
            "" headers))

  (let* ((status     (http-response-status response))
         (first-line (string-append (http-response-version-string response) " "
                                    (number->string (car status)) " "
                                    (cadr status)))
         (headers    (join-headers (map header->string
                                        (http-response-headers response))))
         (body       (http-response-body response)))
    (write-line first-line out)
    (write-line headers out)
    (write-line "" out)
    (cond
     ((string? body)   (write-line body out))
     ((u8vector? body) (write-u8vector body out)))
    (write-line http-header-terminator out)))

;;
;; Serves up an exception as the response
;;
(define (serve-exp-response exp out)
  (define (create-body)
    (if (condition? exp)
        (let ((message   (escape-html (exn-message->string exp)))
              (arguments (escape-html (exn-arguments->string exp)))
              (location  (escape-html (exn-location->string exp))))
          (stringify
           "<html><head><title>Error</title></head><body>"
           "<h1>Exception</h1>"
           "Message: "
           "<span class='exception-message'>" message "</span><br />"
           "Arguments: "
           "<span class='exception-arguments'>" arguments "</span><br />"
           "Location: "
           "<span class='exception-location'>" location "</span><br />"
           "</body></html>"))
        (stringify exp)))

  (log-error "Caught exception when serving request: " exp)
  (let ((response (make-http-response status:internal-server-error)))
    (http-response-body-set! response (create-body))
    (write-http-response out response)))

;;
;; Adds or modifies an existing header with new data
;;
(define (http-response-header-set! response header-key header-value)
  (let* ((headers (http-response-headers response))
         (current (assoc header-key headers)))
    (http-response-headers-set! response
     (cons (list header-key header-value)
           (if (list? current)
               (delete current headers)
               headers)))))

;;
;; Performs actual work for the server
;;
(define (run-server server-config)
  (define (cleanup-ports in out)
    (close-input-port in)
    (close-output-port out))

  (define (process-tcp-request in out)
    (let ((request         (read-http-request in))
          (response        (make-http-response))
          (request-handler (server-config-request-handler server-config)))
      (log-info (upcase (keyword->string (http-request-method request)))
                " request for "
                (http-request-path request))
      
      (request-handler server-config request response)
      (log-debug "Request handler finished.")
      (write-http-response out response)))
  
  (define (server-iter)
    (if (tcp-accept-ready? listener)
        (let-values (((in out) (tcp-accept listener))
                     ((exp-handler) (current-exception-handler)))
          (call-with-current-continuation
           (lambda (cc)
             (with-exception-handler
              (lambda (exp)
                (with-exception-handler exp-handler
                                        (lambda ()
                                          (log-debug "Double exception")
                                          (serve-exp-response exp out)
                                          (cc '())))
                (cc '()))
              (lambda ()
                (process-tcp-request in out)))))
          (cleanup-ports in out)))
    (if server-running
        (server-iter)))
  
  (if server-running
      (server-iter)
      (log-critical "Server not started, cannot process work.")))

