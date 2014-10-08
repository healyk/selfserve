(require-extension posix files
                   srfi-4
                   srfi-13)

;;
;; Creates a well-constructed path from various file arguments
;;
(define (join-path first . rest)
  ;; Takes a path - a string - and removes any dot references.
  (define (normalize-path path)
    (let ((split-paths (string-split path "/")))
      (fold (lambda (result path)
              (string-append path "/" result))
              ""
              (filter (lambda (x)
                        (not (or (string=? x ".") (string=? x ".."))))
                      split-paths))))

  (apply string-append
         (cons "." (map normalize-path (cons first rest)))))

;;
;; Reads all the data in a file.  Assumes it's binary for safety
;;
(define (read-all-from-file filename)
  (let* ((fd  (file-open filename (+ open/rdonly open/binary)))
         (len (file-size fd))
         (buf (make-blob len)))
    (file-read fd len buf)
    (file-close fd)
    (blob->u8vector buf)))

(define (read-file-into-response response filename)
  (http-response-body-set! response (read-all-from-file filename)))

;;
;; Since html files can be specified as .html or .htm or .shtml, some
;; files may need to be searched for.  This looks though the
;; files and search for an existing file with one of those extensions,
;; in that order.
;;
(define (resolve-html-file full-path)
  (find regular-file? (map (lambda (ex)
                             (pathname-replace-extension full-path ex))
                           (list "html" "htm" "shtml"))))

;;;
;;; Request Handlers
;;;
;;; The request handlers are chanined via chain-request-handlers.
;;; This creates a new request handler which will call the given
;;; handlers in the order passed to it.  Any handler that returns a
;;; 404 will cause the next handler to be called to search for that
;;; resource.
;;;

(define (chain-request-handlers handler-first . rest-handlers)
  (let ((handlers (cons handler-first rest-handlers)))
    (lambda (server-config request response)
      (let handler-iter ((current (car handlers))
                         (rest    (cdr handlers)))
        (current server-config request response)
        (if (= (car (http-response-status response)) (car status:not-found))
            (if (not (null? rest))
                (handler-iter (car rest)
                              (cdr rest))))))))

;;
;; Serves up a file out of the www root directory.
;;
(define (file-request-handler server-config request response)
  (let* ((public-path       (server-config-root-dir server-config))
         (request-path      (http-request-path request)))
    (let server-lambda ((full-request-path (join-path public-path request-path)))
      (let ((mime-type (pathname-extension full-request-path)))
        (cond
         ((regular-file? full-request-path)
          (begin
            (log-verbose "Serving file " full-request-path)
            (read-file-into-response response full-request-path)
            (http-response-header-set! response
                                       Content-Type: (get-mime-type mime-type))
            (http-response-status-set! response status:ok)))
         ((directory? full-request-path)
          (begin
            (log-debug "Searching for index file")
            (server-lambda (resolve-html-file
                       (join-path full-request-path "index.foo")))))
         (else
          (begin
            (log-warning "Could not find file " full-request-path)
            (http-response-status-set! response status:not-found))))))))

;;
;; Serves up a 404 page.
;;
(define (404-handler server-config request response)
  (let* ((public-path (server-config-root-dir server-config))
         (404-file    (resolve-html-file
                       (join-path public-path "404.foo"))))
    (if (regular-file? 404-file)
        (begin
          (log-verbose "Serving 404 file")
          (read-file-into-response response 404-file)
          (http-response-header-set! response
                                     Content-Type: (get-mime-type #:html))))))

;;
;; Creates a request handler that rejects the listed methods with
;; a 405
;;
(define (make-method-rejection-handler method . rest)
  (let ((methods (cons method rest)))
    (lambda (server-config request response)
      (let ((request-method (http-request-method request)))
        (if (any (lambda (method)
                   (eq? method request-method))
                 methods)
            (begin
              (log-warning "Rejecting method " request-method)
              (http-response-status-set! response
                                         status:method-not-allowed)))))))

(define default-request-handler
  (chain-request-handlers
   (make-method-rejection-handler
    #:options #:head #:post #:put #:delete #:trace #:connect #:patch)
   file-request-handler
   404-handler))
