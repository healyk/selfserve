(require-extension srfi-69)

(define %mime-type-db (make-hash-table))
(define (get-mime-type extension)
  (hash-table-ref/default %mime-type-db
                          (if (string? extension)
                              (string->keyword extension)
                              extension)
                          "plain/text"))

;;
;; Defines a new mime type
;;
;;  file-extension -> extension for files of this type, as a keyword
;;                    example: #:html => "text/html"
;;  type           -> Text type: "text/html"
;;
(define (define-mime-type file-extension type)
  (if (list? file-extension)
      (for-each (lambda (extension) (define-mime-type extension type))
                file-extension)
      (hash-table-set! %mime-type-db file-extension type)))

(define-mime-type '(#:html #:htm #:shtml) "text/html")
(define-mime-type #:js   "application/javascript")
(define-mime-type #:json "application/json")
(define-mime-type #:gif  "image/gif")
(define-mime-type '(#:jpeg #:jpg) "image/jpeg")
(define-mime-type #:png  "image/png")
(define-mime-type #:svg  "image/svg+xml")
(define-mime-type #:txt  "text/plain")
(define-mime-type #:xml  "text/xml")
