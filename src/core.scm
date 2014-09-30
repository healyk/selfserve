;;;;
;;;; core.scm
;;;;
;;;; Core utilities
;;;;

(require-extension srfi-1)

;;
;; Concatinates a series of strings together.  It will stringify any
;; parameters passed in it can.
;;
(define (stringify s1 . rest)
  (define (stringify-param param)
    (cond
     ((char? param)    (string param))
     ((number? param)  (number->string param))
     ((keyword? param) (string-append "#:" (keyword->string param)))
     ((symbol? param)  (symbol->string param))
     ((list? param)    (list->string param))
     ((boolean? param)
      (if param "true" "false"))
     ((condition? param)  (exn->string param))
     (else param)))
  (apply string-append (map stringify-param (cons s1 rest))))

(define (downcase str)
  (list->string (map char-downcase (string->list str))))

(define (upcase str)
  (list->string (map char-upcase (string->list str))))


;;;
;;; Exception stringification
;;;

(define (exn-message->string exn)
  ((condition-property-accessor 'exn 'message) exn))

(define (exn-location->string exn)
  (with-output-to-string
    (lambda ()
      (write ((condition-property-accessor 'exn 'location) exn)))))

(define (exn-arguments->string exn)
  (with-output-to-string
    (lambda ()
      (write ((condition-property-accessor 'exn 'arguments) exn)))))

;;
;; Provides very simple stringifcation.
;;
(define (exn->string exn)
  (let* ((message   (exn-message->string exn))
         (location  (exn-location->string exn))
         (arguments (exn-arguments->string exn)))
    (string-append "Message: " message "\n"
                   "Location: " location "\n"
                   "Arguments: " arguments)))

;;
;; Replaces characters in an string with the html-escaped equivalent
;;
(define (escape-html str)
  (define html-escape-codes
    '((#\< "&lt;")
      (#\> "&gt;")))
  (apply stringify
         (map (lambda (chr)
                (let ((replace-chr (assoc chr html-escape-codes)))
                  (if replace-chr
                      (cadr replace-chr)
                      chr)))
              (string->list str))))
