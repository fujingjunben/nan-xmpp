#lang racket/base
(require racket/list
	 racket/contract
         racket/string
         net/base64) 

(provide (contract-out [hash-append (hash? hash? . -> . hash?)]
		       [read-async (port? . -> . string?)]
		       [read-async-bytes (port? . -> . (listof byte?))])
         debug? debugf
         clean sasl-plain-string)

;;;; ;  ;;  ;
;; 
;; debugging
;;
;;;;  ;  ;

(define debug? #t)

(define debugf
  (case-lambda 
    ((str) (when debug? (printf str)))
    ((str . dir) (when debug? (apply printf (cons str dir))))))

;;;;;;;;;;; ; ;;;;  ;   ;;; ;    ; ;;     ;
;;
;; networking
;;
;;;;;; ;;  ;;  ;  ; ;   ;

(define (read-async in)
  (bytes->string/utf-8 (list->bytes (read-async-bytes in))))

(define (read-async-bytes in)
  (let ((bstr '()))
    (when (sync/timeout 0 in)
      (set! bstr (cons (read-byte in) (read-async-bytes in))))
    bstr))

;;; other
;; sasl authentication string
(define (sasl-plain-string username password)
  (let ((s (bytes->string/utf-8
            (base64-encode
             (string->bytes/utf-8
              (string-append "\u0000"
                             username
                             "\u0000"
                             password))))))
    (substring s 0 (- (string-length s) 2)))) ;; remove CRLF in the end of string

(define (hash-append hs-1 hs-2)
  (if (= 0 (hash-count hs-2))
      hs-1
      (let* ([first-key (first (hash-keys hs-2))]
	     [first-value (hash-ref hs-2 first-key)])
	(hash-append (hash-set hs-1 first-key first-value) (hash-remove hs-2 first-key)))))

(define (remove-xml-header str)
  (regexp-replace (regexp "<\\?.+\\?>") str ""))

(define (remove-stream-header str)
  (regexp-replace (regexp "<stream:stream[^<>]+>") str ""))

(define (clean str)
  (string-trim (remove-stream-header (remove-xml-header str))))
