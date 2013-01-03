#lang racket/base
(require racket/list
	 racket/contract) 

(provide (contract-out [hash-append (hash? hash? . -> . hash?)]
		       [read-async (port? . -> . string?)]
		       [read-async-bytes (port? . -> . (listof byte?))])
         debug? debugf)

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
(define (hash-append hs-1 hs-2)
  (if (= 0 (hash-count hs-2))
      hs-1
      (let* ([first-key (first (hash-keys hs-2))]
	     [first-value (hash-ref hs-2 first-key)])
	(hash-append (hash-set hs-1 first-key first-value) (hash-remove hs-2 first-key)))))
