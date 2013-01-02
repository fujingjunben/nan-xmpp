#lang racket/base

(require racket/contract
	 racket/tcp ;; networking
	 openssl    ;; ssl/tls
	 "utils.rkt"
         (planet lizorkin/sxml:2:1/sxml))

(define-struct connection (host i-port o-port custodian)
  #:mutable)
(provide xmpp-send)
(provide/contract
 [struct connection
         ([host string?]
          [i-port input-port?]
          [o-port output-port?]
          [custodian custodian?])]
; [xmpp-send (connection? any . -> . any)]
 [send-string (port? string? . -> . any)]
 [new-connection (string? . -> . connection?)]
 [kill-connection! (connection? . -> . void)])

(define port 5222)
(define ssl-port 5223)

;;;; ;; ;  ;;;  ;
;;
;; tls & sasl
;;  - http://xmpp.org/rfcs/rfc3920.html#tls
;;  - http://xmpp.org/rfcs/rfc3920.html#sasl
;;
;;;; ;;


;; moved to xmpp-sasl until it 'works'

(define session->tls? #f) ;; changes state when a tls proceed is recived

(define (xmpp-send conn sz)
  (send-string (connection-o-port conn) (srl:sxml->xml sz)))

(define (send-string out str)
  (debugf "sending: ~a ~%~%" str) 
    (fprintf out "~A~%" str) (flush-output out))

(define (new-connection host)
    (define conn-cust (make-custodian))
    (parameterize ([current-custodian conn-cust])
      (let-values ([(in out)
		    (ssl-connect host ssl-port 'tls)])
	(file-stream-buffer-mode out 'line)
	(make-connection
	 host in out conn-cust))))

(define (kill-connection! conn)
  (with-handlers ([exn:fail:network? void])
    (close-output-port (connection-o-port conn)))
  (with-handlers ([exn:fail:network? void])
    (close-input-port (connection-i-port conn)))
  (custodian-shutdown-all (connection-custodian conn)))
