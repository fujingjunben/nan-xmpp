#lang racket/base

(require racket/list
         racket/contract
	 racket/tcp ;; networking
	 openssl    ;; ssl/tls
	 "utils.rkt"
         "xmpp.rkt"
         xml
         xml/path)

(struct connection (host i-port o-port custodian buffer) #:mutable)
(provide xmpp-send
         open-session
         xmpp-receive)
(provide/contract
 [struct connection
         ([host string?]
          [i-port input-port?]
          [o-port output-port?]
          [custodian custodian?]
          [buffer list?])]
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

(define (xmpp-receive conn)
  (cond
   ((empty? (connection-buffer conn)) ;;if there is no stanza in buffer - get new one
    (define str (clean (read-async (connection-i-port conn)))) ;;string from port
    (define szs (se-path*/list '(port)
                               (string->xexpr
                                (string-append "<port>" str "</port>")))) 
    (cond 
     ((empty? szs) #f)
     ((empty? (cdr szs)) (car szs))
     (else (set-connection-buffer! conn (cdr szs))
           (car szs))))
   (else (define b (connection-buffer conn))
         (set-connection-buffer! conn (cdr b))
         (car b))))

(define (xmpp-send conn sz)
  (send-string (connection-o-port conn) (xexpr->string sz)))

(define (send-string out str)
  (debugf "sending: ~a ~%~%" str) 
    (fprintf out "~A~%" str) (flush-output out))

(define (new-connection host)
    (define conn-cust (make-custodian))
    (parameterize ([current-custodian conn-cust])
      (let-values ([(in out)
		    (ssl-connect host ssl-port 'tls)])
	(file-stream-buffer-mode out 'line)
	(connection
	 host in out conn-cust '()))))

(define (open-session conn jid pass)
  (let ((host (jid-host jid))
	(user (jid-user jid))
	(resource (jid-resource jid)))
    (send-string (connection-o-port conn) (xmpp-stream host))
    (xmpp-send conn (xmpp-session host))           
    (xmpp-send conn (xmpp-auth user pass resource))
    (xmpp-send conn (presence))))

(define (kill-connection! conn)
  (with-handlers ([exn:fail:network? void])
    (close-output-port (connection-o-port conn)))
  (with-handlers ([exn:fail:network? void])
    (close-input-port (connection-i-port conn)))
  (custodian-shutdown-all (connection-custodian conn)))
