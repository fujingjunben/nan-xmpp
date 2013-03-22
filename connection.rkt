#lang racket/base

(require racket/list
         racket/match
         racket/contract
         racket/tcp ;; networking
         openssl    ;; ssl/tls
         xml
         xml/path
         "utils.rkt"
         "xmpp.rkt")

(struct connection (host i-port o-port custodian buffer encryption) #:mutable)
(provide/contract
 [struct connection
   ([host string?]
    [i-port input-port?]
    [o-port output-port?]
    [custodian custodian?]
    [buffer list?]
    [encryption symbol?])]
 [xmpp-send (-> connection? any/c void?)]
 [open-session (->* (connection? string? string?) (boolean?) void?)]
 [xmpp-receive (-> connection? any/c)]
 [xmpp-set-handler (-> connection? (-> any/c void) void)]
 [xmpp-stop-handler (-> connection? void)]
 [send-string (port? string? . -> . any)]
 [new-connection (string? . -> . connection?)]
 [kill-connection! (connection? . -> . void)]
 [xmpp-version (-> string?)])

(define (xmpp-version) "nan-xmpp v 0.23")
(define port 5222)
(define ssl-port 5223)

;;;; ;; ;  ;;;  ;
;;
;; tls & sasl
;;  - http://xmpp.org/rfcs/rfc3920.html#tls
;;  - http://xmpp.org/rfcs/rfc3920.html#sasl
;;
;;;; ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;xmpp-set-handler, xmpp-stop-handler - abstractions over threads that check for new incoming messages
;;                                      and run handling procedures on them
;;Usage:
;;(xmpp-set-handler conn handler-proc)
;;(xmpp-stop-handler conn)
(struct h-thread (handler thread) #:mutable)

(define-values (xmpp-set-handler xmpp-stop-handler)
  ;;handler-hash is storing the state
  (let ((handler-hash (make-hash)))
      
      (define (receive-handle-thread conn)
        ;;creates a thread with handling procedure from handler-hash
        (parameterize ((current-custodian (connection-custodian conn)))
          (thread (lambda ()
                    (let handler-loop ()
                      (define sz (xmpp-receive-blocking conn))
                      (define handler-proc (h-thread-handler
                                            (hash-ref handler-hash conn)))
                      (handler-proc sz)
                      (handler-loop))))))
    
      (values
       ;;xmpp-set-handler
       (λ (conn handler-proc)
         (define h-t (hash-ref handler-hash conn #f))
         (if h-t
             (set-h-thread-handler! h-t handler-proc)
             (hash-set! handler-hash
                        conn (h-thread handler-proc
                                       (receive-handle-thread conn)))))
       ;;xmpp-stop-handler
       (λ (conn)  
         (define h-t (hash-ref handler-hash conn #f))
         (when h-t
           (kill-thread (h-thread-thread h-t))
           (hash-remove! handler-hash conn))))))

(define (xmpp-receive-blocking conn)
  ;;;loops until a stanza is received
  (let loop ()
    (let ((sz (xmpp-receive conn)))
      (or sz
          (begin (sleep 0.1)
                 (loop))))))

(define (xmpp-receive conn)
  ;;;Checks for a stanza in buffer and input port.
  ;;;If there is no new stanza returns #f
  (cond
    ((empty? (connection-buffer conn)) ;;if there is no stanza in buffer - get new one
     (define str (clean (read-async (connection-i-port conn)))) ;;string from port
     (or (= (string-length str) 0) (debugf "~a ~%" str))
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
  (fprintf out "~A~%" str)
  (flush-output out))

(define (new-connection host)
  (with-handlers
      ((exn:fail:network? (lambda (e) (new-connection-tcp host))))
    (define conn-cust (make-custodian))
    (parameterize ([current-custodian conn-cust])
      (let-values ([(in out)
                    (ssl-connect host ssl-port 'tls)])
        (file-stream-buffer-mode out 'line)
        (connection
         host in out conn-cust '() 'tls)))))

(define (new-connection-tcp host)
  (define conn-cust (make-custodian))
  (parameterize ((current-custodian conn-cust))
    (let-values ([(tcp-in tcp-out)
                  (tcp-connect host port)])
      (connection
       host tcp-in tcp-out conn-cust '() 'none))))

(define (open-session conn jid pass (encrypt #t))
  (let ((host (jid-host jid))
        (user (jid-user jid))
        (resource (jid-resource jid)))
    (send-string (connection-o-port conn) (xmpp-stream host))
    (cond
      ((and encrypt (eq? (connection-encryption conn) 'none))
       ;; tcp connection - tls/sasl are to negotiate
       (xmpp-receive-blocking conn) ;receiving stream from server
       (negotiate-tls conn)
       (xmpp-send conn (sasl-plain-auth (jid-user jid) pass))
       (unless (eq? (car (xmpp-receive-blocking conn))
                    'success)
         (raise "SASL negotiation failed"))
       (send-string (connection-o-port conn) (xmpp-stream host))
       (xmpp-receive-blocking conn) ;receiving stream
       (xmpp-send conn (xmpp-bind))
       (unless (string=? (iq-type (xmpp-receive-blocking conn))
                         "result")
         (raise "Resource binding failed")))
      (else
       ;; tls connection - everything should work just fine
       (xmpp-send conn (xmpp-session host))           
       (xmpp-send conn (xmpp-auth user pass resource))))))

(define (negotiate-tls conn)
  (xmpp-send conn (xmpp-starttls))
  (let loop ()
    (let ((sz (xmpp-receive-blocking conn)))
      (displayln sz)
      (case (car sz)
        ('failure (raise "TLS negotiation failed"))
        ('proceed (parameterize ((current-custodian (connection-custodian conn)))
                    (let-values ([(ssl-in ssl-out)
                                  (ports->ssl-ports (connection-i-port conn)
                                                    (connection-o-port conn)
                                                    #:encrypt 'tls)])
                      (set-connection-i-port! conn ssl-in)
                      (set-connection-o-port! conn ssl-out)
                      (set-connection-encryption! conn 'tls)
                      ;; send/receive stream-header
                      (send-string (connection-o-port conn) (xmpp-stream (connection-host conn)))
                      (xmpp-receive-blocking conn))))
        (else (loop))))))


(define (kill-connection! conn)
  (with-handlers ([exn:fail:network? void])
    (close-output-port (connection-o-port conn)))
  (with-handlers ([exn:fail:network? void])
    (close-input-port (connection-i-port conn)))
  (custodian-shutdown-all (connection-custodian conn)))
