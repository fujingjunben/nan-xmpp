#lang racket/base

(require racket/contract
	 racket/list
	 racket/async-channel
	 racket/dict
	 "utils.rkt"
	 "xmpp.rkt"
	 "connection.rkt"
	 ;(planet lizorkin/sxml:2:1/sxml)  ;; encoding xml
	 ;(planet lizorkin/ssax:2:0/ssax)
         xml) ;; decoding xml

(provide (all-defined-out))

(define xmpp-handlers (make-hash)) ;; a hash of tags and functions (possibly extend to using sxpaths and multiple handlers)

(define (set-xmpp-handler type fcn)
  (dict-set! xmpp-handlers type fcn))

(define (remove-xmpp-handler type fcn)
  (dict-remove! xmpp-handlers type fcn))

(define (run-xmpp-handler type sz)
  (let ((fcn (dict-ref xmpp-handlers type #f))) 
    (when fcn (begin
                (debugf "attempting to run handler ~a.~%" fcn)
                (fcn sz)))))

;; no real parsing yet. dispatches any received xml stanzas as sxml

(define (parse-xmpp-response str)
  (when (> (string-length str) 0)
    (let ((sz (xml->xexpr (open-input-string (clean str)) '())))
      (cond
        ((equal? '(null) (cadr sz)) 
         (newline))
        ((equal? 'message (caadr sz))
         (run-xmpp-handler 'message sz))
        ((equal? 'iq (caadr sz)) 
         (run-xmpp-handler 'iq sz))
        ((equal? 'presence (caadr sz)) 
         (run-xmpp-handler 'presence sz))
        (else (run-xmpp-handler 'other sz))))))

;; response handler
(define (xmpp-response-handler in)
  (thread (lambda () 
            (let loop () 
              (parse-xmpp-response (read-async in))
              (sleep 0.1) ;; slight delay to avoid a tight loop
              (loop)))))

;; QND hack to filter out anything not a message, iq or presence
(define (clean str)
  (let ((test (substring str 0 3)))
    (cond ((string-ci=? test "<me") str)
          ((string-ci=? test "<iq") str)
          ((string-ci=? test "<pr") str)
          ((string-ci=? test "<ur") str)
          (else 
           (debugf "~%recieved: ~a ~%parsed as <null/>~%~%" str)
           "<null/>"))))

(define (open-session conn jid pass)
  (let ((host (jid-host jid))
	(user (jid-user jid))
	(resource (jid-resource jid)))
    (send-string (connection-o-port conn) (xmpp-stream host))
    (xmpp-send conn (xmpp-session host))           
    (xmpp-send conn (xmpp-auth user pass resource))
    (xmpp-send conn (presence))))
