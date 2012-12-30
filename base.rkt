#lang racket/base

(require racket/contract
	 racket/list
	 racket/async-channel
	 "utils.rkt"
	 "xmpp.rkt"
	 "connection.rkt")

(define (run-xmpp-handler type sz)
  (let ((fcn (dict-ref (xmpp-handlers) type #f))) 
    (when fcn (begin
                (debugf "attempting to run handler ~a.~%" fcn)
                (fcn sz)))))

;; no real parsing yet. dispatches any received xml stanzas as sxml
(define (parse-xmpp-response str)
  (when (> (string-length str) 0)
    (let ((sz (ssax:xml->sxml (open-input-string (clean str)) '())))
      ;;(let ((sz (lazy:xml->sxml (open-input-string str) '())))
      (cond
        ((equal? '(null) (cadr sz)) 
         (newline))
        ((equal? 'message (caadr sz))
         (run-xmpp-handler 'message sz))
        ((equal? 'iq (caadr sz)) 
         (run-xmpp-handler 'iq sz))
        ((equal? 'presence (caadr sz)) 
         (run-xmpp-handler 'presence sz))
	((equal? 'stanza (caadr sz))
	 (run-xmpp-handler 'stanza sz))
        (else (run-xmpp-handler 'other sz))))))

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

(define (open-session out jid pass)
  (let ((host (jid-host jid))
	(user (jid-user jid))
	(resource (jid-resource jid)))
    (send-string out (xmpp-stream host))
    (send-string out (xmpp-session host))           
    (send-string out (xmpp-auth user pass resource))
    (send-string out (presence))))
