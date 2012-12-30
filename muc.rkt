#lang racket/base
(require srfi/13
         "xmpp.rkt"
	 "utils.rkt")

(provide (all-defined-out))

(define (conference-room c)
  (string-take c (string-index c #\/)))

(define (conference-nick c)
  (let ((v (string-index c #\/)))
    (when v (string-take-right c (- (string-length c) v 1)))))
  
(define (presence-groupchat room nick jid #:type (type ""))
  (let ([cid (string-append room "/" nick)]
	[from jid])
    (cond ((string=? type "")
	   (ssxml `(presence (@ (from ,from) (to ,cid))
			     (x (@ (xmlns "http://jabber.org/protocol/muc"))))))
	  (else (ssxml `(presence (@ (from ,from) (to ,cid) (type ,type))
				  (x (@ (xmlns "http://jabber.org/protocol/muc")))))))))

(define (message-groupchat room nick body)
  (let ([to room])
    (ssxml `(message (@ (to ,to) (type "groupchat"))
		     (body ,body)
		     (nick (@ (xmlns "http://jabber.org/protocol/nick")) ,nick)))))
