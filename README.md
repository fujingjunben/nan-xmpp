# XMPP

A production module for IM using the Jabber/XMPP protocol with Racket.

## Protocol Support

Should eventually implement XMPP-Core and XMPP-IM to conform with RFCs
3920 and 3921. Progress toward supporting the full protocol is
currently documented in the file `xmpp.rkt`

## Installation

    (require (planet marune/nan-xmpp))

## Session

## Sending   

## Response Handlers 

## Example Chat Client

    #lang racket/base
    (require  "../nan-xmpp/main.rkt"
              racket/match)
    
    (define (read-input prompt)
      (display prompt)
      (read-line))
   
    (define jid (read-input "jid: "))
    (define password (read-input "password: "))
    (define to (read-input "chat to: "))
    
    (define conn (new-connection (jid-host jid)))
    (open-session conn jid password)
    
    (define (handler sz)
      (match sz
        ((list 'message _ ...) (print-message sz))
        ((list 'presence _ ...) (print-presence sz))
        (_ 'do-nothing)))
    
    (xmpp-handle 'set-handler conn handler)
    
    (let message-loop () 
      (define txt (read-line))
      (unless (string=? txt "/exit")
        (xmpp-send conn (message #:to to #:body txt))                      
        (message-loop)))
    
    (kill-connection! conn)

## possibly interesting extensions to implement. 

see http://xmpp.org/extensions/

* XEP-0047: In-Band Bytestreams
* XEP-0066: Out of Band Data
* XEP-0030: Service Discovery
* XEP-0060: Publish-Subscribe
* XEP-0045: Multi-User Chat (IN PROGRESS)
* XEP-0149: Time Periods
* XEP-0166: Jingle
* XEP-0174: Serverless Messaging
* XEP-0199: XMPP Ping
* XEP-0224: Attention
* XEP-0077: In-Band Registration
