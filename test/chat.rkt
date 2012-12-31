#lang racket/base
(require racket/string)

(require "../main.rkt")
(define conn (new-connection "jabber.ru"))
(open-session (connection-o-port conn) "mashin-bot@jabber.ru" "password")
(set-xmpp-handler 'message print-message)
(xmpp-response-handler (connection-i-port conn))

(let loop ((txt ""))
  (set! txt (read-line))
  (unless (string=? txt "exit")
    (send-string (connection-o-port conn) (message "dgee519@jabber.ru" txt))
    (set-xmpp-handler 'message print-message)
    (xmpp-response-handler (connection-i-port conn))
    (loop txt)))

(kill-connection! conn)
