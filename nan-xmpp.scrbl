#lang scribble/doc
@(require scribble/manual)

@title{XMPP}

A module for using the Jabber/XMPP protocol.

@table-of-contents[]

@section{Require}

@scheme[(require (planet magica/nan-xmpp))]

If you are using @scheme[send] provided from @scheme[scheme/class] you
should use a prefix to avoid a name clash with @scheme[send].

@scheme[(require (prefix-in xmpp: (planet magica/nan-xmpp)))]

@section{Protocol Support}

A minimal subset of the XMPP protocols are supported, but not much
beyond sending and receiving messages and presence updates. This
module should eventually implement XMPP-Core and XMPP-IM to conform
with RFCs 3920 and 3921. Currently, the default connection uses 'old
style' SSL, which is deprecated and may cause problems with some
servers. Progress toward supporting the full protocol is documented in
the file 'xmpp.rkt'