#!/usr/bin/env python

import email, os, re, sys

msg = email.message_from_string(sys.stdin.read())

msgId = re.sub(r'[<>]', '', msg.get('message-id'))
subject = msg.get('Subject')

os.system("jed org-protocol:/capture:/m/mutt:%s/mail/'%s'" % (msgId, subject))
