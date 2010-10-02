#|

Keyboard-paste Sawfish Module.  Version 1.2.

Copyright (c) 2003-2004 Ewan Mellor <sawfish@ewanmellor.org.uk>.
All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.


This module contains a function -- keyboard-paste -- which simulates a
middle-button mouse click in order to paste the current primary selection, and
another function -- clear-selection -- to clear that primary selection.

This code uses Sawfish's synthesize-event function, which in turn uses
XSendEvent to pass the event to the window.  These synthetic events are
deliberately ignored by xterms and XEmacs, for security reasons.  To work
around this problem, start the terminal with

xterm -xrm 'xterm*allowSendEvents: True'

The analogous hack for XEmacs (not needed if you set
*keyboard-paste-use-gnuclient*) is to use

(setq x-allow-sendevents t)

These hacks open a fairly serious security hole, though, so only do this if
you know what you are doing.

If you have gnuclient available with your XEmacs installation, this module
can use that instead.  Just make sure that *keyboard-paste-use-gnuclient* is
set and there should be no need to allow send-events to XEmacs.


aterm honours XSendEvent as long as the mouse is over the terminal window,
which means that keyboard-paste works in that situation.


In order to fix this properly, Sawfish will need to be patched to use
XTestFakeKeyEvent in the XTest extension rather than XSendEvent, as suggested
at <http://www.handhelds.org:8080/wiki/GeneratingSyntheticEvents> (or possibly
by other means, such as using the XInputExtension; I don't know a great deal
about this area).


Install this file by placing it inside your ~/.sawfish/lisp/ directory, and
then place the following lines (or similar) into your ~/.sawfish/rc:

(require 'keyboard-paste)
(bind-keys
 global-keymap
 "M-Return" 'keyboard-paste
 "M-BS"     'clear-selection)

This will bind the keystroke Meta-Return to keyboard-paste, and Meta-Backspace
to clear-selection (in my case, meta is actually the Windows key).


clear-selection requires xsel by Conrad Parker, which you should be able
download from http://www.vergenet.net/~conrad/software/xsel/.

|#


(define-structure keyboard-paste
  (export keyboard-paste
          *keyboard-paste-use-gnuclient*
          clear-selection
          )

  (open rep
        rep.regexp
        rep.system
        sawfish.wm
        )


(defvar *clear-selection-command* "xsel -c ''")


(defvar *keyboard-paste-use-gnuclient* t
  "If true, gnuclient is used to paste into Emacsen.  This means that it is
not necessary to use x-allow-sendevents."
  )


(define (keyboard-paste focus)
  "Paste the X primary selection as if the middle mouse button had been
pressed."
  (interactive "%f")

  (when focus
    (if (and *keyboard-paste-use-gnuclient*
             (string-match "Emacs" (window-class focus)))
        (system "gnuclient -batch -f mouse-consolidated-yank >/dev/null &")
      (synthesize-event "Button2-Click1" focus))
    )
  )


(define (clear-selection)
  "Clear the X primary selection."
  (interactive)
  
  (system *clear-selection-command*)
  t
  )


)
