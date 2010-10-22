;;;_* outdent.el - Indentation-based outline exposure and maneuvering

;; Author: Ken Manheimer <klm@nist.gov>
;; Maintainer: Ken Manheimer <klm@i.am>
;; Created: Sep 1994 - adapted from my allout.el
;; Version: $Id: outdent.el 4054 2001-08-30 19:05:28Z barry $
;; Keywords: outline mode wp languages text

;;;_* This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;_* Commentary:

;; A casual adaptation of allout.el to provide outline exposure
;; control and maneuvering according entirely to text indentation.
;; This is particularly for outline-structured editing of
;; block-structured program code.

;; Ken Manheimer
;; klm@i.am

;;;_* Provide
(provide 'outdent)

;;;_* USER CUSTOMIZATION VARIABLES:

;;;_ = outdent-command-prefix
(defvar outdent-command-prefix "\C-c"
  "*Key sequence to be used as prefix for outline mode command key bindings.")

;;;_ = outdent-keybindings-list
;;; You have to reactivate outdent-mode - '(outdent-mode t)' - to
;;; institute changes to this var.
(defvar outdent-keybindings-list ()
  "*List of outdent-mode key / function bindings, for outdent-mode-map.

String or vector key will be prefaced with outdent-command-prefix,
unless optional third, non-nil element is present.")
(setq outdent-keybindings-list
      '(
                                        ; Motion commands:
        ("\C-n" outdent-next-visible-heading)
        ("\C-p" outdent-previous-visible-heading)
        ("\C-u" outdent-up-current-level)
        ("\C-f" outdent-forward-current-level)
        ("\C-b" outdent-backward-current-level)
        ("\C-a" outdent-beginning-of-current-entry)
        ("\C-e" outdent-end-of-current-entry)
                                        ; Exposure commands:
        ("\C-i" outdent-show-children)
        ("\C-s" outdent-show-current-subtree)
        ("\C-h" outdent-hide-current-subtree)
        ("\C-o" outdent-show-current-entry)
        ("!" outdent-show-all)
                                        ; Alteration commands:
        ;;(" " outdent-open-sibtopic)
        ;;("." outdent-open-subtopic)
        ;;("," outdent-open-supertopic)
        ;;("'" outdent-shift-in)
        ;;(">" outdent-shift-in)
        ;;("<" outdent-shift-out)
        ;;("\C-m" outdent-rebullet-topic)
        ;;("b" outdent-rebullet-current-heading)
        ;;("#" outdent-number-siblings)
        ("\C-k" outdent-kill-line t)
	;; Outdent yank hasn't been reconciled, yet
	;;("\C-y" outdent-yank t)
        ;;("\M-y" outdent-yank-pop t)
        ("\C-k" outdent-kill-topic)
                                        ; Miscellaneous commands:
	;;([?\C-\ ] outdent-mark-topic)
        ;;("@" outdent-resolve-xref)
        ;;("=c" outdent-copy-exposed-to-buffer)
        ;;("=i" outdent-indented-exposed-to-buffer)
	;;("=t" outdent-latexify-exposed)
	;;("=p" outdent-flatten-exposed-to-buffer)
	))

;;;_ = outdent-isearch-dynamic-expose
(defvar outdent-isearch-dynamic-expose t
  "*Non-nil enable's outdent dynamic exposure of hidden incremental-search
targets as they're encountered.")
(make-variable-buffer-local 'outdent-isearch-dynamic-expose)

;;;_ = outdent-use-hanging-indents
(defvar outdent-use-hanging-indents t
  "*If non-nil, topic body text auto-indent defaults to indent of the header.
Ie, it is indented to be just past the header prefix.  This is
relevant mostly for use with indented-text-mode, or other situations
where auto-fill occurs.

[This feature no longer depends in any way on the 'filladapt.el'
lisp-archive package.]")
(make-variable-buffer-local 'outdent-use-hanging-indents)

;;;_ = outdent-reindent-bodies
(defvar outdent-reindent-bodies (if outdent-use-hanging-indents
				    'text)
  "*Non-nil enables auto-adjust of topic body hanging indent with depth shifts.

When active, topic body lines that are indented even with or beyond
their topic header are reindented to correspond with depth shifts of
the header.

A value of `t' enables reindent in non-programming-code buffers, ie
those that do not have the variable `comment-start' set.  A value of
`force' enables reindent whether or not `comment-start' is set.")

(make-variable-buffer-local 'outdent-reindent-bodies)

;;;_ = outdent-auto-activation
(defvar outdent-auto-activation nil
  "*Regulates auto-activation modality of outdent - see `outdent-init'.

Setq-default by `outdent-init' to regulate whether or not outdent mode
is automatically activated when the buffer-specific variable
`outdent-layout' is non-nil, and whether or not the layout dictated by
`outdent-layout' should be imposed on mode activation.

With value `t', auto-mode-activation and auto-layout are enabled.
\(This also depends on `outdent-find-file-hooks' being installed in
`find-file-hooks', which is also done by `outdent-init'.)

With value `ask', auto-mode-activation is enabled, and endorsement for
performing auto-layout is asked of the user each time.

With value `activate', only auto-mode-activation is enabled, auto-
layout is not.

With value `nil', neither auto-mode-activation nor auto-layout are
enabled.

See the docstring for `outdent-init' for the proper interface to
this variable.")

;;;_ = outdent-layout
(defvar outdent-layout nil
  "*Layout specification and provisional mode trigger for outdentation.

Buffer-specific.

A list value specifies a default layout for the current buffer, to be
applied upon activation of outdent-mode.  Any non-nil value will
automatically trigger outdent-mode, provided `outdent- init' has been
called to enable it.

See the docstring for `outdent-init' for details on setting up for
auto-mode-activation, and for `outdent-expose-topic' for the format of
the layout specification.

You can associate a particular outline layout with a file by setting
this var via the file's local variables.  For example, the following
lines at the bottom of an elisp file:

;;;Local variables:
;;;outdent-layout: \(0 : -1 -1 0\)
;;;End:

will, modulo the above-mentioned conditions, cause the mode to be
activated when the file is visited, followed by the equivalent of
`\(outdent-expose-topic 0 : -1 -1 0\)'.

Also, outdent's mode-specific provisions will make topic prefixes
default to the comment-start string, if any, of the language of the
file.  This is modulo the setting of `outdent-use-mode-specific-
leader', which see.") 
(make-variable-buffer-local 'outdent-layout)

;;;_* CODE - no user customizations below.

;;;_ #1 Internal Outline Formatting and Configuration
;;;_  = outdent-version
(defvar outdent-version
  (let ((rcs-rev "$Revision: 4054 $"))
    (condition-case err
	(save-match-data
	  (string-match "\\$Revision: \\([0-9]+\\.[0-9]+\\)" rcs-rev)
	  (substring rcs-rev (match-beginning 1) (match-end 1)))
      (error rcs-rev)))
  "Revision number of currently loaded outline package.  \(outdent.el\)")
;;;_  > outdent-version
(defun outdent-version (&optional here)
  "Return string describing the loaded outline version."
  (interactive "P")
  (let ((msg (concat "Outdent Mode v " outdent-version)))
    (if here (insert-string msg))
    (message "%s" msg)
    msg))
;;;_  - Topic header format
;;;_  = outdent-regexp
(defvar outdent-regexp nil
  "*Regular expression by which the beginning of an outdent topic is located.")
(make-variable-buffer-local 'outdent-regexp)
;;;_  = outdent-layout
(defvar outdent-layout nil
  "*Layout specification and provisional mode trigger for outdentation.

Buffer-specific.

A list value specifies a default layout for the current buffer, to be
applied upon activation of outdent-mode.  Any non-nil value will
automatically trigger outdent-mode, provided `outdent- init' has been
called to enable it.

See the docstring for `outdent-init' for details on setting up for
auto-mode-activation, and for `outdent-expose-topic' for the format of
the layout specification.

You can associate a particular outline layout with a file by setting
this var via the file's local variables.  For example, the following
lines at the bottom of an elisp file:

;;;Local variables:
;;;outdent-layout: \(0 : -1 -1 0\)
;;;End:

will, modulo the above-mentioned conditions, cause the mode to be
activated when the file is visited, followed by the equivalent of
`\(outdent-expose-topic 0 : -1 -1 0\)'.

Also, outdent's mode-specific provisions will make topic prefixes
default to the comment-start string, if any, of the language of the
file.  This is modulo the setting of `outdent-use-mode-specific-
leader', which see.") 
(make-variable-buffer-local 'outdent-layout)
;;;_  = outdent-line-boundary-regexp
(defvar outdent-line-boundary-regexp nil
  "Outdent-regexp with outdent-style beginning-of-line anchor.

\(Ie, C-j, *or* C-m, for prefixes of hidden topics).  This is properly
set so that \(match-beginning 1) and \(match-end 1) delimit the prefix.")
(make-variable-buffer-local 'outdent-line-boundary-regexp)
;;;_  = outdent-bob-regexp
(defvar outdent-bob-regexp nil
  "Like outdent-line-boundary-regexp, for headers at beginning of buffer.
\(match-beginning 2) and \(match-end 2) delimit the prefix.")
(make-variable-buffer-local 'outdent-bob-regexp)
;;;_  = outdent-isearch-prior-pos nil
(defvar outdent-isearch-prior-pos nil
  "Cue for isearch-dynamic-exposure tracking, used by outdent-isearch-expose.")
(make-variable-buffer-local 'outdent-isearch-prior-pos)
;;;_  = outdent-isearch-did-quit
(defvar outdent-isearch-did-quit nil
  "Distinguishes isearch conclusion and cancellation.

Maintained by outdent-isearch-abort \(which is wrapped around the real
isearch-abort\), and monitored by outdent-isearch-expose for action.")
(make-variable-buffer-local 'outdent-isearch-did-quit)

;;;_  - Key bindings
;;;_   = outdent-mode-map
(defvar outdent-mode-map nil "Keybindings for outdent minor mode.")
;;;_   > produce-outdent-mode-map (keymap-alist &optional base-map)
(defun produce-outdent-mode-map (keymap-list &optional base-map)
  "Produce keymap for use as outdent-mode-map, from keymap-list.

Built on top of optional BASE-MAP, or empty sparse map if none specified.
See doc string for outdent-keybindings-list for format of binding list."
  (let ((map (or base-map (make-sparse-keymap)))
	(pref (list outdent-command-prefix)))
    (mapcar (function
	     (lambda (cell)
	       (let ((add-pref (null (cdr (cdr cell))))
		     (key-suff (list (car cell))))
		 (apply 'define-key
			(list map
			      (apply 'concat (if add-pref
						 (append pref key-suff)
					       key-suff))
			      (car (cdr cell)))))))
	    keymap-list)
    map))
;;;_   = outdent-prior-bindings - being deprecated.
(defvar outdent-prior-bindings nil 
  "Variable for use in V18, with outdent-added-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_   = outdent-added-bindings - being deprecated
(defvar outdent-added-bindings nil 
  "Variable for use in V18, with outdent-prior-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_  - Mode-Specific Variable Maintenance Utilities
;;;_   = outdent-mode-prior-settings
(defvar outdent-mode-prior-settings nil
  "Internal outdent mode use; settings to be resumed on mode deactivation.")
(make-variable-buffer-local 'outdent-mode-prior-settings)
;;;_   > outdent-resumptions (name &optional value)
(defun outdent-resumptions (name &optional value)

  "Registers or resumes settings over outdent-mode activation/deactivation.

First arg is NAME of variable affected.  Optional second arg is list
containing outdent-mode-specific VALUE to be imposed on named
variable, and to be registered.  (It's a list so you can specify
registrations of null values.)  If no value is specified, the
registered value is returned (encapsulated in the list, so the caller
can distinguish nil vs no value), and the registration is popped
from the list."

  (let ((on-list (assq name outdent-mode-prior-settings))
        prior-capsule                   ; By 'capsule' i mean a list
                                        ; containing a value, so we can
                                        ; distinguish nil from no value.
        )

    (if value

        ;; Registering:
        (progn
          (if on-list
              nil 	; Already preserved prior value - don't mess with it.
            ;; Register the old value, or nil if previously unbound:
            (setq outdent-mode-prior-settings
                  (cons (list name
                              (if (boundp name) (list (symbol-value name))))
                        outdent-mode-prior-settings)))
                                        ; And impose the new value, locally:
	  (progn (make-local-variable name)
		 (set name (car value))))

      ;; Relinquishing:
      (if (not on-list)

          ;; Oops, not registered - leave it be:
          nil

        ;; Some registration:
                                        ; reestablish it:
        (setq prior-capsule (car (cdr on-list)))
        (if prior-capsule
            (set name (car prior-capsule)) ; Some prior value - reestablish it.
          (makunbound name))		; Previously unbound - demolish var.
                                        ; Remove registration:
        (let (rebuild)
          (while outdent-mode-prior-settings
            (if (not (eq (car outdent-mode-prior-settings)
                         on-list))
                (setq rebuild
                      (cons (car outdent-mode-prior-settings)
                            rebuild)))
            (setq outdent-mode-prior-settings
                  (cdr outdent-mode-prior-settings)))
          (setq outdent-mode-prior-settings rebuild)))))
  )
;;;_  - Mode-specific incidentals
;;;_   = outdent-during-write-cue nil
(defvar outdent-during-write-cue nil
  "Used to inhibit outline change-protection during file write.

See also `outdent-post-command-business', `outdent-write-file-hook',
`outdent-before-change-protect', and `outdent-post-command-business'
functions.")
;;;_   = outdent-override-protect nil
(defvar outdent-override-protect nil
  "Used in outdent-mode for regulate of concealed-text protection mechanism.

Outdent mode regulates alteration of concealed text to protect against
inadvertant, unnoticed changes.  This is for use by specific, native
outline functions to temporarily override that protection.  It's
automatically reset to nil after every buffer modification.")
(make-variable-buffer-local 'outdent-override-protect)
;;;_   > outdent-unprotected (expr)
(defmacro outdent-unprotected (expr)
  "Evaluate EXPRESSION with `outdent-override-protect' let-bound 't'."
  (` (let ((outdent-override-protect t))
       (, expr))))
;;;_   = outdent-undo-aggregation
(defvar outdent-undo-aggregation 30
  "Amount of successive self-insert actions to bunch together per undo.

This is purely a kludge variable, regulating the compensation for a bug in
the way that before-change-function and undo interact.")
(make-variable-buffer-local 'outdent-undo-aggregation)
;;;_   = file-var-bug hack
(defvar outdent-v18/19-file-var-hack nil
  "Horrible hack used to prevent invalid multiple triggering of outline
mode from prop-line file-var activation.  Used by outdent-mode function
to track repeats.")
;;;_   > outdent-write-file-hook ()
(defun outdent-write-file-hook ()
  "In outline mode, run as a local-write-file-hooks activity.

Currently just sets 'outdent-during-write-cue', so outdent-change-
protection knows to keep inactive during file write."
  (setq outdent-during-write-cue t)
  nil)

;;;_ #2 Mode activation
;;;_  = outdent-mode
(defvar outdent-mode () "Outdent mode minor-mode flag.")
(make-variable-buffer-local 'outdent-mode)
;;;_  > outdent-mode-p ()
(defsubst outdent-mode-p ()
  "Return t if outdent-mode is active in current buffer."
  outdent-mode)
;;;_  = outdent-explicitly-deactivated
(defvar outdent-explicitly-deactivated nil
  "Outdent-mode was last deliberately deactived.
So outdent-post-command-business should not reactivate it...")
(make-variable-buffer-local 'outdent-explicitly-deactivated)
;;;_  > outdent-init (&optional mode)
(defun outdent-init (&optional mode)
  "Prime outdent-mode to enable/disable auto-activation, wrt `outdent-layout'.

MODE is one of the following symbols:

 - nil \(or no argument) deactivate auto-activation/layou;
 - 'activate', enable auto-activation only;
 - 'ask', enable auto-activation, and enable auto-layout but with
   confirmation for layout operation solicitated from user each time;
 - 'report', just report and return the current auto-activation state;
 - anything else \(eg, t) for auto-activation and auto-layout, without
   any confirmation check.

Use this function to setup your emacs session for automatic activation
of outdent mode, contingent to the buffer-specific setting of the
`outdent-layout' variable.  (See `outdent-layout' and
`outdent-expose-topic' docstrings for more details on auto layout).

`outdent-init' works by setting up (or removing) the outdent-mode
find-file-hook, and giving `outdent-auto-activation' a suitable
setting.

To prime your emacs session for full auto-outline operation, include
the following two lines in your emacs init file:

\(require 'outdent)
\(outdent-init t)"

  (interactive)
  (if (interactive-p)
      (progn
	(setq mode
	      (completing-read
	       (concat "Select outline auto setup mode "
		       "(empty for report, ? for options) ")
	       '(("nil")("full")("activate")("deactivate")
		 ("ask") ("report") (""))
	       nil
	       t))
	(if (string= mode "")
	    (setq mode 'report)
	  (setq mode (intern-soft mode)))))
  (let
      ;; convenience aliases, for consistent ref to respective vars:
      ((hook 'outdent-find-file-hook)
       (curr-mode 'outdent-auto-activation))
	
    (cond ((not mode)
	   (setq find-file-hooks (delq hook find-file-hooks))
	   (if (interactive-p)
	       (message "Outdent mode auto-activation inhibited.")))
	  ((eq mode 'report)
	   (if (not (memq hook find-file-hooks))
	       (outdent-init nil)
	     ;; Just punt and use the reports from each of the modes:
	     (outdent-init (symbol-value curr-mode))))
	  (t (add-hook 'find-file-hooks hook)
	     (set curr-mode		; 'set', not 'setq'!
		  (cond ((eq mode 'activate)
			 (message
			  "Outline mode auto-activation enabled.")
			 'activate)
			((eq mode 'report)
			 ;; Return the current mode setting:
			 (outdent-init mode))
			((eq mode 'ask)
			 (message
			  (concat "Outline mode auto-activation and "
				  "-layout \(upon confirmation) enabled."))
			 'ask)
			((message
			  "Outline mode auto-activation and -layout enabled.")
			 'full)))))))
		   
;;;_  > outdent-mode (&optional toggle)
;;;_   : Defun:
(defun outdent-mode (&optional toggle)
;;;_    #1 Doc string:
  "Toggle minor mode for exposure and editing of text-indentation outlines.

Optional arg forces mode reactivation iff arg is positive num or symbol.

Outdent mode provides indentation-oriented outline exposure control.

Below is a description of the bindings, and then explanation of
special outdent-mode features and terminology.

The bindings themselves are established according to the values of
variables `outdent-keybindings-list' and `outdent-command-prefix',
each time the mode is invoked.  Prior bindings are resurrected when
the mode is revoked.

	Navigation:				   Exposure Control:
	----------                                 ----------------
C-c C-n outdent-next-visible-heading     | C-c C-h outdent-hide-current-subtree
C-c C-p outdent-previous-visible-heading | C-c C-i outdent-show-children
C-c C-u outdent-up-current-level         | C-c C-s outdent-show-current-subtree
C-c C-f outdent-forward-current-level    | C-c C-o outdent-show-current-entry
C-c C-b outdent-backward-current-level   | ^U C-c C-s outdent-show-all
C-c C-e outdent-end-of-current-entry     |	   outdent-hide-current-leaves
C-c C-a outdent-beginning-of-current-entry, alternately, goes to hot-spot"
;;;_    #2 Code
  (interactive "P")

  (let* ((active (and (not (equal major-mode 'outline))
		     (outdent-mode-p)))
				       ; Massage universal-arg 'toggle' val:
	 (toggle (and toggle
		     (or (and (listp toggle)(car toggle))
			 toggle)))
				       ; Activation specficially demanded?
	 (explicit-activation (or
			      ;;
			      (and toggle
				   (or (symbolp toggle)
				       (and (natnump toggle)
					    (not (zerop toggle)))))))
	 ;; outdent-mode already called once during this complex command?
	 (same-complex-command (eq outdent-v18/19-file-var-hack
				  (car command-history)))
	 do-layout
	 )

				       ; See comments below re v19.18,.19 bug.
    (setq outdent-v18/19-file-var-hack (car command-history))

    (cond

     ;; Provision for v19.18, 19.19 bug -
     ;; Emacs v 19.18, 19.19 file-var code invokes prop-line-designated
     ;; modes twice when file is visited.  We have to avoid toggling mode
     ;; off on second invocation, so we detect it as best we can, and
     ;; skip everything.
     ((and same-complex-command		; Still in same complex command
				       ; as last time outdent-mode invoked.
	  active			; Already activated.
	  (not explicit-activation)	; Prop-line file-vars don't have args.
	  (string-match "^19.1[89]"	; Bug only known to be in v19.18 and
			emacs-version)); 19.19.
      t)
	  
     ;; Deactivation:
     ((and (not explicit-activation)
	  (or active toggle))
				       ; Activation not explicitly
				       ; requested, and either in
				       ; active state or *de*activation
				       ; specifically requested:
      (setq outdent-explicitly-deactivated t)
      (if (string-match "^18\." emacs-version)
				       ; Revoke those keys that remain
				       ; as we set them:
	  (let ((curr-loc (current-local-map)))
	   (mapcar (function
		    (lambda (cell)
		      (if (eq (lookup-key curr-loc (car cell))
			      (car (cdr cell)))
			  (define-key curr-loc (car cell)
			    (assq (car cell) outdent-prior-bindings)))))
		   outdent-added-bindings)
	   (outdent-resumptions 'outdent-added-bindings)
	   (outdent-resumptions 'outdent-prior-bindings)))

      (outdent-resumptions 'selective-display)
      (add-hook 'pre-command-hook 'outdent-pre-command-business)
      (add-hook 'post-command-hook 'outdent-post-command-business)
      (setq local-write-file-hooks
	   (delq 'outdent-write-file-hook
		 local-write-file-hooks))
;;      (outdent-resumptions 'paragraph-start)
;;      (outdent-resumptions 'paragraph-separate)
      (outdent-resumptions (if (string-match "^18" emacs-version)
			      'auto-fill-hook
			    'auto-fill-function))
      (outdent-resumptions 'outdent-former-auto-filler)
      (setq outdent-mode nil))

     ;; Activation:
     ((not active)
      (setq outdent-explicitly-deactivated nil)
      ;;(setq outdent-regexp
      ;;	    (concat "[ \t]*\\(\\)[^ \t\r\n" (or comment-start "") "]"))
      (setq outdent-regexp
	    (concat "[ \t]*\\(\\)[^ \t\r\n]"))
      (setq outdent-line-boundary-regexp (concat "[\n\r]" outdent-regexp))
      ;(setq outdent-use-hanging-indents (not comment-start))
      (setq outdent-bob-regexp (concat "\\`" outdent-regexp))

				       ; Produce map from current version
				       ; of outdent-keybindings-list:
      (if (boundp 'minor-mode-map-alist)

	  (progn			; V19, and maybe lucid and
				       ; epoch, minor-mode key bindings:
	   (setq outdent-mode-map
		 (produce-outdent-mode-map outdent-keybindings-list))
	   (fset 'outdent-mode-map outdent-mode-map)
				       ; Include on minor-mode-map-alist,
				       ; if not already there:
	   (if (not (member '(outdent-mode . outdent-mode-map)
			    minor-mode-map-alist))
	       (setq minor-mode-map-alist
		     (cons '(outdent-mode . outdent-mode-map)
			   minor-mode-map-alist))))

				       ; V18 minor-mode key bindings:
				       ; Stash record of added bindings
				       ; for later revocation:
	(outdent-resumptions 'outdent-added-bindings
			    (list outdent-keybindings-list))
	(outdent-resumptions 'outdent-prior-bindings
			    (list (current-local-map)))
				       ; and add them:
	(use-local-map (produce-outdent-mode-map outdent-keybindings-list
						(current-local-map)))
	)
		 
				       ; selective-display is the
				       ; emacs conditional exposure
				       ; mechanism:
      (outdent-resumptions 'selective-display '(t))

				       ; Temporarily set by any outline
				       ; functions that can be trusted to
				       ; deal properly with concealed text.
      (add-hook 'local-write-file-hooks 'outdent-write-file-hook)
				       ; Custom auto-fill func, to support
				       ; respect for topic headline,
				       ; hanging-indents, etc:
      (let* ((fill-func-var (if (string-match "^18" emacs-version)
			       'auto-fill-hook
			     'auto-fill-function))
	    (fill-func (symbol-value fill-func-var)))
	;; Register prevailing fill func for use by outdent-auto-fill:
	(outdent-resumptions 'outdent-former-auto-filler (list fill-func))
	;; Register outdent-auto-fill to be used if filling is active:
	(outdent-resumptions fill-func-var '(outdent-auto-fill)))
;; No, paragraphs are regular according to mode ("indented-text rules":-).
;;      ;; Paragraphs are broken by topic headlines.
;;      (make-local-variable 'paragraph-start)
;;      (outdent-resumptions 'paragraph-start
;;			  (list (concat paragraph-start "\\|^\\("
;;					outdent-regexp "\\)")))
;;      (make-local-variable 'paragraph-separate)
;;      (outdent-resumptions 'paragraph-separate
;;			  (list (concat paragraph-separate "\\|^\\("
;;					outdent-regexp "\\)")))

      (or (assq 'outdent-mode minor-mode-alist)
	  (setq minor-mode-alist
	       (cons '(outdent-mode " OutDent") minor-mode-alist)))

      (if outdent-layout
	  (setq do-layout t))

      (if (and outdent-isearch-dynamic-expose
	       (not (fboundp 'outline-real-isearch-abort)))
	  (outdent-enwrap-isearch))

      (run-hooks 'outdent-mode-hook)
      (setq outdent-mode t))

     ;; Reactivation:
     ((setq do-layout t)
      (setq outdent-regexp
	    ;;(concat "[ \t]*\\(\\)[^ \t\r\n" (or comment-start "") "]")))
	    (concat "[ \t]*\\(\\)[^ \t\r\n]")))
     )					; cond

    (if (and do-layout
	     outdent-auto-activation
	     (listp outdent-layout)
	     (and (not (eq outdent-auto-activation 'activate))
		  (if (eq outdent-auto-activation 'ask)
		      (if (y-or-n-p (format "Expose %s with layout '%s'? "
					    (buffer-name)
					    outdent-layout))
			  t
			(message "Skipped %s layout." (buffer-name))
			nil)
		    t)))
	(save-excursion
	  (message "Adjusting '%s' exposure..." (buffer-name))
	  (goto-char 0)
	  (outdent-this-or-next-heading)
	  (condition-case err
	      (progn 
		(apply 'outdent-expose-topic (list outdent-layout))
		(message "Adjusting '%s' exposure... done." (buffer-name)))
	    ;; Problem applying exposure - notify user, but don't
	    ;; interrupt, eg, file visit:
	    ('error (message "%s" (car (cdr err)))
		    (sit-for 1)))))
    outdent-mode
    )					; let*
  )  					; defun

;;;_ #3 Internal Position State-Tracking - "outdent-recent-*" funcs
;;; All the basic outline functions that directly do string matches to
;;; evaluate heading prefix location set the variables
;;; `outdent-recent-prefix-beginning'  and `outdent-recent-prefix-end'
;;; when successful.  Functions starting with `outdent-recent-' all
;;; use this state, providing the means to avoid redundant searches
;;; for just-established data.  This optimization can provide
;;; significant speed improvement, but it must be employed carefully.
;;;_  = outdent-recent-prefix-beginning
(defvar outdent-recent-prefix-beginning 0
  "Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'outdent-recent-prefix-beginning)
;;;_  = outdent-recent-prefix-end
(defvar outdent-recent-prefix-end 0
  "Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'outdent-recent-prefix-end)
;;;_  = outdent-recent-prefix-col
(defvar outdent-recent-prefix-col 0
  "Column of start of most recently encountered topic prefix.")
(make-variable-buffer-local 'outdent-recent-prefix-col)
;;;_  = outdent-rel-lev
(defvar outdent-rel-lev 0
  "Index of relative level, adjusted by outdent-prefix-data, wrt
outdent-recent-prefix-col.")
;;;_  = outdent-recent-end-of-subtree
(defvar outdent-recent-end-of-subtree 0
  "Buffer point last returned by outdent-end-of-current-subtree.")
(make-variable-buffer-local 'outdent-recent-end-of-subtree)
;;;_  > outdent-prefix-data (beg col)
(defsubst outdent-prefix-data (beg col)
  "Register outdent-prefix state data - BEGINNING and COLUMN of prefix.

For reference by 'outdent-recent' funcs.  Returns BEGINNING."
  (save-restriction
    (widen)
    (if (/= col outdent-recent-prefix-col)
	(setq outdent-recent-prefix-col col
	      outdent-rel-lev (if (> col outdent-recent-prefix-col)
				  (1+ outdent-rel-lev)
				(1- outdent-rel-lev))))
    (setq outdent-recent-prefix-beginning beg
	  outdent-recent-prefix-end (+ beg col))))
;;;_  > outdent-recent-indent ()
(defsubst outdent-recent-indent ()
  "Indentation of last heading encountered by outdent maneuvering function."

  outdent-recent-prefix-col)

;;;_ #4 Navigation

;;;_  - Position Assessment
;;;_   : Location Predicates
;;;_    > outdent-on-current-heading-p ()
(defun outdent-on-current-heading-p ()
  "Return non-nil if point is on current visible topics' header line.

Actually, returns prefix beginning point."
  (save-excursion
    (beginning-of-line)
    (and (looking-at outdent-regexp)
	 (goto-char (match-beginning 1))
	 (outdent-prefix-data (match-beginning 0) (current-column)))))
;;;_    > outdent-e-o-prefix-p ()
(defun outdent-e-o-prefix-p ()
  "True if point is located where current topic prefix ends, heading begins."
  (and (save-excursion (beginning-of-line)
		       (looking-at outdent-regexp))
       (= (point)(save-excursion (outdent-goto-prefix)(point)))))
;;;_    > outdent-hidden-p ()
(defsubst outdent-hidden-p ()
  "True if point is in hidden text."
  (save-excursion
    (and (re-search-backward "[\n\r]" () t)
	 (= ?\r (following-char)))))
;;;_    > outdent-visible-p ()
(defsubst outdent-visible-p ()
  "True if point is not in hidden text."
  (interactive)
  (not (outdent-hidden-p)))
;;;_   : Location attributes
;;;_    > outdent-indentation ()
(defsubst outdent-indentation ()
  "Like outdent-current-indent, but respects hidden as well as visible topics."
  (save-excursion
    (outdent-goto-prefix)
    (outdent-recent-indent)))
;;;_    > outdent-current-indent ()
(defsubst outdent-current-indent ()
  "Return nesting depth of visible topic most immediately containing point."
  (save-excursion
    (if (outdent-back-to-current-heading)
	outdent-recent-prefix-col
      0)))
;;;_    > outdent-sibling-index (&optional depth)
(defun outdent-sibling-index (&optional levels)
  "Item number of this prospective topic among its siblings.

If optional arg levels is greater than zero, then we're
opening a new level, and return 0.

If less than zero, ascend to that number of levels and count..."

  (save-excursion
    (cond ((or (not levels) (= levels 0))
	   ;; At level of interest - count prior topics:
	   (let ((index 1)
		 (indent (outdent-indentation)))
	     (while (outdent-previous-sibling indent nil)
	       (setq index (1+ index)))
	     index))
	  ((< levels 0)
	   ;; Ascending - go up and count there:
	   (while (and (< levels 0)
		       (outdent-ascend))
	     (setq levels (1+ levels)))
	   (outdent-sibling-index))
	  ;; Descending - before first
	  (0))))
;;;_    > outdent-topic-flat-index ()
;;rectifying:
(defun outdent-topic-flat-index ()
  "Return a list indicating point's numeric section.subsect.subsubsect...
Outermost is first."
  (let* ((indent (outdent-indentation))
	 (next-index (outdent-sibling-index))
	 (rev-sibls nil))
    (save-excursion
      (while (and (> next-index 0)
		  (setq rev-sibls (cons next-index rev-sibls))
		  (outdent-ascend))
	(setq next-index (outdent-sibling-index))))
    rev-sibls))
;;;_  - Navigation macros
;;;_   > outdent-next-heading ()
(defsubst outdent-next-heading ()
  "Move to the heading for the topic \(possibly invisible) before this one.

Returns the location of the heading, or nil if none found."

  (if (and (bobp) (not (eobp)))
       (forward-char 1))

  (if (re-search-forward outdent-line-boundary-regexp nil 0)
      (outdent-prefix-data		; Got valid location state - set vars:
       (goto-char (or (match-beginning 1) outdent-recent-prefix-beginning))
       (current-column))))
;;;_   > outdent-this-or-next-heading
(defun outdent-this-or-next-heading ()
  "Position cursor on current or next heading."
  ;; A throwaway non-macro that is defined after outdent-next-heading
  ;; and usable by outdent-mode.
  (if (not (outdent-goto-prefix)) (outdent-next-heading)))
;;;_   > outdent-previous-heading ()
(defsubst outdent-previous-heading ()
  "Move to the prior \(possibly invisible) heading line.

Return the location of the beginning of the heading, or nil if not found."

  (if (bobp)
      nil
    (outdent-goto-prefix)
    (if
	;; searches are unbounded and return nil if failed:
	(or (re-search-backward outdent-line-boundary-regexp nil 0)
	    (looking-at outdent-bob-regexp))
	(outdent-prefix-data		; Got valid location state - set vars:
	 (goto-char (or (match-beginning 1) outdent-recent-prefix-beginning))
	 (current-column)))))

;;;_  - Subtree Charting
;;;_   " These routines either produce or assess charts, which are
;;; nested lists of the locations of topics within a subtree.
;;;
;;; Use of charts enables efficient navigation of subtrees, by
;;; requiring only a single regexp-search based traversal, to scope
;;; out the subtopic locations.  The chart then serves as the basis
;;; for assessment or adjustment of the subtree, without redundant
;;; traversal of the structure.

;;;_   > outdent-raw-chart-subtree (&optional levels)
(defun outdent-raw-chart-subtree (&optional levels)
  "Produce reverse point / indentation pairs for topics in current topic.
Do not go deeper than optional LEVELS, if specified."

  (outdent-goto-prefix)

  (let* ((orig-indent (outdent-recent-indent))
	 (depth (if levels (cons (list orig-indent) levels)))
	 chart indent continue)

    (while (and

	    (if (or (not depth) (not (= (cdr depth) 0)))
		;; Move by heading:
		(outdent-next-heading)
	      ;; Move by siblings, or outwards if no more siblings:
	      (setq indent (outdent-recent-indent))
	      (or (outdent-next-sibling)
		  (progn
		    (while (and (setq continue (outdent-next-heading))
				(>= (outdent-recent-indent) indent)))
		    continue)))
	    ;; not back at starting original indent:
	    (not (= (setq curr-indent (outdent-recent-indent)) orig-indent)))

      (setq chart (cons (point) (cons curr-indent chart)))
      (if depth
	  (cond ((< curr-indent (car (car depth)))
		 ;; Ascending:
		 (while (and (car depth)
			     (< curr-indent (car (car depth))))
		   (setq depth (cons (cdr (car depth))
				     (1+ (cdr depth)))))
		 (if (or (not (car depth))
			 (not (= (car (car depth)) curr-indent)))
		     (setq depth (cons (cons curr-indent (car depth))
				       (1- (cdr depth))))))
		((> curr-indent (car (car depth)))
		 ;; Going deeper:
		 (setq depth (cons (cons curr-indent (car depth))
				   (max 0 (1- (cdr depth)))))))))
    chart))
;;;_   > outdent-nestle-raw-chart (raw)
(defun outdent-nestle-raw-chart (doing &optional recursing)

  "Given a list of ints, representing alternating point/indent data
\(in reverse order), return a list of just the points, nested at
relative indents."

  (if doing
      (let* ((this-indent (nth 1 doing))
	     (got (if (listp (car doing))
		      (car doing)
		    (list (car doing))))
	     (doing (nthcdr 2 doing))
	     next-indent next sub)
	(while doing
	  (setq next-indent (nth 1 doing)
		next (car doing))
	  (cond
	   ((= this-indent next-indent)
	    ;; Continuation of current level:
	    (setq got (if (listp next)
			  (append next got)  ; splice in composite.
			(cons next got)))    ; cons in item.
	    (setq doing (nthcdr 2 doing))    ; increment.
	    (if (and (not doing) recursing)
		(setq got (list got this-indent))))
	   ((> this-indent next-indent)
	    ;; This is an offspring of some prior relative:
	    (if recursing		; Return composite and rest to caller:
		(setq got (cons
			   (cons next (if (listp (car got))
					  got
					(list got)))
			   ;; Indententation of unit whole is next-indent:
			   (cons next-indent (nthcdr 2 doing)))
		      doing nil)
	      ;; At top caller - continue in line:
	      (setq got (cons next (if (listp (car got)) got (list got)))
		    this-indent next-indent
		    doing (nthcdr 2 doing))))
	   (t				; Nested level - recurse:
	    (if (nthcdr 2 doing)
		(while (and (nthcdr 2 doing)
			    (< this-indent (nth 1 doing)))
		  (setq doing (outdent-nestle-raw-chart doing t)))
	      (setq doing (cons (list (car doing)) (cdr doing))))
	    (setq next-indent (nth 1 doing))
	    (setq next (car doing))
	    (setq doing (cons (if (< this-indent next-indent)
				  (cons next got)
				(append next got))
			      (cons this-indent (nthcdr 2 doing))))
	    (if recursing
		(setq got doing doing nil)
	      (setq got nil)))))

	got)))
;;;_   > outdent-chart-subtree (&optional levels)
(defun outdent-chart-subtree (&optional levels)
  "Produce a location \"chart\" of subtopics of the containing topic.

Optional argument LEVELS specifies the depth \(relative to start
depth\) for the chart, or 1 if not specified.

Charts capture outline structure, so that structure-altering routines
need only chart the structure once, and then use the chart for
whatever elaborate manipulations they do.

The entry for each topic consists of an integer indicating the point
at the beginning of the topic.  Charts for offspring consists of a
list containing, recursively, the charts for the respective subtopics."

  (outdent-nestle-raw-chart (outdent-raw-chart-subtree levels)))
;;;_   > outdent-chart-siblings (&optional start end)
(defun outdent-chart-siblings (&optional start end)
  "Produce a list of locations of this and succeeding sibling topics.
Effectively a top-level chart of siblings.  See 'outdent-chart-subtree'
for an explanation of charts."
  (save-excursion
    (if (outdent-goto-prefix)
	(let ((chart (list (point))))
	  (while (outdent-next-sibling)
	    (setq chart (cons (point) chart)))
	  (if chart (setq chart (nreverse chart)))))))
;;;_   > outdent-chart-to-reveal (chart levels)
(defun outdent-chart-to-reveal (chart levels)

  "Return a flat list of hidden points in subtree CHART, up to LEVELS levels.

Note that point can be left at any of the points on chart, or at the
start point."

  (let (result here)
    (while (and (or (eq levels t) (>= levels 0))
		chart)
      (setq here (car chart))
      (if (listp here)
	  (let ((further (outdent-chart-to-reveal here (or (eq levels t)
							   (1- levels)))))
	    ;; We're on the start of a subtree - recurse with it, if there's
	    ;; more depth to go:
	    (if further (setq result (append further result)))
	    (setq chart (cdr chart)))
	(goto-char here)
	(if (not (outdent-visible-p))
	    (setq result (cons here result)))
	(setq chart (cdr chart))))
    result))
;;;_   X outdent-chart-spec (chart spec &optional exposing)
(defun outdent-chart-spec (chart spec &optional exposing)
  "Not yet \(if ever\) implemented.

Produce exposure directives given topic/subtree CHART and an exposure SPEC.

Exposure spec indicates the locations to be exposed and the prescribed
exposure status.  Optional arg EXPOSING is an integer, with 0
indicating pending concealment, anything higher indicating depth to
which subtopic headers should be exposed, and negative numbers
indicating (negative of) the depth to which subtopic headers and
bodies should be exposed.

The produced list can have two types of entries.  Bare numbers
indicate points in the buffer where topic headers that should be
exposed reside.

 - bare negative numbers indicates that the topic starting at the
   point which is the negative of the number should be opened,
   including their entries.
 - bare positive values indicate that this topic header should be
   openned.
 - Lists signify the beginning and end points of regions that should
   be flagged, and the flag to employ.  (For concealment: '\(\?r\)', and
   exposure:"
  (while spec
    (cond ((listp spec) 
	   )
	  )
    (setq spec (cdr spec)))
  )

;;;_  - Within Topic
;;;_   > outdent-goto-prefix ()
(defun outdent-goto-prefix ()
  "Put point at beginning of immediately containing indentation topic.

Goes to most immediate subsequent topic if none immediately containing.

Not sensitive to topic visibility.

Returns a the point at the beginning of the prefix, or nil if none."

  (let (done)
    (while (and (not done)
		(re-search-backward "[\n\r]" nil 1))
      (forward-char 1)
      (if (looking-at outdent-regexp)
	  (progn (goto-char (match-beginning 1))
		 (setq done (outdent-prefix-data (match-beginning 0)
						 (current-column))))
	(forward-char -1)))
    (if (bobp)
	(cond ((looking-at outdent-regexp)
	       (goto-char (match-beginning 1))
	       (outdent-prefix-data (match-beginning 0)(current-column)))
	      ((outdent-next-heading))
	      (done))
      done)))
;;;_   > outdent-end-of-prefix ()
(defun outdent-end-of-prefix (&optional ignore-decorations)
  "Position cursor at beginning of header text."

  (if (not (outdent-goto-prefix))
      nil
    (let ((match-data (match-data)))
      (goto-char (1- (match-end 0)))
      (store-match-data match-data))
    ;; Reestablish where we are:
    (outdent-current-indent)))
;;;_   > outdent-back-to-current-heading ()
(defun outdent-back-to-current-heading ()
  "Move to heading line of current topic, or beginning if already on the line."

  (beginning-of-line)
  (prog1 (or (outdent-on-current-heading-p)
             (and (re-search-backward (concat "^" outdent-regexp)
                                      nil
                                      'move)
		  (goto-char (match-beginning 1))
                  (outdent-prefix-data (match-beginning 0)(current-column))))))
;;;_   > outdent-pre-next-preface ()
(defun outdent-pre-next-preface ()
  "Skip forward to just before the next heading line.

Returns that character position."

  (if (re-search-forward outdent-line-boundary-regexp nil 'move)
      (prog1 (goto-char (match-beginning 0))
             (outdent-prefix-data (match-beginning 1)(current-column)))))
;;;_   > outdent-end-of-current-subtree ()
(defun outdent-end-of-current-subtree ()
  "Put point at the end of the last leaf in the currently visible topic."
  (interactive)
  (outdent-back-to-current-heading)
  (let ((level (outdent-recent-indent)))
    (outdent-next-heading)
    (while (and (not (eobp))
                (> (outdent-recent-indent) level))
      (outdent-next-heading))
    (beginning-of-line)
    (and (not (eobp)) (forward-char -1))
    (and (memq (preceding-char) '(?\n ?\r))
         (memq (aref (buffer-substring (max 1 (- (point) 3)) (point)) 1)
               '(?\n ?\r))
         (forward-char -1))
    (setq outdent-recent-end-of-subtree (point))))
;;;_   > outdent-beginning-of-current-entry ()
(defun outdent-beginning-of-current-entry ()
  "When not already there, position point at beginning of current topic's body.

If already there, move cursor to bullet for hot-spot operation.
\(See outdent-mode doc string for details on hot-spot operation.)"
  (interactive)
  (let ((start-point (point)))
    (outdent-goto-prefix)))
;;;_   > outdent-end-of-current-entry ()
(defun outdent-end-of-current-entry ()
  "Position the point at the end of the current topics' entry."
  (interactive)
  (outdent-show-entry)
  (prog1 (outdent-pre-next-preface)
    (if (and (not (bobp))(looking-at "^$"))
        (forward-char -1))))

;;;_  - Depth-wise
;;;_   > outdent-ascend ()
(defun outdent-ascend ()
  "Ascend LEVELS, returning indentation if successful, nil if not."
  (let ((last-good (outdent-goto-prefix)))
    (outdent-beginning-of-level)
    (if (outdent-previous-heading)
	(outdent-recent-indent)
      (goto-char last-good)
      nil)))
;;;_   > outdent-descend-to-level (depth)
(defun outdent-descend-to-level (depth)
  "Descend to depth DEPTH within current topic.

Returning depth if successful, nil if not."
  (let ((start-point (point))
        (start-level (outdent-indentation)))
    (while
        (and (not (= depth (outdent-recent-indent))) ; ... not there yet
             (outdent-next-heading)     ; ... go further
             (< start-level (outdent-recent-indent)))) ; ... still in topic
    (if (= (outdent-recent-indent) depth)
        depth
      (goto-char start-point)
      nil))
  )
;;;_   > outdent-up-current-level (arg &optional dont-complain)
(defun outdent-up-current-level (arg &optional dont-complain)
  "Move out ARG levels from current visible topic.

Positions on heading line of containing topic.  Error if unable to
ascend that far, or nil if unable to ascend but optional arg
DONT-COMPLAIN is non-nil."
  (interactive "p")
  (outdent-back-to-current-heading)
  (let ((present-level (outdent-recent-indent))
	(last-good (point))
	failed
	return)
    ;; Loop for iterating arg:
    (while (and (> (outdent-recent-indent) 1)
                (> arg 0)
                (not (bobp))
		(not failed))
      (setq last-good (point))
      ;; Loop for going back over current or greater depth:
      (while (and (not (< (outdent-recent-indent) present-level))
		  (or (outdent-previous-visible-heading 1)
		      (not (setq failed present-level)))))
      (setq present-level (outdent-current-indent))
      (setq arg (- arg 1)))
    (if (or failed
	    (> arg 0))
	(progn (goto-char last-good)
	       (if (interactive-p) (outdent-goto-prefix))
	       (if (not dont-complain)
		   (error "Can't ascend past outermost level.")
		 (if (interactive-p) (outdent-goto-prefix))
		 nil))
      (if (interactive-p) (outdent-goto-prefix))
      outdent-recent-prefix-beginning)))

;;;_  - Linear
;;;_   > outdent-next-sibling (&optional indent backward)
(defun outdent-next-sibling (&optional indent backward)
  "Like outdent-forward-current-level, but respects invisible topics.

Traverse at optional INDENT, or current indent if none specified.

Go backward if optional arg BACKWARD is non-nil.

Return indent if successful, nil otherwise."

  (if (and backward (bobp))
      nil
    (let ((start-indent (or indent (outdent-indentation)))
          (start-point (point))
	  last-indent)
      (while (and (not (if backward (bobp) (eobp)))
                  (if backward (outdent-previous-heading)
                    (outdent-next-heading))
                  (> (setq last-indent (outdent-recent-indent)) start-indent)))
      (if (and (not (eobp))
	       (= (outdent-recent-indent) start-indent)
	       (not (and backward
			 (bobp)
			 (not (looking-at outdent-regexp)))))
          outdent-recent-prefix-beginning
        (goto-char start-point)
	(if indent (outdent-indentation) start-indent)
        nil))))
;;;_   > outdent-previous-sibling (&optional depth backward)
(defun outdent-previous-sibling (&optional depth backward)
  "Like outdent-forward-current-level,but backwards & respect invisible topics.

Optional DEPTH specifies depth to traverse, default current depth.

Optional BACKWARD reverses direction.

Return depth if successful, nil otherwise."
  (outdent-next-sibling depth (not backward))
  )
;;;_   > outdent-snug-back ()
(defun outdent-snug-back ()
  "Position cursor at end of previous topic

Presumes point is at the start of a topic prefix."
  (re-search-backward "[\n\r]" nil t)
  (if (or (bobp) (not (memq (preceding-char) '(?\n ?\r))))
      nil
    (forward-char -1))
  (point))
;;;_   > outdent-beginning-of-level ()
(defun outdent-beginning-of-level ()
  "Go back to the first sibling at this level, visible or not."
  (outdent-end-of-level 'backward))
;;;_   > outdent-end-of-level (&optional backward)
(defun outdent-end-of-level (&optional backward)
  "Go to the last sibling at this level, visible or not."

  (let ((indent (outdent-indentation)))
    (while (outdent-previous-sibling indent nil))
    (prog1 (outdent-recent-indent)
      (if (interactive-p) (outdent-goto-prefix)))))
;;;_   > outdent-next-visible-heading (arg)
(defun outdent-next-visible-heading (arg)
  "Move to the next ARG'th visible heading line, backward if arg is negative.

Move as far as possible in indicated direction \(beginning or end of
buffer\) if headings are exhausted."

  (interactive "p")
  (let* ((backward (if (< arg 0) (setq arg (* -1 arg))))
	 (step (if backward -1 1))
	 (start-point (point))
	 prev got)

    (while (> arg 0)			; limit condition
      (while (and (not (if backward (bobp)(eobp))) ; boundary condition
		  ;; Move, skipping over all those concealed lines:
		  (< -1 (forward-line step))
		  (not (setq got (looking-at outdent-regexp)))))
      ;; Register this got, it may be the last:
      (if got (setq prev got))
      (setq arg (1- arg)))
    (cond (got				; Last move was to a prefix:
	   (outdent-goto-prefix)
	   (outdent-prefix-data (match-beginning 0) (current-column)))
	  (prev				; Last move wasn't, but prev was:
	   (outdent-goto-prefix)
	   (outdent-prefix-data (match-beginning 0) (current-column)))
	  ((not backward) (end-of-line) nil))))
;;;_   > outdent-previous-visible-heading (arg)
(defun outdent-previous-visible-heading (arg)
  "Move to the previous heading line.

With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that outdent-regexp
matches)."
  (interactive "p")
  (outdent-next-visible-heading (- arg)))
;;;_   > outdent-forward-current-level (arg)
(defun outdent-forward-current-level (arg)
  "Position point at the next heading of the same level.

Takes optional repeat-count, goes backward if count is negative.

Returns resulting position, else nil if none found."
  (interactive "p")
  (let ((start-indent (outdent-current-indent))
	(start-point (point))
	(start-arg arg)
	(backward (> 0 arg))
	last-indent
	(last-good (point))
	at-boundary)
    (if backward (setq arg (* -1 arg)))
    (while (not (or (zerop arg)
		    at-boundary))
      (while (and (not (if backward (bobp) (eobp)))
		  (if backward (outdent-previous-visible-heading 1)
		    (outdent-next-visible-heading 1))
		  (> (setq last-indent (outdent-recent-indent)) start-indent)))
      (if (and last-indent (= last-indent start-indent)
	       (not (if backward (bobp) (eobp))))
	  (setq last-good (point)
		arg (1- arg))
	(setq at-boundary t)))
    (if (and (not (eobp))
	     (= arg 0)
	     (= (outdent-recent-indent) start-indent))
	outdent-recent-prefix-beginning
      (goto-char last-good)
      (if (not (interactive-p))
	  nil
	(outdent-goto-prefix)
	(error "Hit %s level %d topic, traversed %d of %d requested."
	       (if backward "first" "last")
	       (outdent-recent-indent)
	       (- (abs start-arg) arg)
	       (abs start-arg))))))
;;;_   > outdent-backward-current-level (arg)
(defun outdent-backward-current-level (arg)
  "Inverse of `outdent-forward-current-level'."
  (interactive "p")
  (if (interactive-p)
      (let ((current-prefix-arg (* -1 arg)))
	(call-interactively 'outdent-forward-current-level))
    (outdent-forward-current-level (* -1 arg))))


;;;_ #5 Exposure Control

;;;_  - Fundamental
;;;_   > outdent-flag-region (from to flag)
(defsubst outdent-flag-region (from to flag)
  "Hide or show lines from FROM to TO, via emacs selective-display FLAG char.
Ie, text following flag C-m \(carriage-return) is hidden until the
next C-j (newline) char.

Returns the endpoint of the region."
  (let ((buffer-read-only nil))
    (subst-char-in-region from to
			  (if (= flag ?\n) ?\r ?\n)
			  flag t)))
;;;_   > outdent-flag-current-subtree (flag)
(defun outdent-flag-current-subtree (flag)
  "Hide or show subtree of currently-visible topic.

See `outdent-flag-region' for more details."

  (save-excursion
    (outdent-back-to-current-heading)
    (outdent-flag-region (point)
			 (progn (outdent-end-of-current-subtree) (1- (point)))
			 flag)))

;;;_  - Topic-specific
;;;_   > outdent-show-entry ()
; outdent-show-entry basically for isearch dynamic exposure, as is...
(defun outdent-show-entry ()
  "Like `outdent-show-current-entry', reveals entries nested in hidden topics.

This is a way to give restricted peek at a concealed locality without the
expense of exposing its context, but can leave the outline with aberrant
exposure.  outdent-hide-current-entry-completely or outdent-show-offshoot
should be used after the peek to rectify the exposure."

  (interactive)
  (save-excursion
    (outdent-goto-prefix)
    (outdent-flag-region (if (bobp) (point) (1- (point)))
                         (or (outdent-pre-next-preface) (point))
			 ?\n)))
;;;_   > outdent-show-children (&optional levels strict)
(defun outdent-show-children (&optional levels strict)

  "If point is visible, show all direct subheadings of this heading.

Otherwise, do outdent-show-to-offshoot, and then show subheadings.

Optional LEVELS specifies how many levels below the current level
should be shown, or all levels if t.  Default is 1.

Optional STRICT means don't resort to -show-to-offshoot, no matter
what.  This is basically so -show-to-offshoot, which is called by
this function, can employ the pure offspring-revealing capabilities of
it.

Returns point at end of subtree that was opened, if any.  (May get a
point of non-opened subtree?)"

  (interactive "p")
  (let (max-pos)
    (if (and (not strict)
	     (outdent-hidden-p))

	(progn (outdent-show-to-offshoot) ; Point's concealed, open to
					  ; expose it.
	       ;; Then recurse, but with "strict" set so we don't
	       ;; infinite regress:
	       (setq max-pos (outdent-show-children levels t)))

      (save-excursion
	(save-restriction
	  (let* ((start-pt (point))
		 (chart (outdent-chart-subtree levels))
		 (to-reveal (outdent-chart-to-reveal chart (or levels 1))))
	    (goto-char start-pt)
	    (if (and strict (= (preceding-char) ?\r))
		;; Concealed root would already have been taken care of,
		;; unless strict was set.
		(outdent-flag-region (point) (outdent-snug-back) ?\n))
	    (while to-reveal
	      (goto-char (car to-reveal))
	      (outdent-flag-region (point) (outdent-snug-back) ?\n)
	      (setq to-reveal (cdr to-reveal)))))))))
;;;_   > outdent-hide-point-reconcile ()
(defun outdent-hide-reconcile ()
  "Like `outdent-hide-current-entry'; hides completely if within hidden region.

Specifically intended for aberrant exposure states, like entries that were
exposed by outdent-show-entry but are within otherwise concealed regions."
  (interactive)
  (save-excursion
    (outdent-goto-prefix)
    (outdent-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outdent-pre-next-preface)
                                (if (= ?\r (following-char))
                                    (point)
                                  (1- (point))))
                         ?\r)))
;;;_   > outdent-show-to-offshoot ()
(defun outdent-show-to-offshoot ()
  "Like outdent-show-entry, but reveals opens all concealed ancestors, as well.

As with outdent-hide-current-entry-completely, useful for rectifying
aberrant exposure states produced by outdent-show-entry."

  (interactive)
  (save-excursion
    (let ((orig-pt (point))
	  (orig-pref (outdent-goto-prefix))
	  (last-at (point))
	  bag-it)
      (while (or bag-it (if (re-search-backward "[\n\r]" nil t)
			    (looking-at "\r")))
	(beginning-of-line)
	(if (= last-at (setq last-at (point)))
	    ;; Oops, we're not making any progress!  Show the current
	    ;; topic completely, and bag this try.
	    (progn (beginning-of-line)
		   (outdent-show-current-subtree)
		   (goto-char orig-pt)
		   (setq bag-it t)
		   (beep)
		   (message "%s: %s"
			    "outdent-show-to-offshoot: "
			    "Aberrant nesting encountered.")))
	(outdent-show-children)
	(goto-char orig-pref))
      (goto-char orig-pt)))
  (if (outdent-hidden-p)
      (outdent-show-entry)))
;;;_   > outdent-hide-current-entry ()
(defun outdent-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outdent-back-to-current-heading)
  (save-excursion
   (outdent-flag-region (point)
                        (progn (outdent-end-of-current-entry) (point))
                        ?\r)))
;;;_   > outdent-show-current-entry (&optional arg)
(defun outdent-show-current-entry (&optional arg)

  "Show body following current heading, or hide the entry if repeat count."

  (interactive "P")
  (if arg
      (outdent-hide-current-entry)
    (save-excursion
      (outdent-flag-region (point)
			   (progn (outdent-end-of-current-entry) (point))
			   ?\n))))
;;;_   > outdent-hide-current-entry-completely ()
; ... outdent-hide-current-entry-completely also for isearch dynamic exposure:
(defun outdent-hide-current-entry-completely ()
  "Like outdent-hide-current-entry, but conceal topic completely.

Specifically intended for aberrant exposure states, like entries that were
exposed by outdent-show-entry but are within otherwise concealed regions."
  (interactive)
  (save-excursion
    (outdent-goto-prefix)
    (outdent-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outdent-pre-next-preface)
                                (if (= ?\r (following-char))
                                    (point)
                                  (1- (point))))
                         ?\r)))
;;;_   > outdent-show-current-subtree (&optional arg)
(defun outdent-show-current-subtree (&optional arg)
  "Show everything within the current topic.  With a repeat-count,
expose this topic and its' siblings."
  (interactive "P")
  (save-excursion
    (if (not arg)
	(outdent-flag-current-subtree ?\n)
      (outdent-beginning-of-level)
      (outdent-expose-topic '(* :)))))
;;;_   > outdent-hide-current-subtree (&optional arg just-close)
(defun outdent-hide-current-subtree (&optional arg just-close)
  "Close the current topic, or containing topic if this one is already closed.

With a repeat count, will close this topic and all its siblings.
If this topic is closed and it's a top level topic, close this topic
and its' siblings.

If second optional arg JUST-CLOSE is non-nil, do not treat the parent or
siblings, even if the target topic is already closed."

  (interactive "P")

  (let ((from (point))
	(orig-eol (progn (end-of-line)
			 (if (not (outdent-goto-prefix))
			     (error "No topics found.")
			   (end-of-line)(point)))))
    (outdent-flag-current-subtree ?\r)
    (goto-char from)
    (if (and (= orig-eol (progn (goto-char orig-eol)
				(end-of-line)
				(point)))
	     (not just-close))
	;; Structure didn't change - try hiding current level:
	(progn
	  (goto-char from)
	  (if (outdent-up-current-level 1 t)
	      (outdent-hide-current-subtree)
	    (if (not arg)
		(message "%s - %s"
			 "Top-level topic already closed"
			 "use arg to close all siblings")
	      
	      (goto-char 0)
	      (let ((msg (concat "Top-level topic already closed"
				 " - closing siblings...")))
		(message msg)
		(outdent-expose-topic '(0 :))
		(message (concat msg "  Done.")))))))
    (goto-char from)))
;;;_   > outdent-show-current-branches ()
(defun outdent-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (beginning-of-line)
  (outdent-show-children t))
;;;_   > outdent-hide-current-leaves ()
(defun outdent-hide-current-leaves ()
  "Hide the bodies of the current topic and all its' offspring."
  (interactive)
  (outdent-back-to-current-heading)
  (outdent-hide-region-body (point) (progn (outdent-end-of-current-subtree)
                                           (point))))

;;;_  - Region and beyond
;;;_   > outdent-show-all ()
(defun outdent-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (message "Exposing entire buffer...")
  (outdent-flag-region (point-min) (point-max) ?\n)
  (message "Exposing entire buffer...  Done."))
;;;_   > outdent-hide-bodies ()
(defun outdent-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (outdent-hide-region-body (point-min) (point-max)))
;;;_   > outdent-hide-region-body (start end)
(defun outdent-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(outdent-flag-region (point)
                             (progn (outdent-pre-next-preface) (point)) ?\r)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "[\n\r][\n\r]")
		 2 1)))))))

;;;_   > outdent-expose-topic (spec)
(defun outdent-expose-topic (spec)
  "Apply exposure specs to successive outline topic items.

Use the more convenient frontend, `outdent-new-exposure', if you don't
need evaluation of the arguments, or even better, the `outdent-layout'
variable-keyed mode-activation/auto-exposure feature of outdent mode.
See the respective documentation strings for more details.

Cursor is left at start position.

SPEC is either a number or a list.

Successive specs on a list are applied to successive sibling topics.

A simple spec \(either a number, one of a few symbols, or the null
list) dictates the exposure for the corresponding topic.

Non-null lists recursively designate exposure specs for respective
subtopics of the current topic.

The ':' repeat spec is used to specify exposure for any number of
successive siblings, up to the trailing ones for which there are
explicit specs following the ':'.

Simple (numeric and null-list) specs are interpreted as follows:

 Numbers indicate the relative depth to open the corresponding topic.
     - negative numbers force the topic to be closed before opening to the
       absolute value of the number, so all siblings are open only to
       that level.
     - positive numbers open to the relative depth indicated by the
       number, but do not force already opened subtopics to be closed.
     - 0 means to close topic - hide all offspring.
  :  - 'repeat'
       apply prior element to all siblings at current level, *up to*
       those siblings that would be covered by specs following the ':'
       on the list.  Ie, apply to all topics at level but the last
       ones.  \(Only first of multiple colons at same level is
       respected - subsequent ones are discarded.)
  *  - completely opens the topic, including bodies.
  +  - shows all the sub headers, but not the bodies
  -  - exposes the body of the corresponding topic.

Examples:
\(outdent-expose-topic '(-1 : 0))
	Close this and all following topics at current level, exposing
	only their immediate children, but close down the last topic
	at this current level completely.
\(outdent-expose-topic '(-1 () : 1 0))
	Close current topic so only the immediate subtopics are shown;
	show the children in the second to last topic, and completely
	close the last one.
\(outdent-expose-topic '(-2 : -1 *))
        Expose children and grandchildren of all topics at current
	level except the last two; expose children of the second to
	last and completely open the last one."

  (interactive "xExposure spec: ")
  (if (not (listp spec))
      nil
    (let ((depth (outdent-indentation))
	  (max-pos 0)
	  prev-elem curr-elem
	  stay done
	  snug-back
	  )
      (while spec
	(setq prev-elem curr-elem
	      curr-elem (car spec)
	      spec (cdr spec))
	(cond				; Do current element:
	 ((null curr-elem) nil)
	 ((symbolp curr-elem)
	  (cond ((eq curr-elem '*) (outdent-show-current-subtree)
		 (if (> outdent-recent-end-of-subtree max-pos)
		     (setq max-pos outdent-recent-end-of-subtree)))
		((eq curr-elem '+) (outdent-show-current-branches)
		 (if (> outdent-recent-end-of-subtree max-pos)
		     (setq max-pos outdent-recent-end-of-subtree)))
		((eq curr-elem '-) (outdent-show-current-entry))
		((eq curr-elem ':)
		 (setq stay t)
		 ;; Expand the 'repeat' spec to an explicit version,
		 ;; w.r.t. remaining siblings:
		 (let ((residue	   ; = # of sibs not covered by remaining spec
			;; Dang - could be nice to make use of the chart, sigh:
			(- (length (outdent-chart-siblings))
			   (length spec))))
		   (if (< 0 residue)
		       ;; Some residue - cover it with prev-elem:
		       (setq spec (append (make-list residue prev-elem)
					  spec)))))))
	 ((numberp curr-elem)
	  (if (and (>= 0 curr-elem) (outdent-visible-p))
	      (save-excursion (outdent-hide-current-subtree nil t)
			      (if (> 0 curr-elem)
				  nil
				(if (> outdent-recent-end-of-subtree max-pos)
				    (setq max-pos
					  outdent-recent-end-of-subtree)))))
	  (if (> (abs curr-elem) 0)
	      (progn (outdent-show-children (abs curr-elem))
		     (if (> outdent-recent-end-of-subtree max-pos)
			 (setq max-pos outdent-recent-end-of-subtree)))))
	  ((listp curr-elem)
	   (if (outdent-descend)
	       (let ((got (outdent-expose-topic curr-elem)))
		 (if (and got (> got max-pos)) (setq max-pos got))))))
	(cond (stay (setq stay nil))
	      ((listp (car spec)) nil)
	      ((> max-pos (point))
	       ;; Capitalize on max-pos state to get us nearer next sibling:
	       (progn (goto-char (min (point-max) max-pos))
		      (outdent-next-heading)))
	      ((outdent-next-sibling depth))))
      max-pos)))
;;;_   > outdent-old-expose-topic (spec &rest followers)
(defun outdent-old-expose-topic (spec &rest followers)

  "Deprecated.  Use outdent-expose-topic \(with different schema
format\) instead.

Dictate wholesale exposure scheme for current topic, according to SPEC.

SPEC is either a number or a list.  Optional successive args
dictate exposure for subsequent siblings of current topic.

A simple spec (either a number, a special symbol, or the null list)
dictates the overall exposure for a topic.  Non null lists are
composite specs whose first element dictates the overall exposure for
a topic, with the subsequent elements in the list interpreted as specs
that dictate the exposure for the successive offspring of the topic.

Simple (numeric and null-list) specs are interpreted as follows:

 - Numbers indicate the relative depth to open the corresponding topic:
  - negative numbers force the topic to be close before opening to the
    absolute value of the number.
  - positive numbers just open to the relative depth indicated by the number.
  - 0 just closes
 - '*' completely opens the topic, including bodies.
 - '+' shows all the sub headers, but not the bodies
 - '-' exposes the body and immediate offspring of the corresponding topic.

If the spec is a list, the first element must be a number, which
dictates the exposure depth of the topic as a whole.  Subsequent
elements of the list are nested SPECs, dictating the specific exposure
for the corresponding offspring of the topic.

Optional FOLLOWER arguments dictate exposure for succeeding siblings."

  (interactive "xExposure spec: ")
  (let ((depth (outdent-current-indent))
	done
	max-pos)
    (cond ((null spec) nil)
	  ((symbolp spec)
	   (if (eq spec '*) (outdent-show-current-subtree))
	   (if (eq spec '+) (outdent-show-current-branches))
	   (if (eq spec '-) (outdent-show-current-entry)))
	  ((numberp spec)
	   (if (>= 0 spec)
	       (save-excursion (outdent-hide-current-subtree nil t)
			       (end-of-line)
			       (if (or (not max-pos)
				       (> (point) max-pos))
				   (setq max-pos (point)))
			       (if (> 0 spec)
				   (setq spec (* -1 spec)))))
	   (if (> spec 0)
	     (outdent-show-children spec)))
	  ((listp spec)
	   ;(let ((got (outdent-old-expose-topic (car spec))))
	   ;  (if (and got (or (not max-pos) (> got max-pos)))
	   ;	 (setq max-pos got)))
	   (let ((new-level  (+ (outdent-current-indent) 1))
		 got)
	     (setq max-pos (outdent-old-expose-topic (car spec)))
	     (setq spec (cdr spec))
	     (if (and spec
		      (outdent-descend)
		      (not (outdent-hidden-p)))
		 (progn (setq got (apply 'outdent-old-expose-topic spec))
			(if (and got (or (not max-pos) (> got max-pos)))
			    (setq max-pos got)))))))
    (while (and followers
		(progn (if (and max-pos (< (point) max-pos))
			   (progn (goto-char max-pos)
				  (setq max-pos nil)))
		       (end-of-line)
		       (outdent-next-sibling depth)))
      (outdent-old-expose-topic (car followers))
      (setq followers (cdr followers)))
    max-pos))
;;;_   > outdent-new-exposure '()
(defmacro outdent-new-exposure (&rest spec)
  "Literal frontend for `outdent-expose-topic', doesn't evaluate arguments.
Some arguments that would need to be quoted in outdent-expose-topic
need not be quoted in outdent-new-exposure.

Cursor is left at start position.

Use this instead of obsolete 'outdent-exposure'.

Examples:
\(outdent-exposure (-1 () () () 1) 0)
	Close current topic at current level so only the immediate
	subtopics are shown, except also show the children of the
	third subtopic; and close the next topic at the current level.
\(outdent-exposure : -1 0)
	Close all topics at current level to expose only their
	immediate children, except for the last topic at the current
	level, in which even its' immediate children are hidden.
\(outdent-exposure -2 : -1 *)
        Expose children and grandchildren of first topic at current
	level, and expose children of subsequent topics at current
	level *except* for the last, which should be opened completely."
  (list 'save-excursion
	'(if (not (or (outdent-goto-prefix)
		      (outdent-next-heading)))
	     (error "outdent-new-exposure: Can't find any outline topics."))
	(list 'outdent-expose-topic (list 'quote spec))))
;;;_   > outdent-exposure '()
(defmacro outdent-exposure (&rest spec)
  "Being deprecated - use more recent 'outdent-new-exposure' instead.

Literal frontend for `outdent-old-expose-topic', doesn't evaluate arguments
and retains start position."
  (list 'save-excursion
	'(if (not (or (outdent-goto-prefix)
		      (outdent-next-heading)))
	     (error "Can't find any outline topics."))
	(cons 'outdent-old-expose-topic
	      (mapcar (function (lambda (x) (list 'quote x))) spec))))

;;;_ #6 Alteration

;;;_  - Hooks and sentinels (not yet migrated)
;;;_   > outdent-find-file-hook ()
(defun outdent-find-file-hook ()
  "Activate outdent-mode when `outdent-auto-activation' & `outdent-layout' are non-nil.

See `outdent-init' for setup instructions."
  (if (and outdent-auto-activation
	   (not (outdent-mode-p))
	   outdent-layout)
      (outdent-mode t)))
;;;_   > outdent-post-command-business ()
(defun outdent-post-command-business ()
  "Outdent post-command-hook function.

- Null outdent-override-protect, so it's not left open.

- Implement (and clear) outdent-post-goto-bullet, for hot-spot
  outdent commands.

- Massages buffer-undo-list so successive, standard character self-inserts are
  aggregated.  This kludge compensates for lack of undo bunching when
  before-change-function is used."

					; Apply any external change func:
  (if (not (outdent-mode-p))		; In outdent-mode.
      nil
    (setq outdent-override-protect nil)
    (if outdent-isearch-dynamic-expose
	(outdent-isearch-rectification))
    (if outdent-during-write-cue
	;; Was used by outdent-before-change-protect, done with it now:
	(setq outdent-during-write-cue nil))
    ;; Undo bunching business:
    (if (and (listp buffer-undo-list)	; Undo history being kept.
	     (equal this-command 'self-insert-command)
	     (equal last-command 'self-insert-command))
	(let* ((prev-stuff (cdr buffer-undo-list))
	       (before-prev-stuff (cdr (cdr prev-stuff)))
	       cur-cell cur-from cur-to
	       prev-cell prev-from prev-to)
	  (if (and before-prev-stuff	; Goes back far enough to bother,
		   (not (car prev-stuff)) ; and break before current,
		   (not (car before-prev-stuff)) ; !and break before prev!
		   (setq prev-cell (car (cdr prev-stuff))) ; contents now,
		   (setq cur-cell (car buffer-undo-list)) ; contents prev.

		   ;; cur contents denote a single char insertion:
		   (numberp (setq cur-from (car cur-cell)))
		   (numberp (setq cur-to (cdr cur-cell)))
		   (= 1 (- cur-to cur-from))

		   ;; prev contents denote fewer than aggregate-limit
		   ;; insertions:
		   (numberp (setq prev-from (car prev-cell)))
		   (numberp (setq prev-to (cdr prev-cell)))
					; Below threshold:
		   (> outdent-undo-aggregation (- prev-to prev-from)))
	      (setq buffer-undo-list
		    (cons (cons prev-from cur-to)
			  (cdr (cdr (cdr buffer-undo-list))))))))
    ;; Implement -post-goto-bullet, if set: (must be after undo business)
    (if (and outdent-post-goto-bullet
	     (outdent-current-bullet-pos))
	(progn (goto-char (outdent-current-bullet-pos))
	       (setq outdent-post-goto-bullet nil)))
    ))
;;;_   > outdent-pre-command-business ()
(defun outdent-pre-command-business ()
  "Outdent pre-command-hook function for outdent buffers.

Implements special behavior when cursor is on bullet char.

Self-insert characters are reinterpreted control-character references
into the outdent-mode-map.  The outdent-mode post-command hook will
position a cursor that has moved as a result of such reinterpretation,u
on the destination topic's bullet, when the cursor wound up in the

The upshot is that you can get easy, single (ie, unmodified) key
outdent maneuvering operations by positioning the cursor on the bullet
char.  This mode persists until you deliberately use some regular
cursor-positioning command to relocate the cursor away from a bullet
char."

  (if (not (outdent-mode-p))
      ;; Shouldn't be invoked if not in allout outdent-mode, but just in case:
      nil
    ;; Register isearch status:
    (if (and (boundp 'isearch-mode) isearch-mode)
	(setq outdent-pre-was-isearching t)
      (setq outdent-pre-was-isearching nil))
    (if (and (eq this-command 'self-insert-command)
	     (eq (point)(outdent-current-bullet-pos)))
	(let* ((this-key-num (if (numberp last-command-char)
				 last-command-char
			       0))
	       mapped-binding)
	  (if (zerop this-key-num)
	      nil
					; Map upper-register literals
					; to lower register:
	    (if (<= 96 this-key-num)
		(setq this-key-num (- this-key-num 32)))
					; Check if we have a literal:
	    (if (and (<= 64 this-key-num)
		     (>= 96 this-key-num))
		(setq mapped-binding
		      (lookup-key 'outdent-mode-map
				  (concat outdent-command-prefix
					  (char-to-string (- this-key-num
							     64))))))
	    (if mapped-binding
		(setq outdent-post-goto-bullet t
		      this-command mapped-binding)))))))
;;;_   > outdent-isearch-rectification
(defun outdent-isearch-rectification ()
  "Rectify outdent exposure before, during, or after isearch.

Called as part of outdent-post-command-business."

  (let ((isearching (and (boundp 'isearch-mode) isearch-mode)))
    (cond ((and isearching (not outdent-pre-was-isearching))
	   (outdent-isearch-expose 'start))
	  ((and isearching outdent-pre-was-isearching)
	   (outdent-isearch-expose 'continue))
	  ((and (not isearching) outdent-pre-was-isearching)
	   (outdent-isearch-expose 'final))
	  ;; Not and wasn't isearching:
	  (t (setq outdent-isearch-prior-pos nil)
	     (setq outdent-isearch-did-quit nil)))))
;;;_   > outdent-isearch-expose (mode)
(defun outdent-isearch-expose (mode)
  "Mode is either 'clear, 'start, 'continue, or 'final."
  ;; outdent-isearch-prior-pos encodes exposure status of prior pos:
  ;; (pos was-vis header-pos end-pos)
  ;; pos	- point of concern
  ;; was-vis	- t, else 'topic if entire topic was exposed, 'entry otherwise
  ;; Do reclosure or prior pos, as necessary:
  (if (and (not (eq mode 'start))
	   outdent-isearch-prior-pos)
      (if (listp outdent-isearch-prior-pos)
	  (outdent-flag-region (car (cdr outdent-isearch-prior-pos))
			       (car (cdr (cdr outdent-isearch-prior-pos)))
			       ?\r)))
  (if (outdent-visible-p)
      (setq outdent-isearch-prior-pos nil)
    (if (not (eq mode 'final))
	(setq outdent-isearch-prior-pos (cons (point) (outdent-show-entry)))
      (if outdent-isearch-did-quit
	  nil
	(setq outdent-isearch-prior-pos nil)
	(outdent-show-children))))
  (setq outdent-isearch-did-quit nil))
;;;_   > outdent-enwrap-isearch ()
(defun outdent-enwrap-isearch ()
  "Impose outdent-mode isearch-abort wrapper for dynamic exposure in isearch.

The function checks to ensure that the rebinding is done only once."

  (if (fboundp 'outdent-real-isearch-abort)
      ;; 
      nil
                                        ; Ensure load of isearch-mode:
    (if (or (and (fboundp 'isearch-mode)
                 (fboundp 'isearch-abort))
            (condition-case error 
                (load-library "isearch-mode")
              (file-error (message "Skipping isearch-mode provisions - %s '%s'"
                                   (car (cdr error))
                                   (car (cdr (cdr error))))
                          (sit-for 1)
                          ;; Inhibit subsequent tries and return nil:
                          (setq outdent-isearch-dynamic-expose nil))))
        ;; Isearch-mode loaded, encapsulate specific entry points for
        ;; outdent dynamic-exposure business:
        (progn 
	  ;; stash crucial isearch-mode funcs under known, private
	  ;; names, then register wrapper functions under the old
	  ;; names, in their stead:
          (fset 'outdent-real-isearch-abort (symbol-function 'isearch-abort))
          (fset 'isearch-abort 'outdent-isearch-abort)))))
;;;_   > outdent-isearch-abort ()
(defun outdent-isearch-abort ()
  "Wrapper for outdent-real-isearch-abort \(which see), to register
actual quits."
  (interactive)
  (setq outdent-isearch-did-quit nil)
  (condition-case what
      (outdent-real-isearch-abort)
    (quit (setq outdent-isearch-did-quit t)
	  (signal 'quit nil))))

;;;_  - Outline Alteration
;;;_   : Topic Modification
;;;_    = outdent-former-auto-filler
(defvar outdent-former-auto-filler nil
  "Name of modal fill function being wrapped by outdent-auto-fill.")
;;;_    > outdent-shift-in (&optional levels)
(defun outdent-current-bullet-pos ()
  outdent-recent-prefix-end)

(defun outdent-shift-in (&optional levels)

  "Increase indentation of current topic and offspring.

Optional arg LEVELS specifies amount of levels \(relative to
containing topic), with negative numbers indicating outwards shift."

  (let* ((current-level (outdent-indentation))
         (new-level (or new-level current-level))
         (mb outdent-recent-prefix-beginning)
         (me outdent-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (outdent-make-topic-prefix current-bullet
                                                nil
                                                new-level
                                                solicit
                                                number-control
                                                index)))

    ;; Is new one is identical to old?
    (if (and (= current-level new-level)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
	;; Nothing to do:
        t

      ;; New prefix probably different from old:
					; get rid of old one:
      (outdent-unprotected (delete-region mb me))
      (goto-char mb)
					; Put in new prefix:
      (outdent-unprotected (insert-string new-prefix))

      ;; Reindent the body if elected and margin changed:
      (if (and outdent-reindent-bodies
	       (not (= new-level current-level)))
	  (outdent-reindent-body current-level new-level))

      ;; Recursively rectify successive siblings of orig topic if
      ;; caller elected for it:
      (if do-successors
	  (save-excursion
	    (while (outdent-next-sibling new-level nil)
	      (setq index
		    (cond ((numberp index) (1+ index))
			  ((not number-control)  (outdent-sibling-index))))
	      (outdent-rebullet-heading nil		;;; solicit
					new-level	;;; new-level
					nil		;;; number-control
					index		;;; index
					nil)))		;;;(dont!)do-successors
      )	; (if (and (= current-level new-level)...))
    ) ; let* ((current-level (outdent-indentation))...)
  ) ; defun
;;;_    > outdent-rebullet-heading (&optional solicit ...)
(defun outdent-rebullet-heading (&optional solicit
                                           new-level
                                           number-control
                                           index
                                           do-successors)

  "Adjust bullet of current topic prefix.

All args are optional.

If SOLICIT is non-nil then the choice of bullet is solicited from
user.  Otherwise the distinctiveness of the bullet or the topic
depth determines it.

Second arg DEPTH forces the topic prefix to that depth, regardless
of the topics current depth.

Third arg NUMBER-CONTROL can force the prefix to or away from
numbered form.  It has effect only if 'outdent-numbered-bullet' is
non-nil and soliciting was not explicitly invoked (via first arg).
Its effect, numbering or denumbering, then depends on the setting
of the forth arg, INDEX.

If NUMBER-CONTROL is non-nil and forth arg INDEX is nil, then the
prefix of the topic is forced to be non-numbered.  Null index and
non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
INDEX is a number, then that number is used for the numbered
prefix.  Non-nil and non-number means that the index for the
numbered prefix will be derived by outdent-make-topic-prefix.

Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
siblings.

Cf vars 'outdent-stylish-prefixes', 'outdent-old-style-prefixes',
and 'outdent-numbered-bullet', which all affect the behavior of
this function."

  (let* ((current-level (outdent-indentation))
         (new-level (or new-level current-level))
         (mb outdent-recent-prefix-beginning)
         (me outdent-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (outdent-make-topic-prefix current-bullet
                                                nil
                                                new-level
                                                solicit
                                                number-control
                                                index)))

    ;; Is new one is identical to old?
    (if (and (= current-level new-level)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
	;; Nothing to do:
        t

      ;; New prefix probably different from old:
					; get rid of old one:
      (outdent-unprotected (delete-region mb me))
      (goto-char mb)
					; Put in new prefix:
      (outdent-unprotected (insert-string new-prefix))

      ;; Reindent the body if elected and margin changed:
      (if (and outdent-reindent-bodies
	       (not (= new-level current-level)))
	  (outdent-reindent-body current-level new-level))

      ;; Recursively rectify successive siblings of orig topic if
      ;; caller elected for it:
      (if do-successors
	  (save-excursion
	    (while (outdent-next-sibling new-level nil)
	      (setq index
		    (cond ((numberp index) (1+ index))
			  ((not number-control)  (outdent-sibling-index))))
	      (outdent-rebullet-heading nil		;;; solicit
					new-level	;;; new-level
					nil		;;; number-control
					index	;;; index
					nil)))))	;;;(dont!)do-successors
      )	; (if (and (= current-level new-level)...))
    ) ; let* ((current-level (outdent-indentation))...)
  ) ; defun
;;;_    > outdent-rebullet-topic (arg)
(defun outdent-rebullet-topic (arg)
  "Like outdent-rebullet-topic-grunt, but start from topic visible at point.

Descends into invisible as well as visible topics, however.

With repeat count, shift topic depth by that amount."
  (interactive "P")
  (let ((start-col (current-column))
        (was-eol (eolp)))
    (save-excursion
      ;; Normalize arg:
      (cond ((null arg) (setq arg 0))
            ((listp arg) (setq arg (car arg))))
      ;; Fill the user in, in case we're shifting a big topic:
      (if (not (zerop arg)) (message "Shifting..."))
      (outdent-back-to-current-heading)
      (if (< (+ (outdent-recent-indent) arg) 0)
          (error "Attempt to shift topic below level 1"))
      (outdent-rebullet-topic-grunt arg)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (max 0 (+ start-col arg)))))
;;;_     > outdent-rebullet-topic-grunt (&optional relative-level ...)
(defun outdent-rebullet-topic-grunt (&optional relative-level
                                               starting-level
                                               starting-point
                                               index
                                               do-successors)

  "Rebullet the topic at point, visible or invisible, and all
contained subtopics.  See outdent-rebullet-heading for rebulleting
behavior.

All arguments are optional.

First arg RELATIVE-LEVEL means to shift the depth of the entire
topic that amount.

The rest of the args are for internal recursive use by the function
itself.  The are STARTING-LEVEL, STARTING-POINT, and INDEX."

  (let* ((relative-level (or relative-level 0))
         (new-level (outdent-indentation))
         (starting-level (or starting-level new-level))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-level)
                             (not on-starting-call))
                         (outdent-sibling-index))))
         (moving-outwards (< 0 relative-level))
         (starting-point (or starting-point (point))))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> -1 (+ starting-level relative-level))
         (error "Attempt to shift topic out beyond level 1."))	;;; ====>

    (cond ((= starting-level new-level)
           ;; We're at depth to work on this one:
           (outdent-rebullet-heading nil		;;; solicit
                                     (+ starting-level	;;; starting-level
                                        relative-level)
                                     nil		;;; number
                                     index		;;; index
                                     ;; Every contained topic will get hit,
                                     ;; and we have to get to outside ones
                                     ;; deliberately:
                                     nil)		;;; do-successors
           ;; ... and work on subsequent ones which are at greater depth:
           (setq index 0)
           (outdent-next-heading)
           (while (and (not (eobp))
                       (< starting-level (outdent-recent-indent)))
             (setq index (1+ index))
             (outdent-rebullet-topic-grunt relative-level   ;;; relative-level
                                           (1+ starting-level);;;starting-level
                                           starting-point   ;;; starting-point
                                           index)))	    ;;; index

          ((< starting-level new-level)
           ;; Rare case - subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (outdent-rebullet-topic-grunt relative-level   ;;; relative-level
                                         new-level	  ;;; starting-level
                                         starting-point	  ;;; starting-point
                                         index)))	  ;;; index

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-level))
                       (or (= (outdent-recent-indent) starting-level)
                           (= (outdent-recent-indent) (+ starting-level
                                                        relative-level)))))
              (outdent-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-level))
                     (outdent-rebullet-heading nil nil nil nil t)))))
    )
  )
;;;_    > outdent-auto-fill ()
(defun outdent-auto-fill ()
  "Outdent-mode autofill function.

Maintains outline hanging topic indentation if
`outdent-use-hanging-indents' is set."
  (let ((fill-prefix (if outdent-use-hanging-indents
                         ;; Check for topic header indentation:
                         (save-excursion
                           (beginning-of-line)
                           (if (looking-at outdent-regexp)
                               ;; ... construct indentation to account for
                               ;; length of topic prefix:
                               (make-string (progn (outdent-end-of-prefix)
                                                   (current-column))
                                            ?\ ))))))
    (if (or outdent-former-auto-filler outdent-use-hanging-indents)
        (do-auto-fill))))
;;;_    > outdent-reindent-body (old-level new-level &optional number)
(defun outdent-reindent-body (old-level new-level &optional number)
  "Reindent body lines which were indented at old-level to new-level.

Optional arg NUMBER indicates numbering is being added, and it must
be accomodated.

Note that refill of indented paragraphs is not done."

  (save-excursion
    (outdent-end-of-prefix)
    (let* ((new-margin (current-column))
	   excess old-indent-begin old-indent-end
	   curr-ind
	   ;; We want the column where the header-prefix text started
	   ;; *before* the prefix was changed, so we infer it relative
	   ;; to the new margin and the shift in depth:
	   (old-margin (+ old-level (- new-margin new-level))))
             
      ;; Process lines up to (but excluding) next topic header:
      (outdent-unprotected
       (save-match-data
         (while
	     (and (re-search-forward "[\n\r]\\(\\s-*\\)"
				     nil
				     t)
		  ;; Register the indent data, before we reset the
		  ;; match data with a subsequent 'looking-at':
		  (setq old-indent-begin (match-beginning 1)
			old-indent-end (match-end 1))
		  (not (looking-at outdent-regexp)))
	   (if (> 0 (setq excess (- (current-column)
				     old-margin)))
	       ;; Text starts left of old margin - don't adjust:
	       nil
	     ;; Text was hanging at or right of old left margin -
	     ;; reindent it, preserving its existing indentation
	     ;; beyond the old margin:
	     (delete-region (1+ old-indent-begin) old-indent-end)
	     (indent-to (+ new-margin excess)))))))))
;;;_    > outdent-shift-in (arg)
(defun old-outdent-shift-in (arg)
  "Increase depth of current heading and any topics collapsed within it."
  (interactive "p")
  (outdent-rebullet-topic arg))
;;;_    > outdent-shift-out (arg)
(defun outdent-shift-out (arg)
  "Decrease depth of current heading and any topics collapsed within it."
  (interactive "p")
  (outdent-rebullet-topic (* arg -1)))
;;;_   : Surgery (kill-ring) functions with special provisions for outlines:
;;;_    > outdent-kill-line (&optional arg)
(defun outdent-kill-line (&optional arg)
  "Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")
  (if (not (and (outdent-mode-p)		; active outline mode,
		(bolp)				; may be clipping topic head,
		(looking-at outdent-regexp)))	; are clipping topic head.
      ;; Above conditions do not obtain - just do a regular kill:
      (kill-line arg)
    ;; Ah, have to watch out for adjustments:
    (let* ((depth (outdent-indentation)))
                                        ; Do the kill:
      (kill-line arg)
                                        ; Provide some feedback:
      (sit-for 0)
      (save-excursion
                                        ; Start with the topic
                                        ; following killed line:
        (if (not (looking-at outdent-regexp))
            (outdent-next-heading))))))
;;;_    > outdent-kill-topic ()
(defun outdent-kill-topic ()
  "Kill topic together with subtopics.

Leaves primary topic's trailing vertical whitespace, if any."

  (interactive)
  (let* ((beg (progn
                (outdent-back-to-current-heading) (beginning-of-line) (point)))
         (depth (outdent-recent-indent)))
    (outdent-end-of-current-subtree)
    (if (not (eobp))
	(if (or (not (looking-at "^$"))
		;; A blank line - cut it with this topic *unless* this
		;; is the last topic at this level, in which case
		;; we'll leave the blank line as part of the
		;; containing topic:
		(save-excursion
		  (and (outdent-next-heading)
		       (>= (outdent-recent-indent) depth))))
	    (forward-char 1)))
	
    (kill-region beg (point))))
;;;_    > outdent-yank-processing () - this is not yet reconciled
(defun outdent-yank-processing (&optional arg)

  "Incidental outdent-specific business to be done just after text yanks.

Does depth adjustment of yanked topics, when:

1 the stuff being yanked starts with a valid outline header prefix, and
2 it is being yanked at the end of a line which consists of only a valid
     topic prefix.

Also, adjusts numbering of subsequent siblings when appropropriate.

Depth adjustment alters the depth of all the topics being yanked
the amount it takes to make the first topic have the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, are left exactly like normal, non-outdent-specific yanks."

  (interactive "*P")
					; Get to beginning, leaving
					; region around subject:
  (if (< (mark-marker) (point))
      (exchange-point-and-mark))
  (let* ((subj-beg (point))
	 (subj-end (mark-marker))
	 ;; 'resituate' if yanking an entire topic into topic header:
	 (resituate (and (outdent-e-o-prefix-p)
			 (looking-at (concat "\\(" outdent-regexp "\\)"))
			 (outdent-prefix-data (match-beginning 1)
					      (match-end 1)))))
    (if resituate
                                        ; The yanked stuff is a topic:
	(let* ((prefix-len (- (match-end 1) subj-beg))
	       (subj-level (outdent-recent-indent))
	       (adjust-to-level
		;; Nil if adjustment unnecessary, otherwise depth to which
		;; adjustment should be made:
		(save-excursion
		  (and (goto-char subj-end)
		       (eolp)
		       (goto-char subj-beg)
		       (and (looking-at outdent-regexp)
			    (progn
			      (beginning-of-line)
			      (not (= (point) subj-beg)))
			    (looking-at outdent-regexp)
			    (outdent-goto-prefix)
			    (outdent-prefix-data (match-beginning 0)
						 (current-column)))
		       (outdent-recent-indent))))
	       done
	       (more t))
	  (if adjust-to-level
                                        ; Do the adjustment:
	      (progn
		(message "... yanking") (sit-for 0)
		(save-restriction
		  (narrow-to-region subj-beg subj-end)
                                        ; Trim off excessive blank
                                        ; line at end, if any:
		  (goto-char (point-max))
		  (if (looking-at "^$")
		      (outdent-unprotected (delete-char -1)))
                                        ; Work backwards, with each
                                        ; shallowest level,
                                        ; successively excluding the
                                        ; last processed topic from
                                        ; the narrow region:
		  (while more
		    (outdent-back-to-current-heading)
                                        ; go as high as we can in each bunch:
		    (while (outdent-ascend))
		    (save-excursion
		      (outdent-rebullet-topic-grunt (- adjust-to-level
						       subj-level))
		      (outdent-indentation))
		    (if (setq more (not (bobp)))
			(progn (widen)
			       (forward-char -1)
			       (narrow-to-region subj-beg (point))))))
		(message ""))
	    (exchange-point-and-mark))))
    (if (not resituate)
      (exchange-point-and-mark))))
;;;_    > outdent-yank (&optional arg)
(defun outdent-yank (&optional arg)
  "Outdent-mode yank, with depth and numbering adjustment of yanked topics.

Non-topic yanks work no differntly than normal yanks.

If a topic is being yanked into a bare topic prefix, the depth of the
yanked topic is adjusted to the depth of the topic prefix.

  1 we're yanking in an outdent-mode buffer
  2 the stuff being yanked starts with a valid outline header prefix, and
  3 it is being yanked at the end of a line which consists of only a valid
    topic prefix.

If these conditions hold then the depth of the yanked topics are all
adjusted the amount it takes to make the first one at the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, (ones that don't qualify for adjustment) are handled
exactly like normal yanks.

Numbering of yanked topics, and the succesive siblings at the depth
into which they're being yanked, is adjusted.

Outdent-yank-pop works with outdent-yank just like normal yank-pop
works with normal yank in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (yank arg)
  (if (outdent-mode-p)
      (outdent-yank-processing)))
;;;_    > outdent-yank-pop (&optional arg)
(defun outdent-yank-pop (&optional arg)
  "Yank-pop like outdent-yank when popping to bare outline prefixes.

Adapts level of popped topics to level of fresh prefix.

Note - prefix changes to distinctive bullets will stick, if followed
by pops to non-distinctive yanks.  Bug..."

  (interactive "*p")
  (setq this-command 'yank)
  (yank-pop arg)
  (if (outdent-mode-p)
      (outdent-yank-processing)))

;;;_  - Specialty bullet functions
;;;_   : File Cross references
;;;_    > outdent-resolve-xref ()
(defun outdent-resolve-xref ()
  "Pop to file associated with current heading, if it has an xref bullet.

\(Works according to setting of `outdent-file-xref-bullet')."
  (interactive)
  (if (not outdent-file-xref-bullet)
      (error
       "outline cross references disabled - no 'outdent-file-xref-bullet'")
    (if (not (string= (outdent-current-bullet) outdent-file-xref-bullet))
        (error "current heading lacks cross-reference bullet '%s'"
               outdent-file-xref-bullet)
      (let (file-name)
        (save-excursion
          (let* ((text-start outdent-recent-prefix-end)
                 (heading-end (progn (outdent-pre-next-preface)
                                     (point))))
            (goto-char text-start)
            (setq file-name
                  (if (re-search-forward "\\s-\\(\\S-*\\)" heading-end t)
                      (buffer-substring (match-beginning 1) (match-end 1))))))
        (setq file-name
              (if (not (= (aref file-name 0) ?:))
                  (expand-file-name file-name)
                                        ; A registry-files ref, strip the ':'
                                        ; and try to follow it:
                (let ((reg-ref (reference-registered-file
                                (substring file-name 1) nil t)))
                  (if reg-ref (car (cdr reg-ref))))))
        (if (or (file-exists-p file-name)
                (if (file-writable-p file-name)
                    (y-or-n-p (format "%s not there, create one? "
                                      file-name))
                  (error "%s not found and can't be created" file-name)))
            (condition-case failure
                (find-file-other-window file-name)
              (error failure))
          (error "%s not found" file-name))
        )
      )
    )
  )

;;;_* Local emacs vars.
;;; The following `outline-layout' local variable setting:
;;;  - closes all topics from the first topic to just before the third-to-last,
;;;  - shows the children of the third to last (config vars)
;;;  - and the second to last (code section),
;;;  - and closes the last topic (this local-variables section).
;;;Local variables:
;;;outline-layout: (0 : -1 -1 0)
;;;End:

;; outdent.el ends here
