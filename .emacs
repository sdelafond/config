(require 'cl) ;; (loop for ...)

;;(setq debug-on-error t)

;; _____________________________________________________________________
;; custom path
(setq my-home (expand-file-name (concat "~" (or (getenv "SUDO_USER") (getenv "USER")))))
(setq my-name "Sébastien Delafond")
(setq my-emacsd (concat my-home "/.emacs.d/"))
(loop for file in (directory-files my-emacsd t ".*.el$")
      do (load-file file))
(setq auto-save-list-file-prefix my-emacsd)
(setq user-emacs-directory my-emacsd)
(setq load-path (cons (concat my-emacsd "/lisp") load-path))
(setq load-path (cons "/usr/share/org-mode/lisp" load-path))

;; _____________________________________________________________________
;; macros
(defmacro make-interactive-fun (fn args)
  `(lambda () (interactive) (funcall ,fn ,args)))

(defmacro make-fun (fn args)
  `(lambda () (funcall ,fn ,args)))

(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format 
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

;; _____________________________________________________________________
;; functions
(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))
(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

(defun save-current-kbd-macro-to-dot-emacs (name)
  "Save the current macro as named function definition inside your
   initialization file so you can reuse it anytime in the future."
  (interactive "SSave Macro as: ")
  (name-last-kbd-macro name)
  (save-excursion 
    (find-file-literally user-init-file)
    (goto-char (point-max))
    (insert "\n\n;; Saved macro\n")
    (insert-kbd-macro name)
    (insert "\n")))

(defun file-change-too-close-for-comfort ()
  (let* ((file-time-raw (nth 5 (file-attributes (buffer-file-name))))
         (file-time (+ (lsh (nth 0 file-time-raw) 16) (nth 1 file-time-raw)))
         (current-time (+ (lsh (nth 0 (current-time)) 16) (nth 1 (current-time)))))
    (and (eq current-time file-time)
         (message "%s: postpone revert" (buffer-name))
         t)))

(defun my-auto-revert-handler ()
  "Revert current buffer, if appropriate.
   This is an internal function used by Auto-Revert Mode.  We rewrite it so it
   postpones an auto-revert if the last mod time of the file is the same as
   the current system time. It’ll pick it up the next auto-revert iteration,
   which appears to be to be every 2 seconds by default.
   From http://printfdebugger.tumblr.com/post/25085225264/unfortunate-emacs-auto-revert-mode-and-git-pull"
  (when (or auto-revert-tail-mode (not (buffer-modified-p)))
    (let* ((buffer (current-buffer)) size
           (revert
            (or (and buffer-file-name
                     (file-readable-p buffer-file-name)
                     (if auto-revert-tail-mode
                         ;; Tramp caches the file attributes.  Setting
                         ;; `tramp-cache-inhibit' forces Tramp to
                         ;; reread the values.
                         (let ((tramp-cache-inhibit-cache t))
                           (/= auto-revert-tail-pos
                               (setq size
                                     (nth 7 (file-attributes
                                             buffer-file-name)))))
                       (and (not (file-remote-p buffer-file-name))
                            (not (verify-visited-file-modtime buffer))
                            (not (file-change-too-close-for-comfort)))))
                (and (or auto-revert-mode
                         global-auto-revert-non-file-buffers)
                     revert-buffer-function
                     (boundp 'buffer-stale-function)
                     (functionp buffer-stale-function)
                     (funcall buffer-stale-function t))))
           eob eoblist)
      (when revert
        (when (and auto-revert-verbose
                   (not (eq revert 'fast)))
          (message "Reverting buffer `%s'." (buffer-name)))
        ;; If point (or a window point) is at the end of the buffer,
        ;; we want to keep it at the end after reverting.  This allows
        ;; to tail a file.
        (when buffer-file-name
          (setq eob (eobp))
          (walk-windows
           #'(lambda (window)
               (and (eq (window-buffer window) buffer)
                    (= (window-point window) (point-max))
                    (push window eoblist)))
           'no-mini t))
        (if auto-revert-tail-mode
            (auto-revert-tail-handler size)
          ;; Bind buffer-read-only in case user has done C-x C-q,
          ;; so as not to forget that.  This gives undesirable results
          ;; when the file's mode changes, but that is less common.
          (let ((buffer-read-only buffer-read-only))
            (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes)))
        (when buffer-file-name
          (when eob (goto-char (point-max)))
          (dolist (window eoblist)
            (set-window-point window (point-max)))))
      ;; `preserve-modes' avoids changing the (minor) modes.  But we
      ;; do want to reset the mode for VC, so we do it manually.
      (when (or revert auto-revert-check-vc-info)
        (vc-find-file-hook)))))

(defun format-email-body ()
  "Format email body, respecting (or at least trying to) quote levels."
  (interactive)
  (save-excursion
    (let ((re1 "^\\(> \\)+\\w")
          (re2 "^\\(> *\\)*$"))
      (goto-char (point-min))
      (while (<= (point) (point-max))
        (progn
          (search-forward-regexp re1 nil t)
          (move-beginning-of-line nil)
          (set-mark-command nil)
          (search-forward-regexp re2 nil t)
          (search-forward-regexp re1)
          (previous-line)
          (move-beginning-of-line nil)
          (fill-paragraph nil t))))))

(defun id ()
  (interactive)
  (insert (concat my-name " <" my-email ">")))

(defun insert-sig-fr ()
  (interactive)
  (insert (concat "Bien cordialement," "\n\n" "-- \nSD\n")))
(global-set-key (kbd "C-c s") 'insert-sig-fr)
(defun insert-sig-en ()
  (interactive)
  (insert (concat "Cheers," "\n\n" "-- \nSeb\n")))
(global-set-key (kbd "C-c S") 'insert-sig-en)

(defun system-short-name ()
  (car (split-string system-name "\\.")))

(defun ts ()
  (interactive)
  (shell-command "date -R" t))

(defun my-browse-url-tab (url &optional new-window)
  "Open URL"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((cmd "firefox"))
    (start-process (concat cmd " " url) "*Messages*" cmd url)))

(defun paste-and-shift (arg)
  (interactive)
  (let ((begin (point)))
    (insert arg)
    (indent-rigidly begin (point) 2)))

(defun kill-to-eof ()
  (interactive)
  (kill-region (point) (point-max)))
(global-set-key (kbd "C-c k") 'kill-to-eof)

(defun show-file-name ()
  "Show the full path file name in the minibuffer"
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))
(global-set-key (kbd "C-c z") 'show-file-name)

(defun update-ssh-agent-info ()
  "Update SSH_AUTH_SOCK"
  (interactive)
  (let* ((cmd ". ~/.zsh.function ; refresh-ssh-agent-info ; echo -n $SSH_AUTH_SOCK")
	 (new (shell-command-to-string cmd)))
    (setenv "SSH_AUTH_SOCK" new)
    (message (concat "SSH_AUTH_SOCK=" new))))

(defun my-backup-enable-predicate (filename)
  "Do not create backups for certain files."
  (when (normal-backup-enable-predicate filename)
    (not (or (string-match "svn-commit" filename)
	     (string-match "dwssap" filename)
             (string-match "MSG" filename)
	     (string-match "passwd" filename)
	     (string-match "/tmp/dpep" filename)))))
(setq backup-enable-predicate 'my-backup-enable-predicate)

(defun my-autoload (&rest modes)
  "Autoload each mode listed in MODES."
  (loop for mode in modes do (autoload (intern mode) mode nil t)))
(my-autoload "id" "ace-jump-mode" "align"
	     "multi-mode" "org" "time-stamp" "pf-mode"
	     "gtags" "outdent" "vcl-mode")

(defun add-function-to-hooks (fun modes-hooks)
  "Add a call to FUN to each mode-hook listed in MODES-HOOKS."
  (loop for mode-hook in modes-hooks do
	(add-hook mode-hook fun)))
(add-function-to-hooks (make-fun 'set-fill-column 78) '(c-mode-hook lisp-mode-hook
                                                        emacs-lisp-mode-hook
                                                        html-mode-hook))
(add-function-to-hooks (make-fun 'set-fill-column 72) '(text-mode-hook))

;; ELPA/MELPA
(if (>= emacs-major-version 24) 
    (require 'package)
  (load-file (concat my-emacsd "/23/package.el")))
(setq tls-checktrust t)
(setq gnutls-verify-error t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(let ((trustfile "/etc/ssl/certs/ca-certificates.crt"))
  (setq gnutls-trustfiles (list trustfile))
  (setq tls-program
        (list
         (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile))))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("melpa-stable" . 10)
				   ("gnu"          . 5)
				   ("melpa"        . 1)))

(package-initialize)
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

;; use-package extensions
(use-package use-package-chords
  :config (key-chord-mode 1))

;; _____________________________________________________________________
;; Hooks
(defun seb/mutt-hook ()
  (progn 
    (mail-mode)
    (turn-on-orgtbl)
    (local-set-key (kbd "C-c n i") 'org-footnote-new)
    (local-set-key (kbd "C-c n p") 'org-mark-ring-goto)
    (local-set-key (kbd "C-c i") 'format-email-body)))

(defun my-org-mode-hook ()
;;   (require 'org-expiry)
;;   (org-expiry-insinuate)
;;   (setq org-expiry-handler-function 'org-expiry-archive-subtree)

;;   (require 'org-crypt)
;;   (org-crypt-use-before-save-magic)
;;   (setq org-crypt-key "sdelafond@gmx.net")
;;   (add-hook 'before-save-hook 'org-encrypt-entries)
  (require 'ob-ruby)
  (require 'ob-python)
  (setq org-babel-python-command "python3")
  (require 'ob-js)
  (require 'ob-ditaa)
  ;; (defun my-org-confirm-babel-evaluate (lang body)
  ;;   (not (string= lang "ditaa")))  ; don't ask for ditaa
  ;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-confirm-babel-evaluate nil
        org-babel-min-lines-for-block-output 1
        org-babel-default-header-args
        (cons '(:noweb . "yes")
              (assq-delete-all :noweb org-babel-default-header-args))
        org-babel-default-header-args
        (cons '(:exports . "both")
              (assq-delete-all :exports org-babel-default-header-args))
        org-babel-default-header-args
        (cons '(:results . "output verbatim replace")
              (assq-delete-all :results org-babel-default-header-args)))
  (require 'ob-shell)
  (require 'ob-sql)
  (require 'ob-emacs-lisp)
  (setq org-src-fontify-natively t)

  (require 'org-inlinetask)
  (setq org-inlinetask-default-state "TODO")
  (setq org-inlinetask-min-level 8)
  (define-key org-mode-map "\C-c\C-xt" 'org-inlinetask-insert-task)

  (require 'org-protocol)

  (require 'org-tempo)

  (use-package org-ql
    :config
    (require 'org-ql-view)
    (add-to-list 'org-ql-views
		 '("TODO"
		   :buffers-files
		   org-agenda-files
		   :query
		   (and (not (done)) (ts-active :from today :to 4))
		   :sort
		   (date priority)
		   :narrow nil :super-groups org-super-agenda-groups :title "TODO"))
    (add-to-list 'org-ql-views
    		 '("NO DEADLINE"
		   :buffers-files
		   (lambda () (cons "~/org/links.org" org-agenda-files))
    		   :query
    		   (and (todo) (not (deadline)))
    		   :sort
    		   (date priority)
    		   :narrow nil :super-groups org-super-agenda-groups :title "NO DEADLINE")))

  (use-package org-roam
    :init
    (setq org-roam-v2-ack t)
    :config
    (setq org-roam-directory "~/vcs/roam"))

  ;; adapted from
  ;; https://github.com/ag91/ag91.github.io/blob/source/blog/LeadYourFutureWithOrg.org
  (defun seb/org/get-tasks (todo-tag from &optional tag to files category)
    "Get stats for tasks of last week with TODO-TAG TAG FROM optionally define TO date and source FILES to use."
    (org-ql-query
     :from (or files (org-agenda-files))
     :where
     `(and (todo ,todo-tag)
	   (if ,tag (tags ,tag) t)
	   (if ,category (category ,category) t)
	   (ts :from ,from :to ,(or to 'today)))))

  (defun seb/org/get-stats-tasks (todo-tag from &optional tag to files category)
    "Get stats for tasks of last week with TODO-TAG TAG FROM optionally define TO date and source FILES to use."
    (let ((tasks (seb/org/get-stats-tasks todo-tag from tag to files category))
      `((tasks . ,(length tasks))
	(tasks-per-day . ,(/ (length tasks) (abs from)))))))

  ;; (require 'ox-confluence)
  (require 'ox-beamer)
  (require 'ox-md)

  (use-package ox-pandoc)

  (setq org-ellipsis " ▼")
  (setq org-footnote-auto-adjust t)

  (toggle-word-wrap)

  ;; LaTeX
  (require 'ox-latex)
  (setq org-export-latex-listings t)
  ;; (add-to-list 'org-export-latex-packages-alist '("" "listings"))
  ;; (add-to-list 'org-export-latex-packages-alist '("" "color"))
  (setq org-export-latex-emphasis-alist
        '(("*" "\\textbf{%s}" nil)
          ("/" "\\emph{%s}" nil)
          ("_" "\\underline{%s}" nil)
          ("+" "\\st{%s}" nil)
          ("=" "\\url{%s}" nil)
          ;; `url' breaks lines in long strings (was `verb')
          ("~" "\\verb~%s~" t)
          ("@" "\\alert{%s}" nil)))

  ;; clock
  (setq org-clock-into-drawer t)
  ;; (setq org-clock-clocktable-default-properties '(:maxlevel 5 :formula "$7='(org-clock-time% @2$2 $2..$6);%1.f" :narrow 80!))
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :formula % :narrow 80!))

  ;; speed commands
  (setq org-use-speed-commands t)
  (setq org-speed-commands-user (quote (("S" . widen))))

  ;; agenda
  (use-package org-super-agenda
    :config
    (org-super-agenda-mode t))
  (setq org-agenda-include-diary nil)
  (setq org-agenda-span 'year)
  (setq org-agenda-start-day "-4m")
  (setq org-agenda-show-all-dates nil)
  (setq org-agenda-show-log nil)
  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-delay-if-deadline t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timeline-if-done t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  ;; (setq org-combined-agenda-icalendar-file "~/org/org.ics")
  (setq org-icalendar-categories '(all-tags))
  (setq org-icalendar-store-UID t)
  (setq org-icalendar-use-deadline '(event-if-todo))
;; (setq org-icalendar-include-todo t)
  (setq org-deadline-warning-days 0)
  (setq org-default-priority 67)
  (setq org-duration-format (quote h:mm))
;; (setq org-fast-tag-selection-single-key 'expert)

  (setq org-adapt-indentation t)
  (setq org-fast-tag-selection-single-key t)
  (setq org-return-follows-link t)
  (setq org-hide-leading-stars t)
  (setq org-id-locations-file (concat my-emacsd "org-id-locations"))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-lowest-priority 69)
  (setq org-popup-calendar-for-date-prompt nil)
  (setq org-reverse-note-order nil)
  (setq org-use-fast-todo-selection t)
  (setq org-use-sub-superscripts nil)
  ;; (setq org-agenda-custom-commands
  ;;       (quote (("c" todo "DONE|LATER|CANCELED" nil)
  ;;       	("w" todo "TODO|WAITING" nil)
  ;;       	("Z" "blah" todo "WAITING")
  ;;       	("Y" "Weekly Review......." todo "TODO"
  ;;       	 ((agenda (org-agenda-ndays 7))))
  ;;       	("W" agenda "Month" ((org-agenda-ndays 30)))
  ;;       	("A" agenda "Custom"
  ;;       	 ((org-agenda-skip-function
  ;;       	   (lambda nil
  ;;       	     (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
  ;;       	  (org-agenda-ndays 1)
  ;;       	  (org-agenda-overriding-header "Today's Priority #A tasks: ")))
  ;;       	("B" agenda "No (CANCELED|DONE|LATER)"
  ;;       	 ((org-agenda-skip-function '(org-agenda-skip-subtree-if
  ;;       				      'regexp "\\* \\(CANCELED\\|LATER\\|DONE\\)")))
  ;;       	  (org-agenda-ndays 7)
  ;;       	  (org-show-hierarchy-above t)
  ;;       	  (org-agenda-overriding-header "No (CANCELED|DONE|LATER): ")))
  ;;       	("u" alltodo ""
  ;;       	 ((org-agenda-skip-function
  ;;       	   (lambda nil
  ;;       	     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
  ;;       				       (quote regexp) "<[^>\n]+>")))
  ;;       	  (org-agenda-overriding-header "Unscheduled TODO entries: ")))))

  ;; (define-key org-agenda-mode-map
  ;;   "v" 'hydra-org-agenda-view/body)

  (defun org-agenda-cts ()
    (let ((args (get-text-property
		 (min (1- (point-max)) (point))
		 'org-last-args)))
      (nth 2 args)))

;;   (defhydra hydra-org-agenda-view (:hint nil)
;;     "
;; _d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
;; _w_: ?w? week       _[_: inactive      _A_: arch-files
;; _t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
;; _m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
;; _y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
;;     ("SPC" org-agenda-reset-view)
;;     ("d" org-agenda-day-view
;;      (if (eq 'day (org-agenda-cts))
;; 	 "[x]" "[ ]"))
;;     ("w" org-agenda-week-view
;;      (if (eq 'week (org-agenda-cts))
;; 	 "[x]" "[ ]"))
;;     ("t" org-agenda-fortnight-view
;;      (if (eq 'fortnight (org-agenda-cts))
;; 	 "[x]" "[ ]"))
;;     ("m" org-agenda-month-view
;;      (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
;;     ("y" org-agenda-year-view
;;      (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
;;     ("l" org-agenda-log-mode
;;      (format "% -3S" org-agenda-show-log))
;;     ("L" (org-agenda-log-mode '(4)))
;;     ("c" (org-agenda-log-mode 'clockcheck))
;;     ("f" org-agenda-follow-mode
;;      (format "% -3S" org-agenda-follow-mode))
;;     ("a" org-agenda-archives-mode)
;;     ("A" (org-agenda-archives-mode 'files))
;;     ("r" org-agenda-clockreport-mode
;;      (format "% -3S" org-agenda-clockreport-mode))
;;     ("e" org-agenda-entry-text-mode
;;      (format "% -3S" org-agenda-entry-text-mode))
;;     ("g" org-agenda-toggle-time-grid
;;      (format "% -3S" org-agenda-use-time-grid))
;;     ("D" org-agenda-toggle-diary
;;      (format "% -3S" org-agenda-include-diary))
;;     ("!" org-agenda-toggle-deadlines)
;;     ("["
;;      (let ((org-agenda-include-inactive-timestamps t))
;;        (org-agenda-check-type t 'timeline 'agenda)  (org-agenda-redo)))
;;     ("q" (message "Abort") :exit t))

  ;; todo
  (setq org-todo-keywords
 	'((sequence "TODO(t)" "WAITING(w@/!)" "LATER(l@)" "|" "DONE(d!/@)" "CANCELED(c@)")))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "light grey" :weight bold :background "red")
        	("LATER" :foreground "dark violet" :weight bold)
        	("DONE" :foreground "dark green" :weight bold)
        	("WAITING" :foreground "dark orange" :weight bold)
        	("LATER" :foreground "light orange" :weight bold))))
  (setq org-priority-faces
        (quote ((?A . (:background "blue" :foreground "yellow" :weight bold)))))

  ;; links
  (setq org-link-abbrev-alist '(("debian-bug" . "https://bugs.debian.org/%s")
                                ("debian-dp"  . "https://packages.debian.org/%s")
                                ("debian-dsp" . "https://tracker.debian.org/%s")
                                ("debian-dst" . "https://security-tracker.debian.org/tracker/%s")
                                ("salsa-dt-mr" . "https://salsa.debian.org/qa/distro-tracker/merge_requests/%")))

  ;; *** [[url][desc]] :tag1:tag2: -> url #tag1,tag2#
  (defun org-convert-entry-to-irc ()
    (interactive)
    (let* ((org-link (nth 4 (org-heading-components)))
	   (link (progn
		   (if (string-match org-link-bracket-re org-link)
		       (org-link-decode (match-string 1 org-link))
		     org-link)))
           (tags (delete "todo_gcu" (org-get-tags nil t)))
           (irctags (mapconcat 'identity tags ","))
           (delim "#"))
      (org-set-tags tags)
      (org-todo "")
      (message (concat link " " delim irctags delim))))

  ;; spelling
  (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
    "from http://emacs.stackexchange.com/questions/9333/how-does-one-use-flyspell-in-org-buffers-without-flyspell-triggering-on-tangled/9347"
    (let ((rlt ad-return-value)
          (begin-regexp "^\\*")
          (end-regexp "\n")
          old-flag
          b e)
      (when ad-return-value
        (save-excursion
          (setq old-flag case-fold-search)
          (setq case-fold-search t)
          (setq b (re-search-backward begin-regexp nil t))
          (if b (setq e (re-search-forward end-regexp nil t)))
          (setq case-fold-search old-flag))
        (if (and b e (< (point) e)) (setq rlt nil)))
      (setq ad-return-value rlt)))

  ;; archiving
  (setq org-auto-archive-required-days 21)
  (setq org-auto-archive-handler-function 'org-archive-subtree)

  (defun org-archive-region-default ()
    "Archive all entries in the selected region"
    (interactive)
    (save-excursion
      (let ((beg (if (org-region-active-p)
		     (region-beginning)
		   (point-min)))
	    (end (if (org-region-active-p)
		     (region-end)
		   (point-max))))
	(goto-char end)
	(outline-previous-heading)
	(while (>= (point) beg)
	  (org-archive-subtree-default)
	  (outline-previous-heading)))))

  (defun org-auto-archivable-p ()
    "Determines if the current heading is auto-archivable,
     meaning that it has been in a DONE state for more than
     org-auto-archive-required-days."
    (interactive)
    (save-excursion
      (goto-char (line-beginning-position))
      (let ((end-heading (save-excursion
			   (outline-next-heading)
			   (point)))
	    (state-regexp
	     (concat "CLOSED: \\[\\([^]\n]+\\)\\]")))
	(if (and (org-entry-is-done-p)
		 (re-search-forward state-regexp end-heading t))
	    (let* ((time-string (match-string 1))
		   (when-closed (apply #'encode-time
				       (org-parse-time-string time-string)))
		   (days-since-closed (time-to-number-of-days 
				       (time-subtract (current-time)
						      when-closed))))
	      (>= days-since-closed org-auto-archive-required-days))))))
    
  (defun org-my-archive-done ()
    "Archive the current heading and its subtree if it is
     auto-archivable."
    (if (org-auto-archivable-p)
	(funcall org-auto-archive-handler-function)))

  (defun org-my-archive-done-tasks ()
    "Archive all auto-archivable headings in the current region,
     or in the entire buffer if no region is active."
    (interactive)
    (save-excursion
      (let ((beg (if (org-region-active-p)
		     (region-beginning)
		   (point-min)))
	    (end (if (org-region-active-p)
		     (region-end)
		   (point-max))))
	(goto-char end)
	(outline-previous-heading)
	(while (and (>= (point) beg))
	  (org-my-archive-done)
	  (outline-previous-heading)))))
;;  (add-hook 'after-save-hook 'org-my-archive-done-tasks)

  ;; note/capture/refile
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-default-notes-file org-agenda-files)
  (setq seb/org-refile-files '())
  (setq org-refile-targets '((seb/org-refile-files . (:maxlevel . 2))))

  (setq org-capture-templates
        (quote (("h" "Home" entry (file+olp "~/org/home.todo" "Home" "Inbox")
                 "* TODO %?\n  DEADLINE: %t")
                ("l" "Link" entry (file+olp "~/org/links.org" "URLs" "Inbox")
                 "* %?\n  %U")
                ("L" "Link from FF" entry (file+olp "~/org/links.org" "URLs" "Inbox")
                 "* %a %?\n  %U")
                ("e" "Mail" entry (file+headline "~/org/home.todo" "Inbox")
                 "* TODO %? %U\n  Source: %u, %c\n  %i"))))

  ;; bindings
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c /") 'org-sparse-tree)
  (define-key org-mode-map (kbd "C-c C-x C-r") 'org-clock-report)
  (define-key org-mode-map (kbd "C-c SPC") 'nil)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c /") 'org-sparse-tree)

  (key-chord-define org-mode-map "uu" '(lambda ()
					 (interactive)
					 (let ((prefix-arg last-prefix-arg))
					   (org-ctrl-c-ctrl-c))))
  
;;   (defun hot-expand (str)
;;     "Expand org template."
;;     (insert str)
;;     (org-try-structure-completion))

;;   (defhydra hydra-org-template (:color blue :hint nil)
;;     "
;; _c_enter  _q_uote    _L_aTeX:
;; _l_atex   _e_xample  _i_ndex:
;; _a_scii   _v_erse    _I_NCLUDE:
;; _s_rc     ^ ^        _H_TML:
;; _h_tml    ^ ^        _A_SCII:
;; "
;;     ("s" (hot-expand "<s"))
;;     ("e" (hot-expand "<e"))
;;     ("q" (hot-expand "<q"))
;;     ("v" (hot-expand "<v"))
;;     ("c" (hot-expand "<c"))
;;     ("l" (hot-expand "<l"))
;;     ("h" (hot-expand "<h"))
;;     ("a" (hot-expand "<a"))
;;     ("L" (hot-expand "<L"))
;;     ("i" (hot-expand "<i"))
;;     ("I" (hot-expand "<I"))
;;     ("H" (hot-expand "<H"))
;;     ("A" (hot-expand "<A"))
;;     ("<" self-insert-command "ins")
;;     ("o" nil "quit"))

;;   (define-key org-mode-map "<"
;;     (lambda () (interactive)
;;       (if (looking-back "^")
;;           (hydra-org-template/body)
;;         (self-insert-command 1))))

  (local-set-key (kbd "<M-RET>") 'org-meta-return)

  (setq org-priority-faces '((65 :foreground "color-81" :weight bold)))

  (defun seb/org-previous-timestamp ()
    (save-excursion
      (org-previous-visible-heading 2)
      (forward-line 1)
      (back-to-indentation)
      (org-kill-line)
      (yank)
      (current-kill 0 t)))

  (defhydra hydra-org-timestamp (:color amaranth :hint nil)
    ("a" (let ((current-prefix-arg '(16)))
           (call-interactively 'org-time-stamp))
     "active")
    ("A" (insert (format-time-string "<%F %a>"))
     "active (date-only)")
    ("i" (let ((current-prefix-arg '(16)))
           (call-interactively 'org-time-stamp-inactive))
     "inactive")
    ("I" (insert (format-time-string "[%F %a]"))
     "inactive (date-only)")
    ("l" (insert (seb/org-previous-timestamp)) "insert last timestamp")
    ("L" (progn
	         (back-to-indentation)
	         (org-kill-line)
	         (insert (seb/org-previous-timestamp)) "set last timestamp"))
    ("b" (org-timestamp-change -1 'hour) "-1h")
    ("f" (org-timestamp-change 1 'hour) "+1h")
    ("p" (org-timestamp-change -1 'day) "-1d")
    ("n" (org-timestamp-change 1 'day) "+1d")
    ("q" nil "quit" :color blue))
  (key-chord-define org-mode-map "hh" 'hydra-org-timestamp/body))

(add-hook 'org-load-hook 'my-org-mode-hook)
(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-recentf-mode-hook ()
  (setq recentf-save-file (concat my-emacsd "recentf"))
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 60)
  (setq recentf-exclude '("/tmp/.*")))
(add-hook 'recentf-load-hook 'my-recentf-mode-hook)

(defun my-time-stamp-hook ()
  (time-stamp-format "%3a, %02d %3b %Y %02H:%02M:%02S %z"))
(add-hook 'time-stamp-hook 'my-time-stamp-hook)

(defun my-change-log-hook ()
  (setq left-margin 2))
(add-hook 'change-log-mode-hook 'my-change-log-hook)

(defun my-no-electric-indent-hook ()
  (local-set-key (kbd "C-j") 'newline)
  (local-set-key (kbd "C-m") 'newline))
(add-hook 'puppet-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'puppet-mode-hook 'my-no-electric-indent-hook)
(add-hook 'ruby-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'ruby-mode-hook 'my-no-electric-indent-hook)

;; _____________________________________________________________________
;; General preferences

;; No more "C-x C-s C-x #' (server-mode)
(define-key global-map (kbd "C-x j") '(lambda ()
                                        (interactive)
                                        (progn
                                          (save-buffer)
                                          (if (and (fboundp 'server-running-p)
                                                   (server-running-p))
                                              (server-edit)
                                            (kill-emacs)))))

;; ace-window/avy
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)
;;(csetq aw-flip-keys '("n" "ν"))
;;(add-to-list 'aw-dispatch-alist '(?ν aw-flip-window))

(avy-setup-default)
(setq avy-all-windows nil)
(setq avy-styles-alist '((avy-goto-char-2 . post)))
(define-key global-map (kbd "C-c SPC") 'avy-goto-line)
(define-key global-map (kbd "M-n") 'avy-goto-char-2)

;; saving history locally: available on every system, and still private as
;; it's not in the public git emacs configuration
;; (from https://github.com/baron42bba/.emacs.d/blob/master/bba.org#save-history)
(setq savehist-additional-variables '(kill-ring 
				      search-ring
				      regexp-search-ring
				      last-kbd-macro
				      kmacro-ring
				      shell-command-history))
(setq kmacro-ring-max 42)
(if (file-directory-p "~/org")
  (progn
    (setq history-delete-duplicates t)
    (setq savehist-file "~/org/emacs_history")
    (if (file-exists-p savehist-file)
	(load-file savehist-file))
    (savehist-mode 1)))

;; save files starting with #! as executable
;; (from https://github.com/baron42bba/.emacs.d/blob/master/bba.org#safe-hash-bang-files-executable)
(defun make-buffer-executable-if-hashbang ()
  (if (and (save-excursion
             (save-restriction
               (widen)
               (goto-char (point-min))
               (save-match-data
                 (looking-at "^#!"))))
           (not (file-executable-p buffer-file-name)))
      (progn
        (shell-command (concat "chmod ugo+x " buffer-file-name))
        (message (concat "Saved " buffer-file-name " with +x")))))
(add-hook 'after-save-hook 'make-buffer-executable-if-hashbang)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.2)
  (setq key-chord-two-keys-delay 0.1))

(use-package edit-server
  :defer 2
  :config
  (setq edit-server-new-frame nil)
  (if (and (fboundp 'server-running-p)
	   (server-running-p)
	   (string= server-name "main"))
	(edit-server-start)))

(use-package ledger-mode
  :defer t
  :mode "\\.ledger$"
  :config
  (setq ledger-use-iso-dates t))

(use-package flycheck
  :defer 2
  :init
  (global-flycheck-mode t)
  :config
  (setq flycheck-keymap-prefix (kbd "C-c ~"))
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*")))
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("<" flycheck-first-error                                       "First")
    (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil                                                       "quit"))
  :bind
  ("M-g f" . hydra-flycheck/body))

(use-package flycheck-color-mode-line
  :after flycheck
  :config
  (flycheck-color-mode-line-mode t))

(use-package magit
  :mode ("\\(svn-commit\\|COMMIT_EDITMSG\\|MERGE_MSG\\)" . git-commit-mode)
  :config
  (setq magit-commit-ask-to-stage "stage")
  (setq magit-diff-refine-hunk "all")
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup)
  :chords (("kk" . magit-status)
           ("KK" . magit-file-dispatch)))

(use-package forge
  :after magit)

(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package notmuch
  :config
    (setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
				   (:name "unread" :query "tag:unread" :key "u")
				   (:name "flagged" :query "tag:flagged" :key "F")
				   (:name "sent" :query "tag:sent" :key "s")
				   (:name "drafts" :query "tag:draft" :key "d")
				   (:name "all mail" :query "*" :key "a")))

    (setq notmuch-show-tag-macro-alist
      (list
       '("u" "-unread" "+test")
       ;; '("n" "+notmuch::patch" "+notmuch::needs-review" "-notmuch::pushed")
       ;; '("o" "+notmuch::patch" "+notmuch::obsolete"
       ;; 	     "-notmuch::needs-review" "-notmuch::moreinfo")
       ;; '("p" "-notmuch::pushed" "-notmuch::needs-review"
       ;; 	 "-notmuch::moreinfo" "+pending")
       ;; '("P" "-pending" "-notmuch::needs-review" "-notmuch::moreinfo" "+notmuch::pushed")
       ;; '("r" "-notmuch::patch" "+notmuch::review")
       ;; '("s" "+notmuch::patch" "-notmuch::obsolete" "-notmuch::needs-review" "-notmuch::moreinfo" "+notmuch::stale")
       ;; '("t" "+notmuch::patch" "-notmuch::needs-review" "+notmuch::trivial")
       '("w" "+notmuch::patch" "+notmuch::wip" "-notmuch::needs-review")))

    (defun notmuch-show-apply-tag-macro (key)
      (interactive "k")
      (let ((macro (assoc key notmuch-show-tag-macro-alist)))
	(apply 'notmuch-show-tag-message (cdr macro))
	(notmuch-refresh-this-buffer)))

    (defun notmuch-search-apply-tag-macro (key)
      (interactive "k")
      (let ((macro (assoc key notmuch-show-tag-macro-alist)))
	(apply 'notmuch-search-tag (list (cdr macro)))
	(notmuch-refresh-this-buffer)))

    (eval-after-load 'notmuch-show
      '(define-key notmuch-show-mode-map "`" 'notmuch-show-apply-tag-macro))

    (eval-after-load 'notmuch-search
      '(define-key notmuch-search-mode-map "`" 'notmuch-search-apply-tag-macro))
)

(use-package gnus
  :defer t
  :config
  (setq user-mail-address my-email)
  (setq user-full-name my-name)

  (setq gnus-select-method '(nntp "news.gmane.io"))
  (setq gnus-secondary-select-methods '((nntp "localhost" 8119)))

  (setq smtpmail-smtp-server "localhost")

  (setq gnus-thread-ignore-subject t)
  (setq gnus-thread-hide-subtree t)

  (setq gnus-startup-file (concat my-emacsd "/gnus/.newsrc"))
  (setq gnus-dribble-directory (concat my-emacsd "/gnus"))
  (setq gnus-always-read-dribble-file t)

  (setq gnus-summary-line-format "%U%R %z {%5L} [%-35,35n] %&user-date;   %t %B%-80,80S\n"
        gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-leaf-with-other "├► "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-leaf "╰► "
        gnus-sum-thread-tree-vertical "│")

  (gnus-add-configuration
   '(article
     (horizontal 1.0
           (vertical 50
         (group 1.0))
           (vertical 1.0
         (summary 0.40 point)
         (article 1.0)))))
  (gnus-add-configuration
   '(summary
     (horizontal 1.0
           (vertical 50
         (group 1.0))
           (vertical 1.0
         (summary 1.0 point)))))

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (setq ivy-height 20) ;; number result lines to display
  (setq ivy-initial-inputs-alist nil) ;; no anchor by default
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq split-height-threshold nil)
  (setq enable-recursive-minibuffers t)
  (ivy-mode t))

(use-package ivy-hydra
  :after ivy)

(use-package ivy-rich
  :after (:all ivy counsel)
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setq ivy-virtual-abbreviate 'full)
  :config
  (ivy-rich-mode t))

(use-package swiper
  :after ivy
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-r" . counsel-grep-or-swiper)))

(use-package counsel
  :after ivy
  :init
  (setq counsel-grep-base-command "grep -P -n -i -e %s %s")
  (setq counsel-git-grep-cmd-function #'counsel-git-grep-cmd-function-ignore-order)
  :config
  (counsel-mode t)
  (setq ivy-initial-inputs-alist nil)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c j" . counsel-git-grep)
	 ("C-x l" . counsel-locate)
	 ("M-y" . counsel-yank-pop)
	 :map help-map
	 ("f" . counsel-describe-function)
	 ("v" . counsel-describe-variable)
	 ("l" . counsel-find-library)
	 ("i" . counsel-info-lookup-symbol)
	 ("u" . counsel-unicode-char)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))

(use-package projectile
  :config
  (setq projectile-use-git-grep t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode t))

(use-package counsel-projectile
  :after (:all projectile counsel)
  :config
  (counsel-projectile-mode t)
  :bind (:map projectile-mode-map
	 ("C-c p f" . counsel-projectile-find-file)
	 ("C-c p p" . counsel-projectile-switch-project)))

(use-package yasnippet
  :config
  (yas-global-mode t)
  (loop for dir in (directory-files my-home t "\.config-.+$")
	do (setq yas-snippet-dirs (append yas-snippet-dirs (list (concat dir "/.emacs.d/snippets")))))
  (yas/reload-all)
  :chords (("yy" . yas-expand))
  :bind (("C-c y" . yas-expand)
	 ("C-c C-i" . yas-insert-snippet)
	 :map yas-minor-mode-map
         ([tab] . nil)
	 ("TAB" . nil)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package eglot
  :hook ((python-mode . eglot-ensure)
	 (python-mode . (lambda() (setq eglot-workspace-configuration '((:pyls . (:plugins (:jedi_completion (:include_params t))))))))
	 (go-mode . eglot-ensure)))

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a
  ;; parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes by switching
  ;; on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package go-mode)

(use-package clojure-mode)
(use-package cider)

(use-package elpy
  :defer t
  :config
  (flymake-mode nil)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (setq python-indent-offset 4)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  :mode "\\(\\.rb$\\|Capfile\\|Rakefile\\)")

(use-package puppet-mode
  :mode "\\.pp$")

(use-package company
  :config
  (setq company-begin-commands '(self-insert-command))
  (defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
	(company-complete-common)
      (indent-according-to-mode)))
  :bind (("C-c i" . complete-or-indent))
  :hook ((puppet-mode . company-mode)
	 (python-mode . company-mode)
	 (ruby-mode . company-mode)
	 (lisp-mode . company-mode)
	 (jsp-mode . company-mode)
	 (html-mode . company-mode)
	 (xml-mode . company-mode)
	 (sh-mode . company-mode)))

(use-package ace-window)
(use-package avy)
(use-package dash)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package epl)
(use-package hydra)
(use-package json-mode)
(use-package markdown-mode)
(use-package multiple-cursors)
(use-package org-super-agenda)
(use-package pkg-info)
(use-package puppet-mode)
(use-package smartparens)
(use-package wgrep)
(use-package yaml-mode)

;; flyspell
(use-package flyspell
  :bind (("C-c f" . (lambda() (interactive) (seb/flyspell/change-dict "francais")))
	 ("C-c e" . (lambda() (interactive) (seb/flyspell/change-dict "american")))
	 ("C-c ," . flyspell-goto-next-error))
  :hook ((flyspell-incorrect . seb/flyspell/ignore-uppercase)
	 (mail-mode . flyspell-mode)
	 (mail-mode . seb/flyspell/choose-dict-automatically))
  :mode ("DSA-" . flyspell-mode)
  :config
  (set-face-foreground 'flyspell-incorrect-face "yellow3")
  (setq flyspell-sort-corrections-function 'seb/flyspell/sort-corrections-function)
  (setq flyspell-sort-corrections t)
  (setq seb/flyspell/regular-letters
	(let ((l "abcdefghijklmnoprstuvwxyz"))
	  (split-string (concat l (upcase l)) "" t)))
  (defun seb/flyspell/ignore-uppercase (beg end &rest rest)
    (while (and (< beg end)
		(let ((c (char-after beg)))
		  (not (= c (downcase c)))))
      (setq beg (1+ beg)))
    (= beg end))
  (defun seb/flyspell/change-dict (dict)
    (interactive)
    (message (concat "Changing dict to: " dict))
    (ispell-change-dictionary dict)
    (flyspell-buffer))
  (defun seb/flyspell/is-buffer-french ()
    "Check if the buffer contains french text."
    (progn
      (goto-char (point-min))
      (save-excursion
	(re-search-forward " \\(je\\|tu\\|il\\|que\\|et\\|les?\\|des?\\) " nil t))))
  (defun seb/flyspell/guess-dict () 
    (seb/flyspell/change-dict (if (seb/flyspell/is-buffer-french) "francais"
				"american")))
  (defun seb/flyspell/choose-dict-automatically ()
    (if (zerop (buffer-size))
	(seb/flyspell/change-dict "francais") ;; default to french
      (seb/flyspell/guess-dict))) ;; if non-empty, try to identify the language...
  (defun seb/flyspell/same-class-p (c1 c2)
    "Non exhaustive function aiming at figuring out if
characters C1 and C2 belong to the same 'class'."
    (let ((a '("a" "â" "à" "á" "ä"))
	  (c '("c" "ç"))
	  (e '("e" "ê" "è" "é" "ë"))
	  (i '("i" "î" "ì" "í" "ï"))
	  (o '("o" "ô" "ò" "ó" "ö"))
	  (u '("u" "û" "ù" "ú" "ü")))
      (loop for tuple in (list a c e i o u) do
	    (if (member c1 tuple)
		(return (member c2 tuple))
	      (if (member c2 tuple)
		  (return (member c1 tuple)))))))
  (defun seb/flyspell/word-distance (word1 word2)
    "Difference in length between WORD1 and WORD2."
    (abs (- (length word1) (length word2))))
  (defun seb/flyspell/word-difference (word1 word2)
    "Different characters between WORD1 and WORD2."
    (if (or (= (length word1) 0) (= (length word2) 0))
	0
      (+ (let ((c1 (substring word1 0 1))
	       (c2 (substring word2 0 1)))
	   (if (string= c1 c2)
	       0
	     (if (seb/flyspell/same-class-p c1 c2) 0
	       1)))
	 (seb/flyspell/word-difference (substring word1 1) (substring word2 1)))))
  (defun seb/flyspell/accent-count (word)
    (let ((count 0))
      (dolist (x (split-string word "" t) count)
	(when (not (member x seb/flyspell/regular-letters))
	  (setq count (1+ count))))))
  (defun seb/flyspell/sort-corrections-function (word1 word2 word)
    "Sort WORD1 and WORD2 as corrections of WORD: favor the
     corrections having the same length as WORD, and use
     number of 'special' characters, then distance from the
     corrected word to the original, as additional criteria."
    (let ((distance1 (seb/flyspell/word-distance word1 word))
	  (distance2 (seb/flyspell/word-distance word2 word)))
      (if (= distance1 distance2)
	  (let ((dif1 (seb/flyspell/word-difference word1 word))
		(dif2 (seb/flyspell/word-difference word2 word)))
	    (if (= dif1 dif2)
		(let ((accents-count1 (seb/flyspell/accent-count word1))
		      (accents-count2 (seb/flyspell/accent-count word2)))
		  (>= accents-count1 accents-count2))
	      (< dif1 dif2)))
	(< distance1 distance2))))
  (flyspell-mode t))

;;;; hydras

;; utilities to resize windows
;; inspired from http://www.emacswiki.org/emacs/WindowResize
(defun win-position ()
  "Return a tuple description the window position; the first element is the
vertical position ('t', 'b' or 'm'), the second one is the horizontal
position ('l', 'r', 'm')"
    (let* ((fr-width (frame-width))
           (fr-height (frame-height))
           (win-edges (window-edges))
           (win-x-min (nth 0 win-edges))
           (win-y-min (nth 1 win-edges))
           (win-x-max (nth 2 win-edges))
           (win-y-max (nth 3 win-edges)))
      (list
       (cond
        ((eq 0 win-y-min) "t")
        ((eq (- fr-height 1) win-y-max) "b")
        (t "m"))
       (cond
        ((eq 0 win-x-min) "l")
        ((eq fr-width win-x-max) "r")
        (t "m")))))

(defun win-shift-vertical (arg)
  (interactive)
  (let ((pos (nth 0 (win-position))))
    (enlarge-window
     (cond
      ((equal "t" pos) arg)
      ((equal "b" pos) (- 0 arg))
      ((equal "m" pos) arg)))))

(defun win-shift-horizontal (arg)
  (interactive)
  (let ((pos (nth 1 (win-position))))
    (enlarge-window-horizontally
     (cond
      ((equal "l" pos) arg)
      ((equal "r" pos) (- 0 arg))
      ((equal "m" pos) arg)))))

;; window-switching hydra
(global-set-key
 (kbd "M-o")
 (defhydra hydra-window (:color amaranth)
   "window"
   ("b" windmove-left)
   ("n" windmove-down)
   ("p" windmove-up)
   ("f" windmove-right)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
        "vert")
   ("h" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
        "horz")
   ;; ("t" transpose-frame "'")
   ("o" delete-other-windows "one" :color blue)
   ("d" delete-window "del")
   ("P" (win-shift-vertical -1) "↑")
   ("N" (win-shift-vertical 1) "↓")
   ("F" (win-shift-horizontal 1) "→")
   ("B" (win-shift-horizontal -1) "←")
   ;; ("a" ace-window "ace")
   ;; ("s" ace-swap-window "swap")
   ;; ("d" ace-delete-window "del")
   ;; ("i" ace-maximize-window "ace-one" :color blue)
   ;; ("b" ido-switch-buffer "buf")
   ;; ("m" headlong-bookmark-jump "bmk")
   ("q" nil "cancel")
   ("k" nil "cancel")))

(global-set-key
 (kbd "M-g j")
 (defhydra hydra-goto (:color blue :hint nil)
  "
Goto:
^Char/Line^         ^Word^                ^org^                    ^search^
^^^^^^^^--------------------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: ivy-occur
_C_: char           _W_: some word        ^ ^: FIXME               _s_: search forward
_L_: char in line   _,_: subword by char  ^ ^                      _r_: search backward
_l_: avy-goto-line  _._: some subword     ^ ^                      _S_: search forward regex
^ ^                 ^ ^                   ^ ^                      _R_: search backward regex
-----------------------------------------------------------------------------------------------
_g_: counsel-git    _j_: counsel-git-grep
_i_: ace-window
_n_: Navigate           _;_: mark position _/_: jump to mark
"
  ("c" avy-goto-char-2)
  ("C" avy-goto-char)
  ("L" avy-goto-char-in-line)
  ("l" avy-goto-line)

  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("," avy-goto-subword-1)
  ("." avy-goto-subword-0)

  ("h" org-goto)

  ("o" ivy-occur)
  ("s" isearch-forward)
  ("r" isearch-backward)
  ("S" isearch-forward-regexp)
  ("R" isearch-backward-regexp)

  ("g" counsel-git)
  ("j" counsel-git-grep)

  ("i" ace-window)

  ("n" hydra-navigate/body)
  (";" org-mark-ring-push :color red)
  ("/" org-mark-ring-goto :color blue)))
(key-chord-define-global "jj" 'hydra-goto/body)

(global-set-key
 (kbd "M-g y")
 (defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")))
(global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

;; (global-set-key
;;  (kbd "C-n")
;;  (defhydra hydra-move
;;    (:body-pre (next-line))
;;    "move"
;;    ("n" next-line)
;;    ("p" previous-line)
;;    ("f" forward-char)
;;    ("b" backward-char)
;;    ("a" beginning-of-line)
;;    ("e" move-end-of-line)
;;    ("v" scroll-up-command)
;;    ;; Converting M-v to V here by analogy.
;;    ("V" scroll-down-command)
;;    ("l" recenter-top-bottom)
;;    ("q" nil "cancel" :color blue)))

(global-set-key
 (kbd "M-g t")
 (defhydra hydra-transpose (:color red)
   "Transpose"
   ("c" transpose-chars "characters")
   ("w" transpose-words "words")
   ("o" org-transpose-words "Org mode words")
   ("l" transpose-lines "lines")
   ("s" transpose-sentences "sentences")
   ("e" org-transpose-elements "Org mode elements")
   ("p" transpose-paragraphs "paragraphs")
   ("t" org-table-transpose-table-at-point "Org mode table")
   ("q" nil "cancel" :color blue)))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
"
  ^_p_^     _d_elete    _s_tring
_b_   _f_   _o_k        _y_ank
  ^_n_^     _c_opy      _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _k_ill
"
  ("b" backward-char nil)
  ("f" forward-char nil)
  ("p" previous-line nil)
  ("n" next-line nil)
  ("e" exchange-point-and-mark nil)
  ("c" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("k" kill-rectangle nil)
  ("o" nil nil)
  ("q" nil "cancel" :color blue))
(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))
(key-chord-define-global "MM" 'hydra-multiple-cursors/body)

(defhydra hydra-lisp-eval (:exit t :columns 2)
  "Lisp eval"
  ("r" eval-region "Region")
  ("b" eval-buffer "Buffer")
  ("e" eval-expression "S-expression")
  ("l" eval-last-sexp "Last s-expression")
  ("L" eval-last-sexp-print-value "Last s-expression and print value")
  ("d" eval-defun "Defun / Function")
  ("f" eval-defun "Defun / Function"))
(key-chord-define lisp-mode-map "LL" 'hydra-lisp-eval/body)

(key-chord-define-global "CC" 'calc)
(key-chord-define-global "WW" 'browse-url-at-point)

;; numbering
(line-number-mode t)
(column-number-mode t)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; narrowing
(put 'narrow-to-region 'disabled nil)

;; font highlighting
(require 'color-theme)
(require 'color-theme-seb)
(color-theme-initialize)
;; (require 'color-theme-solarized)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(if (eq window-system nil)
    (color-theme-console-seb)
  ;;      (color-theme-solarized-dark)
  (color-theme-gnome2))

;; paren matching
(show-paren-mode t)
;; (require 'smartparens-config)
;; (smartparens-global-mode t)

;; disable cursor blinking
(setq visible-cursor nil)

;; region highlighting
(transient-mark-mode t)

;; auto-revert
(global-auto-revert-mode t)
(defalias 'auto-revert-handler 'my-auto-revert-handler)
(setq global-auto-revert-mode-text " ARev")
(setq dired-auto-revert-buffer t)

;; mode-line
(defvar my-mode-line-coding-format
  '(:eval
    (let* ((code (symbol-name buffer-file-coding-system))
           (eol-type (coding-system-eol-type buffer-file-coding-system))
           (eol (cond ((eq 0 eol-type) "UNIX")
                      ((eq 1 eol-type) "DOS")
                      ((eq 2 eol-type) "MAC")
                      (t "???"))))
      (concat code ":" eol " "))))
(put 'my-mode-line-coding-format 'risky-local-variable t)
(setq-default mode-line-format (substitute
                                'my-mode-line-coding-format
                                'mode-line-mule-info
                                mode-line-format))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; no toolbar
;; no electric-indent-mode
(if (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; recentf
(unless (eq (user-uid) 0) ;; not when sudoed
  (require 'recentf)
  (recentf-mode t))

;; cvelist
(autoload 'debian-cvelist-mode "debian-cvelist.el" "Major mode for debian CVE lists" t)

;; no annoying automatic py-help-at-point
(global-eldoc-mode -1)

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; various variables
(setq tramp-mode nil)
(setq indent-tabs-mode nil)
(setq browse-url-browser-function 'my-browse-url-tab)
(setq case-fold-search t)
(setq echo-keystrokes 0.1)
(setq ids-creator-id "seb")
(setq inhibit-startup-message t)
(setq line-move-visual nil) 
(menu-bar-mode -1)
(setq perl-indent-level 2)
(setq lua-indent-level 2)
(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq standard-indent 2)
(setq js-indent-level 2)
(setq tab-width 2)
(setq vc-follow-symlinks t)
(setq visible-bell t)

;; impartiality is key here
(loop for holiday in (list 'holiday-bahai-holidays
                           'holiday-christian-holidays
                           'holiday-general-holidays
                           'holiday-hebrew-holidays
                           'holiday-islamic-holidays
                           'holiday-local-holidays
                           'holiday-oriental-holidays
                           'holiday-other-holidays
                           'holiday-solar-holidays)
      do (set holiday nil))

;; grep-find & friends
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "dist")
     (add-to-list 'grep-find-ignored-directories "staging")
     (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> {} +")))
;(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; key mappings for predefined functions
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-x /") 'revert-buffer)
(global-set-key (kbd "C-c a") 'align)
(global-set-key (kbd "M-k") 'kill-whole-line)
(global-set-key (kbd "M-B") (make-interactive-fun 'forward-whitespace -1))
(global-set-key (kbd "M-F") 'forward-whitespace)

;; associate file patterns and modes
(setq auto-mode-alist 
      (append '(("\\.texi$" 		      	      . texi-outline)
                ("\\.emacs" 		      	      . lisp-mode)
                ("Dockerfile.*"                       . dockerfile-mode)
                ("docker-compose.*"                   . docker-compose-mode)
                ("pf\\.conf" 		      	      . pf-mode)
                ("\\.md" 		      	      . markdown-mode)
                ("\\.properties$" 		      . conf-mode)
                ("rules" 		      	      . makefile-mode)
                ("diff$" 		      	      . diff-mode)
                ("/\.mutt" 		      	      . muttrc-mode)
                ("/\.md$" 		      	      . markdown-mode)
                ("\\.vcl$" 		      	      . vcl-mode)
                ("/tmp/mutt"                         . seb/mutt-hook)
                ("^\\(.*/\\.followup\\|\\.article\\)" . seb/mutt-hook)
                ("\.json"                             . json-mode)
                ("\.jsx"                              . js-mode)
                ("\.xml"                              . html-mode)
                ("CVE/list$"                          . debian-cvelist-mode)
                ("\\.\\(org\\|todo\\|csv\\)$"         . org-mode)
                ("\\.z" 		      	      . sh-mode))
              auto-mode-alist))


;; FIXME: ???
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun jsp-mode ()
  (interactive)
  (multi-mode 1 'html-mode '("<%" jde-mode) '("%>" html-mode)))

;; _____________________________________________________________________
;; Custom-set
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.21")
 '(jde-ant-enable-find t)
 '(jde-global-classpath
   '("." "/usr/share/java/jde.jar" "/opt/tomcat/common/lib/servlet.jar" "/usr/share/java/junit.jar"))
 '(jde-jdk-registry '(("1.5.0_10" . "/usr/lib/jvm/java-1.5.0-sun")))
 '(json-reformat:indent-width 2)
 '(load-home-init-file t t)
 '(org-export-exclude-tags '("noexport" "archive"))
 '(org-export-html-use-infojs 'when-configured)
 '(org-super-agenda-header-separator "")
 '(org-super-agenda-mode t)
 '(org-super-agenda-separator "")
 '(package-selected-packages
   '(tree-sitter-langs wgrep htmlize dpkg-dev-el dh-elpa devscripts yasnippet-snippets use-package-chords smartparens puppet-mode ox-pandoc org-roam org-ql multiple-cursors ledger-mode json-mode jinja2-mode ivy-rich ivy-hydra go-mode gitignore-mode gitconfig-mode forge flycheck-color-mode-line elpy eglot edit-server dockerfile-mode docker-compose-mode counsel-projectile cider browse-kill-ring ace-window ace-jump-mode))
 '(puppet-indent-level 2)
 '(safe-local-variable-values
   '((buffer-file-coding-system-explicit iso-8859-15-dos . iso-8859-15-dos)
     (buffer-file-coding-system-explicit utf-8-dos . utf-8-dos)
     (buffer-file-coding-system-explicit . utf-8-dos))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "color-21" :foreground "white"))))
 '(hydra-face-blue ((t (:foreground "color-33" :weight bold))))
 '(hydra-face-red ((t (:foreground "color-136" :weight bold))))
 '(ledger-occur-xact-face ((t (:background "color-235"))))
 '(org-agenda-date ((t (:foreground "color-117" :weight bold :inverse-video t))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :foreground "color-228" :inverse-video t :underline t :slant italic :weight bold))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
 '(org-agenda-structure ((t (:foreground "color-122" :inverse-video t))))
 '(org-archived ((((class color) (min-colors 8) (background dark)) (:foreground "color-22"))))
 '(org-checkbox-statistics-todo ((t (:foreground "color-177"))))
 '(org-date ((((class color) (background dark)) (:foreground "Cyan"))))
 '(org-hide ((t (:foreground "#00000000"))))
 '(org-level-1 ((t (:foreground "color-33" :weight bold))))
 '(org-level-2 ((t (:foreground "white" :weight bold))))
 '(org-level-3 ((t (:foreground "yellow" :weight bold))))
 '(org-level-4 ((t (:foreground "color-42" :weight bold))))
 '(org-link ((((class color) (background dark)) (:foreground "color-69" :underline t))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :foreground "color-45" :weight bold))))
 '(org-tag ((t (:foreground "color-208" :underline nil :weight bold))))
 '(org-warning ((t (:inherit font-lock-warning-face :foreground "color-250"))))
 '(show-paren-match ((t (:background "color-236")))))
