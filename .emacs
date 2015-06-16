(require 'cl) ;; (loop for ...)
(require 'dired-x)

;;(setq debug-on-error t)

;; _____________________________________________________________________
;; custom path
(setq my-home (expand-file-name (concat "~" (or (getenv "SUDO_USER") (getenv "USER")))))
(setq my-emacsd (concat my-home "/.emacs.d/"))
(loop for file in (directory-files my-emacsd nil "run.*")
      do (load-file (concat my-emacsd file)))
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

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))
(global-set-key "\C-ci" 'complete-or-indent)

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
  (insert (concat "Sébastien Delafond <" my-email ">")))

(defun insert-sig ()
  (interactive)
  (insert (concat "Bien cordialement," "\n\n" "--SD")))
(global-set-key "\C-cs" 'insert-sig)

(defun system-short-name ()
  (car (split-string system-name "\\.")))

(defun ts ()
  (interactive)
  (shell-command "date -R" t))

(defun my-browse-url-tab (url &optional new-window)
  "Open URL"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((cmd "~/bin/browser-maybe-selection.rb"))
    (start-process (concat cmd url) "*Messages*" cmd "default" url)))

(defun paste-and-shift (arg)
  (interactive)
  (let ((begin (point)))
    (insert arg)
    (indent-rigidly begin (point) 2)))

(defun kill-to-eof ()
  (interactive)
  (kill-region (point) (point-max)))
(global-set-key "\C-ck" 'kill-to-eof)

(defun show-file-name ()
  "Show the full path file name in the minibuffer"
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))
(global-set-key "\C-cz" 'show-file-name)

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
(my-autoload "id" "ace-jump-mode" "align" "company-mode" "git-commit-mode" "gitignore-mode"
             "gitconfig-mode" "helm" "projectile" "helm-projectile" "python-mode" "multi-mode" "org"
             "time-stamp" "pf-mode" "ruby-mode" "ruby-electric" "gtags"
             "outdent" "vcl-mode")

(defun add-function-to-hooks (fun modes-hooks)
  "Add a call to FUN to each mode-hook listed in MODES-HOOKS."
  (loop for mode-hook in modes-hooks do
	(add-hook mode-hook fun)))
(add-function-to-hooks (make-fun 'set-fill-column 78) '(c-mode-hook lisp-mode-hook
                                                        emacs-lisp-mode-hook
                                                        html-mode-hook))
(add-function-to-hooks (make-fun 'set-fill-column 72) '(text-mode-hook))

(defun load-gnu-global ()
  "Load GNU Global if available."
  (if (fboundp 'gtags-mode)
      (gtags-mode t)))
(add-function-to-hooks 'load-gnu-global '(python-mode-hook java-mode-hook
                                          shell-mode-hook
                                          c-mode-hook lisp-mode-hook
                                          emacs-lisp-mode-hook))

;; ELPA
(if (>= emacs-major-version 24) 
    (require 'package)
  (load-file (concat my-emacsd "/23/package.el")))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(let ((package-list '(ace-window
                      ;; anything
                      ;; anything-git-files
                      ;; anything-ipython
                      ;; anything-show-completion
                      async
                      avy
                      clojure-mode
                      company
                      dash
                      epl
                      git-commit-mode
                      git-gutter
                      gitconfig-mode
                      gitignore-mode
                      ;; google-maps
                      helm
                      helm-projectile
                      hydra
                      ;; ipython
                      pkg-info
                      projectile
                      ;; python-mode
                      smartparens
                      ;; vcl-mode
		      )))
  (package-initialize)
  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))
  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

 

;; _____________________________________________________________________
;; Hooks
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
  (require 'ob-js)
  (require 'ob-ditaa)
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "ditaa")))  ; don't ask for ditaa
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (require 'ob-sh)
  (require 'ob-sql)
  (require 'ob-emacs-lisp)
  (setq org-src-fontify-natively t)

  (require 'org-protocol)

;;  (require 'ox-confluence)
  (require 'ox-beamer)
  (require 'ox-md)

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
  (setq org-clock-clocktable-default-properties '(:maxlevel 5))

  ;; agenda
  (setq org-agenda-include-diary nil)
  (setq org-agenda-span 7)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-combined-agenda-icalendar-file "~/org/org.ics")
  (setq org-icalendar-store-UID t)
  (setq org-icalendar-use-deadline '(event-if-todo))
;; (setq org-icalendar-include-todo t)
  (setq org-deadline-warning-days 0)
  (setq org-default-priority 67)
;; (setq org-fast-tag-selection-single-key 'expert)
  (setq org-fast-tag-selection-single-key t)
  (setq org-return-follows-link t)
  (setq org-hide-leading-stars t)
  (setq org-id-locations-file (concat my-emacsd "org-id-locations"))
  (setq org-log-done 'time)
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

  ;; todo
  (setq org-todo-keywords
 	'((sequence "TODO(t)" "WAITING(w@/!)" "LATER(l@)" "|" "DONE(d!/@)" "CANCELED(c@)")))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "light grey" :weight bold :background "red")
        	("LATER" :foreground "dark violet" :weight bold)
        	("DONE" :foreground "dark green" :weight bold)
        	("WAITING" :foreground "dark orange" :weight bold)
        	("LATER" :foreground "light orange" :weight bold))))

  ;; links
  (setq org-link-abbrev-alist '(("debian-bug" . "http://bugs.debian.org/%s")))

  ;; *** entry :tag1:tag2: -> link #tag1,tag2#
  (defun org-convert-entry-to-irc ()
    (interactive)
    (let ((heading (org-element-property :raw-value
                                         (save-excursion
                                           (org-back-to-heading)
                                           (org-element-at-point))))
          (tags (mapconcat 'identity (org-get-tags) ","))
          (delim "#"))
    (message (concat heading " " delim tags delim))))

  ;; spelling
  (defun org-mode-flyspell-verify ()
    "Don't let flyspell put overlays at active buttons, or on
todo/all-time/additional-option-like keywords."
    (let ((pos (max (1- (point)) (point-min)))
          (word (thing-at-point 'word)))
      (and (not (get-text-property pos 'keymap))
           (not (get-text-property pos 'org-no-flyspell))
           (not (member word org-todo-keywords-1))
           (not (member word org-all-time-keywords)))))

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
  (setq org-default-notes-file (concat org-directory "~/org/home.todo"))
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-capture-templates
        (quote (("h" "Home" entry (file+olp "~/org/home.todo" "Home" "Inbox")
                 "* TODO %?\n  DEADLINE: %t")
                ("l" "Link" entry (file+olp "~/org/home.todo" "URLs" "Inbox")
                 "* %?\n  %U")
                ("m" "Mail" entry (file+headline "~/org/home.todo" "Inbox")
                 "* TODO %? %U\n  Source: %u, %c\n  %i"))))

  ;; bindings
  (define-key org-mode-map "\C-ca" 'org-agenda)
  (define-key org-mode-map "\C-cl" 'org-store-link)
  (define-key org-mode-map "\C-c/" 'org-sparse-tree)
  (define-key org-mode-map "\C-c " 'nil)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-c/" 'org-sparse-tree)
  
  (defun hot-expand (str)
    "Expand org template."
    (insert str)
    (org-try-structure-completion))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("e" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (looking-back "^")
          (hydra-org-template/body)
        (self-insert-command 1)))))  

(add-hook 'org-load-hook 'my-org-mode-hook)
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; flyspell
(eval-after-load "flyspell" ;; yeah, we wish there was a flyspell-hook...
  '(progn
     (defun my-flyspell-ignore-uppercase (beg end &rest rest)
       (while (and (< beg end)
                   (let ((c (char-after beg)))
                     (not (= c (downcase c)))))
         (setq beg (1+ beg)))
       (= beg end))
     (add-hook 'flyspell-incorrect-hook 'my-flyspell-ignore-uppercase)

     (defun change-dict (dict)
       (interactive)
       (message (concat "Changing dict to: " dict))
       (ispell-change-dictionary dict)
       (flyspell-buffer))
     (defun is-buffer-french ()
       "Check if the buffer contains french text."
       (progn
         (goto-char (point-min))
         (save-excursion
           (re-search-forward " \\(je\\|tu\\|il\\|que\\|et\\|les?\\|des?\\) " nil t))))
     (defun guess-dict () 
       (if (is-buffer-french) (change-dict "francais")
	 (change-dict "american")))
     (defun choose-dict-automatically ()
       (if (zerop (buffer-size))
           (change-dict "francais") ;; default to french
	 (guess-dict))) ;; if non-empty, try to identify the language...
     (set-face-foreground 'flyspell-incorrect-face "yellow3")

     (setq my-flyspell-regular-letters
           (let ((l "abcdefghijklmnoprstuvwxyz"))
             (split-string (concat l (upcase l)) "" t)))

     (defun flyspell-same-class-p (c1 c2)
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

     (defun flyspell-word-distance (word1 word2)
       "Difference in length between WORD1 and WORD2."
       (abs (- (length word1) (length word2))))

     (defun flyspell-word-difference (word1 word2)
       "Different characters between WORD1 and WORD2."
       (if (or (= (length word1) 0) (= (length word2) 0))
           0
         (+ (let ((c1 (substring word1 0 1))
                  (c2 (substring word2 0 1)))
              (if (string= c1 c2)
                  0
                (if (flyspell-same-class-p c1 c2) 0
                  1)))
            (flyspell-word-difference (substring word1 1) (substring word2 1)))))

     (defun flyspell-accent-count (word)
       (let ((count 0))
         (dolist (x (split-string word "" t) count)
           (when (not (member x my-flyspell-regular-letters))
             (setq count (1+ count))))))

     (defun my-flyspell-sort-corrections-function (word1 word2 word)
       "Sort WORD1 and WORD2 as corrections of WORD: favor the
        corrections having the same length as WORD, and use
        number of 'special' characters, then distance from the
        corrected word to the original, as additional criteria."
       (let ((distance1 (flyspell-word-distance word1 word))
             (distance2 (flyspell-word-distance word2 word)))
         (if (= distance1 distance2)
             (let ((dif1 (flyspell-word-difference word1 word))
                   (dif2 (flyspell-word-difference word2 word)))
               (if (= dif1 dif2)
                   (let ((accents-count1 (flyspell-accent-count word1))
                         (accents-count2 (flyspell-accent-count word2)))
                     (>= accents-count1 accents-count2))
                 (< dif1 dif2)))
           (< distance1 distance2))))

     (setq flyspell-sort-corrections-function 'my-flyspell-sort-corrections-function)
     (setq flyspell-sort-corrections t)
     (global-set-key "\C-cf" (make-interactive-fun 'change-dict "francais"))
     (global-set-key "\C-ce" (make-interactive-fun 'change-dict "american"))
     (global-set-key "\C-c," 'flyspell-goto-next-error)))

(defun my-recentf-mode-hook ()
  (setq recentf-save-file (concat my-emacsd "recentf"))
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 60)
  (setq recentf-exclude '("/tmp/.*")))
(add-hook 'recentf-load-hook 'my-recentf-mode-hook)

(defun my-align-hook ()
  (setq align-to-tab-stop nil)
  (setq align-c++-modes '(c++-mode c-mode java-mode jde-mode python-mode ruby-mode)))
(add-hook 'align-load-hook 'my-align-hook)

(defun my-time-stamp-hook ()
  (time-stamp-format "%3a, %02d %3b %Y %02H:%02M:%02S %z"))
(add-hook 'time-stamp-hook 'my-time-stamp-hook)

(defun my-change-log-hook ()
  (setq left-margin 2))
(add-hook 'change-log-mode-hook 'my-change-log-hook)

(defun my-ruby-mode-hook ()
  (local-set-key "\r" 'newline-and-indent)
  (outline-minor-mode)
  (define-key outline-minor-mode-map "\C-c\C-e" 'outline-toggle-children)
  (define-key outline-minor-mode-map "\C-c\C-a" 'hide-body)
  (setq outline-regexp " *\\(def \\|class\\|module\\|#.*Main\\)"))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(defun my-python-mode-hook ()
  (local-set-key "\r" 'newline)
  (require 'ipython)
  (require 'anything)
  (require 'anything-ipython)
  (when (require 'anything-show-completion nil t)
    (use-anything-show-completion 'anything-ipython-complete
                                  '(length initial-pattern)))
  (define-key py-mode-map (kbd "M-TAB") 'anything-ipython-complete)
  ;; (define-key py-mode-map (kbd "M-BACKTAB") 'anything-ipython-import-modules-from-buffer)
  ;; (require 'pymacs)
  ;; (pymacs-load "ropemacs" "rope-")
  ;; (ropemacs-mode)
  ;; (define-key ropemacs-local-keymap "\C-cd" 'rope-show-doc)
  ;; (define-key ropemacs-local-keymap "M-\?" 'rope-code-assist)
  (require 'outdent)
  (outdent-mode t)
  (turn-off-auto-fill)
  ;; (defun py-outline-level ()
  ;;   (let (buffer-invisibility-spec)
  ;;     (save-excursion
  ;;       (skip-chars-forward "[:space:]")
  ;;       (current-column))))
  ;; (setq outline-regexp "[^ \t]\\|[ \t]*\\(def\\|class\\) ")
  ;; (setq outline-level 'py-outline-level)
  ;; (outline-minor-mode)
  ;; (define-key outline-minor-mode-map "\C-c\C-e" 'outline-toggle-children)
  ;; (define-key outline-minor-mode-map "\C-c\C-a" 'hide-body)
)
(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-jde-mode-hook ()
  (c-add-style "my-java" '("java" (c-basic-offset . 4)))
  (c-set-style "my-java")
  (jde-compile-option-classpath nil)
  (jde-compile-option-verbose nil)
;;  (put 'jde-global-classpath 'customized-value '("." "/usr/share/java/jde.jar" "/opt/tomcat/common/lib/servlet.jar" "/usr/share/java/junit.jar"))
;;  (jde-jdk-registry (quote (("1.4.2" . "/usr/local/j2sdk1.4.2_01"))))
)
(add-hook 'jde-mode-hook 'my-jde-mode-hook)
;;(setq global-senator-minor-mode t) ; fix from Debian's BTS

(defun my-puppet-mode-hook ()
  ((define-key map "\C-j" 'newline))
  ((define-key map "\C-m" 'newline)))
(add-hook 'puppet-mode-hook 'my-puppet-mode-hook)

;; _____________________________________________________________________
;; General preferences

;; No more "C-x C-s C-x #'
(define-key global-map "\C-xj" '(lambda ()
                                  (interactive)
                                  (save-buffer)
                                  (if (and (fboundp 'server-running-p)
                                           (server-running-p))
                                      (server-edit)
                                  (kill-emacs))))

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

;; helm
(setq
 ;; open helm buffer in another window
 helm-split-window-default-side 'other
 ;; do not occupy whole other window
 helm-split-window-in-side-p t)

(helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

(require 'helm-projectile)
(defun helm-projectile-switch-buffer ()
  "Use Helm instead of ido to switch buffer in projectile."
  (interactive)
  (helm :sources helm-source-projectile-buffers-list
        :buffer "*helm projectile buffers*"
        :prompt (projectile-prepend-project-name "Switch to buffer: ")))

;; projectile
(require 'projectile)
(projectile-global-mode)
;; Override some projectile keymaps
(eval-after-load 'projectile
  '(progn
     (define-key projectile-command-map (kbd "b") 'helm-projectile-switch-buffer)
     (define-key projectile-command-map (kbd "f") 'helm-projectile)
     (define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)))

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
 (kbd "C-M-o")
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
   ("q" nil "cancel")))

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

;; region highlighting
(transient-mark-mode t)

 ;; always "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; auto-revert
(global-auto-revert-mode t)
(defalias 'auto-revert-handler 'my-auto-revert-handler)
(setq global-auto-revert-mode-text " ARev")

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

;; accents  
(unless (or (featurep 'xemacs)
	    (>= emacs-major-version 22))
  (setq current-language-environment "Latin-1")
  (set-terminal-coding-system 'latin-1)
  (standard-display-european 1)
  (set-input-mode (car (current-input-mode))
		  (nth 1 (current-input-mode))
		  0))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; no toolbar
;; no electric-indent-mode
(if (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; recentf
(unless (eq (user-uid) 0) ;; not when sudoed
  (require 'recentf)
  (recentf-mode t))

;; git-gutter
;;(require 'git-gutter)
(when (fboundp 'global-git-gutter-mode)
  (global-git-gutter-mode t)
  (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
  (setq git-gutter:modified-sign "⚡")
  (set-face-foreground 'git-gutter:modified "cyan")
  (setq git-gutter:separator-sign "|"))

;; git-rebase
(defun my-git-rebase-mode-hook ()
  (read-only-mode))
(add-hook 'git-rebase-mode-hook 'my-git-rebase-mode-hook)

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; various variables
(setq company-begin-commands '(self-insert-command))
(setq tramp-mode nil)
(setq-default indent-tabs-mode nil)
(setq browse-url-browser-function 'my-browse-url-tab)
(setq case-fold-search t)
(setq ids-creator-id "seb")
(setq inhibit-startup-message t)
(setq line-move-visual nil) 
(menu-bar-mode -1)
(setq py-indent-offset 2)
(setq python-indent 2)
(setq perl-indent-level 2)
(setq lua-indent-level 2)
(setq ruby-indent-level 2)
(setq sh-basic-offset 2)
(setq standard-indent 2)
(setq tab-width 4)
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
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-x/" 'revert-buffer)
(global-set-key "\C-ca" 'align)

;; associate file patterns and modes
(setq auto-mode-alist 
      (append '(("\\.texi$" 		      	      . texi-outline)
		("\\.[jp]y$" 		      	      . python-mode)
		("\\.emacs" 		      	      . lisp-mode)
		("\\.rb$" 		      	      . (lambda () (progn
                                                                     (ruby-mode)
                                                                     (company-mode))))
		("Capfile" 		      	      . (lambda () (progn
                                                                     (ruby-mode)
                                                                     (company-mode))))
		("Rakefile" 		      	      . (lambda () (progn
                                                                     (ruby-mode)
                                                                     (company-mode))))
		("\\.pp$" 		      	      . (lambda () (progn
                                                                     (puppet-mode)
                                                                     (company-mode))))
		("pf\\.conf" 		      	      . pf-mode)
		("\\.properties$" 		      . conf-mode)
		("rules" 		      	      . makefile-mode)
		("diff$" 		      	      . diff-mode)
		("/\.mutt/" 		      	      . muttrc-mode)
		("\\.vcl$" 		      	      . vcl-mode)
		("\\.\\(el|clj\\)$"                   . (lambda () (progn
                                                                     (lisp-mode)
                                                                     (company-mode))))
		("^/tmp/mutt"                         . my-mutt-hook)
		("^\\(.*/\\.followup\\|\\.article\\)" . flyspell-mode)
		("\.jsx"                              . js-mode)
		("\\(svn-commit\\|COMMIT_EDITMSG\\|MERGE_MSG\\)"  . (lambda () (progn
                                                                     (git-commit-mode)
                                                                     (flyspell-mode)
                                                                     (remove-hook 'kill-buffer-query-functions
                                                                                  'git-commit-kill-buffer-noop t))))
		("\\.org$"                            . (lambda () (progn
                                                                     (org-mode)
                                                                     ;(flyspell-mode)
                                                                     )))
		("\\.\\(todo\\|csv\\)$"               . org-mode)
		("\\.jsp$" 		      	      . (lambda () (progn
                                                                     (jsp-mode)
                                                                     (company-mode))))
		("\\.html$" 		      	      . (lambda () (progn
                                                                     (html-mode)
                                                                     (company-mode))))
		("\\.xml$" 		      	      . (lambda () (progn
                                                                     (xml-mode)
                                                                     (company-mode))))
		("\\.z" 		      	      . (lambda () (progn
                                                                     (sh-mode)
                                                                     (company-mode)))))
              auto-mode-alist))


;; FIXME: ???
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun jsp-mode ()
  (interactive)
  (multi-mode 1 'html-mode '("<%" jde-mode) '("%>" html-mode)))

(defun my-mutt-hook ()
  (progn 
    (mail-mode)
    (flyspell-mode)
    (choose-dict-automatically)
    (local-set-key "\C-ci" 'format-email-body)))

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
   (quote
    ("." "/usr/share/java/jde.jar" "/opt/tomcat/common/lib/servlet.jar" "/usr/share/java/junit.jar")))
 '(jde-jdk-registry (quote (("1.5.0_10" . "/usr/lib/jvm/java-1.5.0-sun"))))
 '(load-home-init-file t t)
 '(org-export-exclude-tags (quote ("noexport" "archive")))
 '(org-export-html-use-infojs (quote when-configured))
 '(puppet-indent-level 4)
 '(safe-local-variable-values
   (quote
    ((buffer-file-coding-system-explicit iso-8859-15-dos . iso-8859-15-dos)
     (buffer-file-coding-system-explicit utf-8-dos . utf-8-dos)
     (buffer-file-coding-system-explicit . utf-8-dos)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "color-21" :foreground "white"))))
 '(helm-ff-symlink ((t (:foreground "color-33"))))
 '(helm-match ((t (:foreground "color-83"))))
 '(helm-selection ((t (:background "color-239" :distant-foreground "black"))))
 '(hydra-face-blue ((t (:foreground "color-33" :weight bold))))
 '(hydra-face-red ((t (:foreground "color-136" :weight bold))))
 '(org-archived ((((class color) (min-colors 8) (background dark)) (:foreground "color-22"))))
 '(org-checkbox-statistics-todo ((t (:foreground "color-177"))) t)
 '(org-date ((((class color) (background dark)) (:foreground "Cyan"))))
 '(org-hide ((t (:foreground "#00000000"))))
 '(org-level-1 ((t (:foreground "color-33" :weight bold))))
 '(org-level-2 ((t (:foreground "white" :weight bold))))
 '(org-level-3 ((t (:foreground "yellow" :weight bold))))
 '(org-level-4 ((t (:foreground "color-42" :weight bold))))
 '(org-link ((((class color) (background dark)) (:foreground "color-69" :underline t))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :foreground "color-45" :weight bold))))
 '(org-tag ((t (:foreground "color-208" :underline nil :weight bold))))
 '(org-warning ((t (:inherit font-lock-warning-face :foreground "color-250")))))
