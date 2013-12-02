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
(setq load-path (cons my-emacsd load-path))
(setq load-path (cons "/usr/share/org-mode/lisp" load-path))

;; (setq load-path (cons "/usr/share/org-mode/lisp" load-path))
;; (setq my-icicled (concat my-emacsd "icicles"))
;; (setq load-path (cons my-icicled load-path))

;; _____________________________________________________________________
;; macros
(defmacro make-interactive-fun (fn args)
  `(lambda () (interactive) (funcall ,fn ,args)))

(defmacro make-fun (fn args)
  `(lambda () (funcall ,fn ,args)))

;; _____________________________________________________________________
;; functions
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
	     (string-match "passwd" filename)
	     (string-match "/tmp/dpep" filename)))))
(setq backup-enable-predicate 'my-backup-enable-predicate)

(defun my-autoload (&rest modes)
  "Autoload each mode listed in MODES."
  (loop for mode in modes do (autoload (intern mode) mode nil t)))
(my-autoload "id" "align" "company-mode" "git-commit-mode" "gitignore-mode"
             "gitconfig-mode" "python-mode" "multi-mode" "org"
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
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; _____________________________________________________________________
;; Hooks
(defun my-message-mode-hook ()
  (setq message-beginning-of-line nil)
  (turn-off-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-hook)

(defun my-org-mode-hook ()
;;   (require 'org-expiry)
;;   (org-expiry-insinuate)
;;   (setq org-expiry-handler-function 'org-expiry-archive-subtree)

;;   (require 'org-crypt)
;;   (org-crypt-use-before-save-magic)
;;   (setq org-crypt-key "sdelafond@gmx.net")
;;   (add-hook 'before-save-hook 'org-encrypt-entries)

;;  (require 'org-babel-init)
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

 ;; (require 'org-export)
 (require 'ox-confluence)
 (require 'ox-beamer)
;;  (require 'org-e-html)

  ;; LaTeX
  (require 'org-latex)
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

;;   (org-babel-load-library-of-babel)

  ;; agenda
  (setq org-agenda-files (directory-files "~/org" t "^[^.].*\\.todo$"))
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
  (setq org-completion-use-ido t)
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

  ;; tags
  (setq org-tag-alist '((:startgroup . nil) ("@work" . ?w)
			                    ("@home" . ?h)
			                    ("@tel" . ?t)
			                    ("@buy" . ?b)
			(:endgroup . nil)
			(:startgroup . nil) ("laptop" . ?l)
			                    ("pc" . ?p)
			(:startgroup . nil) ("jardin" . ?j)
			                    ("maison" . ?m)
			(:endgroup . nil)
			(:endgroup . nil)
			(:startgroup . nil) ("net" . ?n)
			(:endgroup . nil)))

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
  (setq org-link-abbrev-alist
        '(("debian-bug"   . "http://bugs.debian.org/%s")))

  ;; spelling
  (defun org-mode-flyspell-verify ()
    "Don't let flyspell put overlays at active buttons, or on
todo/all-time/additional-option-like keywords."
    (let ((pos (max (1- (point)) (point-min)))
          (word (thing-at-point 'word)))
      (and (not (get-text-property pos 'keymap))
           (not (get-text-property pos 'org-no-flyspell))
           (not (member word org-todo-keywords-1))
           (not (member word org-all-time-keywords))
           (not (member word org-additional-option-like-keywords)))))

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

;; publishing
  (setq org-publish-project-alist
	'(("orgfiles"
	   :base-directory "~/.config-mappy/org"
	   :base-extension "org"
	   :publishing-directory "~/public_html/"
	   :publishing-function org-publish-org-to-html
	   :exclude "(travel-list|home)\.org" ;; regexp
	   :headline-levels 1
	   :section-numbers nil
	   :table-of-contents 
;;	   :style "<link rel=stylesheet
;;			 href=\"../other/mystyle.css\" type=\"text/css\">"
	   :auto-preamble t
	   :auto-postamble nil)
          ("meh"
	   :base-directory "~/org"
	   :base-extension "org"
	   :publishing-directory "~/public_html/"
	   :publishing-function org-publish-org-to-html
	   :exclude "(travel-list|home)\.org" ;; regexp
	   :headline-levels 1
	   :section-numbers nil
	   :table-of-contents 
;;	   :style "<link rel=stylesheet
;;			 href=\"../other/mystyle.css\" type=\"text/css\">"
	   :auto-preamble t
	   :auto-postamble nil)
	  ("other"
	   :base-directory "~"
	   :base-extension "js"
;;	   :exclude ".*"
;;	   :include "org-info\.js"
	   :publishing-directory "~/public_html/"
	   :publishing-function org-publish-attachment)
	  ("org" :components '("orgfiles" "meh" "other"))))

  ;; note/capture/refile
  (setq org-default-notes-file (concat org-directory "~/org/home.todo"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-capture-templates
        (quote
          (("h" "Home" entry (file+olp "~/org/home.todo" "Home" "Inbox") "* TODO %?\n  DEADLINE: %t")
           ("l" "Link" entry (file+olp "~/org/home.todo" "URLs" "Inbox") "* %?\n  %U")
           ("m" "Mail" entry (file+headline "~/org/home.todo" "Inbox") "* TODO %? %U\n  Source: %u, %c\n  %i"))))

  ;; bindings
  (define-key org-mode-map "\C-ca" 'org-agenda)
  (define-key org-mode-map "\C-cl" 'org-store-link)
  (define-key org-mode-map "\C-c/" 'org-sparse-tree)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-c/" 'org-sparse-tree))

(add-hook 'org-load-hook 'my-org-mode-hook)
(add-hook 'org-mode-hook 'my-org-mode-hook)

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
  (setq recentf-exclude '("/tmp/.*"))
  (run-with-timer 60 60 t 'recentf-save-list)
  (defun ido-from-recentf ()
    (interactive)
    (find-file
     (ido-completing-read "Recentf open: "
			  (mapcar (lambda (path)
				    (expand-file-name path))
				  recentf-list)
			  nil t)))
  (global-set-key "\C-xr\C-f" 
		  (if (fboundp 'icy-mode)
		      'icicle-recent-file
		    'ido-from-recentf)))
(add-hook 'recentf-load-hook 'my-recentf-mode-hook)

(defun my-ido-mode-hook ()
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-show-dot-for-dired t)
  (setq ido-ignore-buffers '("^ " "^\\*.*"))
  (setq ido-confirm-unique-completion nil)
  (setq read-buffer-function 'ido-read-buffer)
  (setq ido-default-buffer-method 'samewindow)
;;      (setq ido-use-filename-at-point t)
  (icomplete-mode 99))
(add-hook 'ido-setup-hook 'my-ido-mode-hook)

(defun my-iswitchb-mode-hook ()
  (setq iswitchb-case t)
  (setq iswitchb-buffer-ignore '("^ " "^\\*.*"))
  (add-hook 'iswitchb-define-mode-map-hook
	    '(lambda ()
	       (define-key iswitchb-mode-map "\C-a" 'iswitchb-toggle-ignore))))
(add-hook 'iswitchb-mode-hook 'my-iswitchb-mode-hook)

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

;; _____________________________________________________________________
;; General preferences

;; No more "C-x C-s C-x #'
(define-key global-map "\C-xj" '(lambda ()
                                  (interactive)
                                  (save-buffer)
                                  (server-edit)))

;; numbering
(line-number-mode t)
(column-number-mode t)

;; narrowing
(put 'narrow-to-region 'disabled nil)

;; font highlighting
(require 'color-theme-seb)
;; (require 'color-theme-solarized)

;; paren matching
(show-paren-mode t)
;; (require 'smartparens-config)
;; (smartparens-global-mode t)

;; region highlighting
(transient-mark-mode t)

;; font-locking
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(defun terminal-init-screen-256color ()
  "Terminal initialization function for screen."
  ;; use the xterm color initialization code.
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

  ;;; For GNU Emacs 21, use our own xterm-256color.el
(if (= 21 emacs-major-version) (load "xterm-256color"))

(if (eq window-system nil)
    (color-theme-console-seb)
  ;;      (color-theme-solarized-dark)
  (color-theme-x-seb))

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

;; recentf
(unless (eq (user-uid) 0) ;; not when sudoed
  (require 'recentf)
  (recentf-mode t))

;; git-gutter
;;(require 'git-gutter)
(global-git-gutter-mode t)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
(setq git-gutter:modified-sign "⚡")
(set-face-foreground 'git-gutter:modified "cyan")
(setq git-gutter:separator-sign "|")

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; buffer switching and more
(if (require 'icicles nil t)
    (progn
      ;; custom-based values don't seem to take effect,
      ;; so we define those here
;;      (setq icicle-apropos-complete-keys '())
;;      (setq icicle-apropos-prefix-keys '())
;;      (setq icicle-apropos-complete-keys '([tab] [(control ?i)]))
      (local-unset-key "\C-c/")
      (setq icicle-menu-items-to-history-flag nil)
      (setq icicle-apropos-complete-keys '([S-tab] [S-iso-lefttab] [backtab]))
      (setq icicle-buffer-no-match-regexp "\\*")
      (global-set-key [backtab] 'icicle-apropos-complete) ;; huh, shouldn't the above do that ?
      (icy-mode)
      ;; not sure why it doesn't work by default in a tty, but the following does
      ;; the trick
      (global-set-key "\C-cK" 'icicle-complete-keys)
      )
  (if (boundp 'ido-mode)
      (ido-mode t)
    (iswitchb-mode t)))

;; EasyPG
;; (setenv "GPG_AGENT_INFO" nil)
(require 'epa-file)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epg-gpg-program "/usr/bin/gpg")
(epa-file-enable)

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
; FIXME: template ?
(setq grep-find-command "find . -type d -name '.svn' -prune -or -name '.git' -prune -or -type d -name 'dist' -prune -or -type d -name staging -prune -or -type f -name '*~' -prune -or -type f -not -name 'semantic.cache' -print0 | xargs -0 grep -I -n -P ")
(setq grep-find-template "find <D> -type d -name 'dist' -prune -or -type d -name staging -prune -or -type f -name '*~' -prune -or -type f -not -name 'semantic.cache' -print0 | xargs -0 grep -I -n -P <R>")

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
		("/\.mutt/" 		      	      . muttrc-mode)
		("\\.vcl$" 		      	      . vcl-mode)
		("\\.clj$" 		      	      . (lambda () (progn
                                                                     (lisp-mode)
                                                                     (company-mode))))
		("^/tmp/mutt"                         . my-mutt-hook)
		("^\\(.*/\\.followup\\|\\.article\\)" . flyspell-mode)
		("\\(svn-commit\\|COMMIT_EDITMSG\\)"  . (lambda () (progn
                                                                     (git-commit-mode)
                                                                     (flyspell-mode))))
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

;; scrollwheel
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

;; FIXME: ???
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; (defun my-nxml-mode-hook ()
;;   (nxml-delimiter-data-face ((nil (:foreground "LightGreen"))))
;;   (nxml-delimiter-face ((t (:foreground "LightSalmon2" :weight bold))))
;;   (nxml-name-face ((nil (:foreground "SkyBlue" :weight bold))))
;;   (nxml-ref-face ((nil (:foreground "LavenderBlush")))))
;; (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

;; _____________________________________________________________________
;; custom modes
;; (defvar server-seb-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-xk"
;;       '(lambda ()
;; 	 (interactive)
;; 	 (server-edit)))
;;     map))
;; (define-minor-mode server-seb-mode "Server")
;; (add-hook 'server-visit-hook 'server-seb-mode)

(defun jsp-mode ()
  (interactive)
  (multi-mode 1 'html-mode '("<%" jde-mode) '("%>" html-mode)))

(defun my-mutt-hook ()
  (cl-flet ((make-html-mail ()
                            (shell-command-on-region (point-min) (point-max)
                                                     "python ~/bin/make-html-mail.py" t t))
            (is-buffer-already-htmlized ()
                                        "Check if the buffer has already been HTMLized"
                                        (goto-char (point-min))
                                        (re-search-forward "src=.*/c/image" nil t))
            (is-buffer-to-htmlize ()
                                  "Check if the buffer is a raw email needing HTMLization."
                                  (goto-char (point-min))
                                  (re-search-forward "^From: " nil t)
                                  (and (re-search-forward "^From: " nil t) (re-search-forward "+sig+" nil t)))
            (htmlize-and-exit ()
                              (make-html-mail)
                              (save-buffer)
                              (server-edit)))
    (if (is-buffer-already-htmlized) (server-edit))
    (if (is-buffer-to-htmlize) (htmlize-and-exit) 
      (progn 
        ;; (message-mode)
        (flyspell-mode)
        (choose-dict-automatically)
        (local-set-key "\C-ci" 'format-email-body)))))

;; _____________________________________________________________________
;; Custom-set
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.21")
 '(jde-ant-enable-find t)
 '(jde-global-classpath (quote ("." "/usr/share/java/jde.jar" "/opt/tomcat/common/lib/servlet.jar" "/usr/share/java/junit.jar")))
 '(jde-jdk-registry (quote (("1.5.0_10" . "/usr/lib/jvm/java-1.5.0-sun"))))
 '(load-home-init-file t t)
 '(org-agenda-files (quote ("/home/seb/org/home.todo" "/home/seb/org/move.todo")))
 '(org-export-exclude-tags (quote ("noexport" "archive")))
 '(org-export-html-use-infojs (quote when-configured))
 '(safe-local-variable-values (quote ((buffer-file-coding-system-explicit iso-8859-15-dos . iso-8859-15-dos) (buffer-file-coding-system-explicit utf-8-dos . utf-8-dos) (buffer-file-coding-system-explicit . utf-8-dos)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-archived ((((class color) (min-colors 8) (background dark)) (:foreground "color-22"))))
 '(org-checkbox-statistics-todo ((t (:foreground "color-177"))) t)
 '(org-date ((((class color) (background dark)) (:foreground "Cyan"))))
 '(org-hide ((t (:foreground "#00000000"))))
 '(org-level-1 ((t (:foreground "color-27" :weight bold))))
 '(org-level-2 ((t (:foreground "white" :weight bold))))
 '(org-level-3 ((t (:foreground "yellow" :weight bold))))
 '(org-level-4 ((t (:foreground "color-36" :weight bold))))
 '(org-link ((((class color) (background dark)) (:foreground "color-69" :underline t))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :foreground "color-45" :weight bold))))
 '(org-tag ((t (:foreground "color-208" :underline nil :weight bold))))
 '(org-warning ((t (:inherit font-lock-warning-face :foreground "color-250")))))

(put 'downcase-region 'disabled nil)
