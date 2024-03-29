;;; cvelist --- Major mode for Debian's CVE list
;;;
;;; Commentary:
;;;   only useful for security-tracker-team/security-tracker.git's data/CVE/list
;;;
;;; Code:
;;;   Guido Günther
;;;   Moritz Muehlenhoff
;;;   Sébastien Delafond
;;;
;;; Can be enabled via:
;;;
;;; (autoload 'debian-cvelist-mode "cvelist.el"
;;;     "Major mode for debian CVE lists" t)
;;; (setq auto-mode-alist
;;;     (cons '("list" . debian-cvelist-mode) auto-mode-alist))

(defun debian-cvelist-insert-not-for-us ()
  "Insert NOT-FOR-US keyword."
  (interactive)
  (insert "\tNOT-FOR-US: "))

(defun debian-cvelist-insert-note ()
  "Insert NOTE comment."
  (interactive)
  (insert "\tNOTE: "))

(defun debian-cvelist-cvesearch ()
  "Look up a CVE ID at the MITRE website."
  (interactive)
  (browse-url (concat "https://cve.mitre.org/cgi-bin/cvename.cgi?name=" (thing-at-point 'symbol))))

(defvar debian-cvelist-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "C-c C-f") 'debian-cvelist-insert-not-for-us)
     (define-key map (kbd "C-c C-n") 'debian-cvelist-insert-note)
     (define-key map (kbd "C-c C-c") 'debian-cvelist-cvesearch)
     map)
   "Keymap for `debian-cvelist-mode'.")

(defvar debian-cvelist-font-lock-keywords
  '(("^CVE-[0-9]\\{4\\}-[0-9X]\\{4,7\\}"
     (0 font-lock-function-name-face) ;; face for CVE keyword
     ("(\\(.+\\))$" nil nil (1 font-lock-warning-face))) ;; face for the rest of the line
    ("D[LS]A-[0-9]\\{4,5\\}-[0-9]" . font-lock-function-name-face)
    ("#[0-9]\\{1,7\\}" . font-lock-type-face)
    ("^\tNOTE:" . font-lock-comment-delimiter-face)
    ("^\tTODO:" . font-lock-warning-face)
    ("^\t\\(RESERVED\\|NOT-FOR-US\\|REJECTED\\)" . font-lock-keyword-face)
    ("\\<unfixed\\|undetermined\\>" . font-lock-warning-face)
    ("\\<end-of-life\\|not-affected\\|no-dsa\\|ignored\\|postponed\\>" . font-lock-constant-face))
  "Keyword highlighting for `debian-cvelist-mode'.")

(defun debian-cvelist-is-cve ()
  "Checks if a current line is a CVE description."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*CVE-")))

(defun debian-cvelist-indent-line ()
  "Indent current line as debian CVE list."
  (beginning-of-line)
  (if (debian-cvelist-is-cve)
      (indent-line-to 0)
    (indent-line-to 8)))

(define-derived-mode debian-cvelist-mode fundamental-mode "debian-cvelist"
  "A major mode for editing data/CVE/list in the Debian
   secure-tracker repository."
  (setq-local font-lock-defaults '(debian-cvelist-font-lock-keywords t))
  (setq indent-line-function 'debian-cvelist-indent-line))

(provide 'debian-cvelist)
;;; cvelist.el ends here
