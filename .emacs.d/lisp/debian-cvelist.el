;; Major mode for Debian's CVE list
;; currently only does some syntax highlighting
;;
;; Can be enabled via
;;
;; (autoload 'debian-cvelist-mode "cvelist.el"
;;     "Major mode for debian CVE lists" t)
;; (setq auto-mode-alist
;;     (cons '("list" . debian-cvelist-mode) auto-mode-alist))

(defun debian-cvelist-insert-not-for-us ()
  "Insert NOT-FOR-US keyword"
  (interactive)
  (insert "\tNOT-FOR-US: "))

(defun debian-cvelist-insert-note ()
  "Insert NOTE comment"
  (interactive)
  (insert "\tNOTE: "))

(defvar debian-cvelist-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "C-c C-f") 'debian-cvelist-insert-not-for-us)
     (define-key map (kbd "C-c C-n") 'debian-cvelist-insert-note)
     map)
   "Keymap for `debian-cvelist-mode'.")

(defvar debian-cvelist-font-lock-keywords
  '(("^CVE-[0-9]\\{4\\}-[0-9X]\\{4\\}" . font-lock-function-name-face)
    ("^\tNOTE:" . font-lock-comment-delimiter-face)
    ("^\tTODO:" . font-lock-warning-face)
    ("^\t\\(RESERVED\\|NOT-FOR-US\\|REJECTED\\)" . font-lock-keyword-face)
    ("^CVE-[0-9]\\{4\\}-[0-9X]\\{4\\}" "\\[\\(.*\\)\\]$" nil nil (1 font-lock-variable-name-face))
    ("\\<unfixed\\|undetermined\\>" . font-lock-warning-face)
    ("\\<end-of-life\\|not-affected\\|no-dsa\\>" . font-lock-constant-face))
  "Keyword highlighting for `debian-cvelist-mode'")

(defun debian-cvelist-is-cve ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*CVE-")))

(defun debian-cvelist-indent-line ()
  "Indent current line as debian CVE list"
  (beginning-of-line)
  (if (debian-cvelist-is-cve)
      (indent-line-to 0)
    (indent-line-to 8)))

(define-derived-mode debian-cvelist-mode fundamental-mode "debian-cvelist"
  "A major mode for editing data/CVE/list in the Debian secure-testing repo."
  (setq-local font-lock-defaults '(debian-cvelist-font-lock-keywords nil))
  (setq indent-line-function 'debian-cvelist-indent-line))

(provide 'debian-cvelist)
