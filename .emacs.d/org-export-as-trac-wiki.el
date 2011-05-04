(require 'org)

(defun org-export-as-trac-wiki (arg)
  "Export the outline as a trac wiki format.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3."
  (interactive "P")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
                                        (org-infile-export-plist)))
         (region
          (buffer-substring
           (if (org-region-active-p) (region-beginning) (point-min))
           (if (org-region-active-p) (region-end) (point-max))))
         (custom-times org-display-custom-times)
         (org-ascii-current-indentation '(0 . 0))
         (level 0) line txt
         (umax nil)
         (umax-toc nil)
         (case-fold-search nil)
         (filename (concat (file-name-as-directory
                            (org-export-directory :ascii opt-plist))
                           (file-name-sans-extension
                            (file-name-nondirectory buffer-file-name))
                           ".trac-wiki.txt"))
         (buffer (find-file-noselect filename))
         (org-levels-open (make-vector org-level-max nil))
         (odd org-odd-levels-only)
         (date  (format-time-string "%Y/%m/%d" (current-time)))
         (time  (format-time-string "%X" (org-current-time)))
         (author      (plist-get opt-plist :author))
         (title       (or (plist-get opt-plist :title)
                          (file-name-sans-extension
                           (file-name-nondirectory buffer-file-name))))
         (email       (plist-get opt-plist :email))
         (language    (plist-get opt-plist :language))
         (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
         (infixed     nil)
         (text        nil)
         (todo nil)
         (lang-words nil))

    (setq org-last-level 1)
    (org-init-section-numbers)

    (find-file-noselect filename)

    (setq lang-words (or (assoc language org-export-language-setup)
                         (assoc "en" org-export-language-setup)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (fundamental-mode)
    ;; create local variables for all options, to make sure all called
    ;; functions get the correct information
    (mapcar (lambda (x)
              (set (make-local-variable (cdr x))
                   (plist-get opt-plist (car x))))
            org-export-plist-vars)
    (org-set-local 'org-odd-levels-only odd)
    (setq umax (if arg (prefix-numeric-value arg)
                 org-export-headline-levels))
    (setq umax-toc (if (integerp org-export-with-toc)
                       (min org-export-with-toc umax)
                     umax))

    ;; File header
    (if title (insert "= "  title " ="))
    (insert "\n")
    (if (or author email)
        (insert " " (concat (nth 1 lang-words) "::\n    " (or author "")
                            (if email (concat " <" email ">") "")
                            "\n")))
    (if (and date time)
        (insert " " (concat (nth 2 lang-words) "::\n    " date " " time "\n")))
    (if text (insert (concat (org-html-expand-for-ascii text) "\n\n")))

    (insert "\n\n")

    (org-init-section-numbers)
    (while (setq line (pop lines))
      ;; Remove the quoted HTML tags.
      (setq line (org-html-expand-for-ascii line))
      ;; Remove targets
      (while (string-match "<<<?[^<>]*>>>?[ \t]*\n?" line)
        (setq line (replace-match "" t t line)))
      ;; Replace internal links
      (while (string-match org-bracket-link-regexp line)
        (setq line (replace-match
                    (if (match-end 3) "[\\1 \\3]" "[\\1]")
                    t nil line)))
      (when custom-times
        (setq line (org-translate-time line)))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]*\\(.*\\)" line)
        ;; a Headline
        (setq level (org-tr-level (- (match-end 1) (match-beginning 1)))
              txt (match-string 2 line))
        (org-trac-wiki-level-start level txt umax lines))
       ((string-match "^\\([-+]+\\)[ \t]*\\(.*\\)" line)
        ;; a list item
        (setq level (org-tr-level (- (match-end 1) (match-beginning 1)))
              txt (match-string 2 line))
        (insert (make-string (* 2 (1- level)) ?\ ) " * " txt "\n"))
       ((string-match "^[ \t]*:\\(.*\\)" line)
        ;; infixed 
        (when (not infixed)
          (insert "{{{\n")
          (setq infixed t))
        (insert (org-html-protect (match-string 1 line)) "\n")
        (when (and lines
                   (not (string-match "^[ \t]*:\\(.*\\)"
                                      (car lines))))
          (insert "}}}\n")
          (setq infixed nil)))
       (t
        (insert (org-fix-indentation line org-ascii-current-indentation) "\n"))))
    (normal-mode)
    (save-buffer)
    ;; remove display and invisible chars
    (let (beg end)
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'display))
        (setq end (next-single-property-change beg 'display))
        (delete-region beg end)
        (goto-char beg)
        (insert "=>"))
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'org-cwidth))
        (setq end (next-single-property-change beg 'org-cwidth))
        (delete-region beg end)
        (goto-char beg)))
    (goto-char (point-min))))

(defun org-trac-wiki-level-start (level title umax &optional lines)
  "Insert a new level in trac-wiki export."
  (let (char (n (- level umax 1)) (ind 0))
    (if (> level umax)
        (progn
          (insert (make-string (* 2 n) ?\ )
                  (char-to-string ?*)
                  " " title "\n")
          ;; find the indentation of the next non-empty line
          (catch 'stop
            (while lines
              (if (string-match "^\\*" (car lines)) (throw 'stop nil))
              (if (string-match "^\\([ \t]*\\)\\S-" (car lines))
                  (throw 'stop (setq ind (org-get-indentation (car lines)))))
              (pop lines)))
          (setq org-ascii-current-indentation (cons (* 2 (1+ n)) ind)))
      (if (or (not (equal (char-before) ?\n))
              (not (equal (char-before (1- (point))) ?\n)))
          (insert "\n"))

      (unless org-export-with-tags
        (if (string-match "[ \t]+\\(:[a-zA-Z0-9_@:]+:\\)[ \t]*$" title)
            (setq title (replace-match "" t t title))))

      (insert (make-string level ?=) " " title " " (make-string level ?=) "\n")
      (setq org-ascii-current-indentation '(0 . 0)))))

(provide 'org-export-as-trac-wiki)
