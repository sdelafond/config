;; $Id: id.el,v 1.2 2008-01-31 19:23:15 seb Exp $

;(add-hook 'write-file-hooks 'update-ids)

(defun insert-id ()
  "Inserts an ID like $I d: filename.c,v 0.03 1999/07/31 00:45:19 dmorris Exp
$. Uses the creator id in \"ids-creator-id\", the current buffer name, and
the current time/date."
  (interactive "*")
  (progn
    (insert "$Id: ")
    ; fix this part to get rid of c:\ crap ;
    (insert (if (null buffer-file-name) "unknown" (car (reverse
(split-string buffer-file-name "/")))))
    (insert ",v 1.00")
    (insert (format-time-string " %Y/%m/%d %T " (current-time)))
    (insert ids-creator-id)
    (insert " Exp $")
;;    (query-replace-regexp "$Id: id.el,v 1.2 2008-01-31 19:23:15 seb Exp $Id: " nil)
))

(defun insert-id-c()
  "Inserts an ID like $I d: filename.c,v 0.03 1999/07/31 00:45:19 dmorris Exp
$. Uses the creator id in \"ids-creator-id\", the current buffer name, and
the current time/date."
  (interactive "*")
  (progn
    (insert "/* ")
    (insert-id)
    (insert " */")
))

(defvar ids-creator-id "anonymous"
  "*Customizable creator id for update-ids hook. (\\[insert-id])")

(defun update-ids ()
  "Find any $I d: filename.c,v 0.03 1999/07/31 00:45:19 tom7 Exp $ strings
and update them.\n\nThe date and time are set current, the creator id by
the \"ids-creator-id\" user variable."
  (save-excursion
    (save-restriction
      (save-match-data
(widen)
(goto-char (point-min))
(while (search-forward-regexp
(concat "$I" "d: [A-Za-z0-9_.-]+,v \\([0-9]+\\.?[0-9]*\\)") nil t)
  (let* ((start (match-beginning 1))
(old-vers (split-string (match-string-no-properties 1)
"\\."))
(major-vers (if (null old-vers) 1 (string-to-number (car
old-vers))))
(minor-vers (if (null (cdr old-vers)) 0 (string-to-number
(cadr old-vers)))))
;;     (message old-vers)
    (search-forward " Exp $")
    (delete-region start (- (point) 6))
    (goto-char start)
    (insert (format "%d.%02d" major-vers (+ 1 minor-vers)))
    (insert (format-time-string " %Y/%m/%d %T " (current-time)))
    (insert ids-creator-id))))))
  nil)


(defvar date-format "%-e %b %Y" "*Customizable format for insert-datestamp
(\\[insert-datestamp]).")
(defvar time-format "%H:%M:%S" "*Customizable format for insert-timestamp.
(\\[insert-timestamp]).")

(defun insert-timestamp ()
  "Inserts the current time into the buffer using the value of
time-format."
  (interactive "*")
  (insert (format-time-string time-format (current-time))))

(defun insert-datestamp ()
  "Inserts the current date into the buffer using the value of
date-format."
  (interactive "*")
  (insert (format-time-string date-format (current-time))))

(provide 'id)
