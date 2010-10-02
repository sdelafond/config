(define-structure restore-workspace-focus
    (export)

    (open user
          rep
          rep.regexp
          rep.system
          sawfish.wm
          sawfish.wm.util.display-window
          sawfish.wm.util.window-order
          sawfish.wm.util.selection
          sawfish.wm.workspace)

  (define *avoid-focus* "(syslog|gnome-panel|desktop_window|gkrellm)")

  (defun undim-in-focus-hook (win focus-mode) 
    "Makes the window that went out-of-focus, dim" 
    (let ((w (window-frame-id win)))
      (if *in-focus-trans*
	  (set-x-property w '_NET_WM_WINDOW_OPACITY (make-vector 1 *in-focus-trans*) 'CARDINAL 32)
	(delete-x-property w '_NET_WM_WINDOW_OPACITY)))
    (display-message "undimmed")
    (sync-server))

  (defun restore-focus ()
    "Restore focus to most recently used window except dock windows and desktop"
    (interactive)
    (let ((all-wins (window-order current-workspace))
          (tmp nil)
          (win nil)
          (name nil)
          (wins '()))
      (while all-wins
        (setq win (car all-wins)
              tmp (aref (get-x-text-property win 'WM_CLASS) 0)
	      name (car (reverse (get-x-property win 'WM_NAME))))
        (if (and (window-visible-p win)
                 (and (not (string-match *avoid-focus* tmp)) (not (string-match *avoid-focus* name))))
	    (when tmp
	      (activate-window tmp)
	      (raise-window tmp)
	      (undim-in-focus-hook tmp nil)))))
    (setq all-wins (cdr all-wins)))

  (add-hook 'enter-workspace-hook restore-focus)
  (add-hook 'destroy-notify-hook restore-focus)
  (add-hook 'iconify-window-hook restore-focus)

  )
