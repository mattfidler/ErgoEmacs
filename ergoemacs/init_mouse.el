;-*- coding: utf-8 -*-

;; for Windows and Mac, set Middle-click to do nothing (it can be customizable by the user)
(when (or (string-equal system-type "windows-nt")
          (string-equal system-type "darwin"))
  (global-set-key [mouse-2] nil)
  )

;; Mouse wheel behavior 
(global-set-key [C-wheel-up] 'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)
(global-set-key [C-down-mouse-2] 'text-scale-normal-size)

;; Right-click opens the context menu
(global-set-key [mouse-3] 'ergoemacs-context-menu)

(defvar edit-popup-menu
      '(keymap
	(undo menu-item "Undo" undo
	      :enable (and
		       (not buffer-read-only)
		       (not
			(eq t buffer-undo-list))
		       (if
			   (eq last-command 'undo)
			   (listp pending-undo-list)
			 (consp buffer-undo-list)))
	      :help "Undo last operation"
	      :keys "Ctrl+Z")
	(separator-undo menu-item "--")
	(cut menu-item "Cut" clipboard-kill-region
	     :help "Delete text in region and copy it to the clipboard"
	     :keys "Ctrl+X")
	(copy menu-item "Copy" clipboard-kill-ring-save
	      :help "Copy text in region to the clipboard"
	      :keys "Ctrl+C")
	(paste menu-item "Paste" clipboard-yank
	       :help "Paste text from clipboard"
	       :keys "Ctrl+V")
	(paste-from-menu menu-item "Paste from Kill Menu" yank-menu
			 :enable (and
				  (cdr yank-menu)
				  (not buffer-read-only))
			 :help "Choose a string from the kill ring and paste it")
	(clear menu-item "Clear" delete-region 
	       :enable (and mark-active (not buffer-read-only))
	       :help "Delete the text in region between mark and current position"
	       :keys "Del")
	(separator-select-all menu-item "--")
	(mark-whole-buffer menu-item "Select All" mark-whole-buffer
			   :help "Mark the whole buffer for a subsequent cut/copy")))

(defun ergoemacs-context-menu (event)
  "Pop up a context menu."
  (interactive "e")
  (popup-menu edit-popup-menu))
