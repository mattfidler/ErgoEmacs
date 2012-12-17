;;-*- coding: utf-8 -*-

;; this file define keys that we want to set/unset because they are already defined by ergoemacs minor mode

(require 'edmacro)

(defcustom ergoemacs-redundant-keys
  '( ("C-/" undo)
     ("C-0" digit-argument)
     ("C-1" digit-argument)
     ("C-2" digit-argument)
     ("C-3" digit-argument)
     ("C-4" digit-argument)
     ("C-5" digit-argument)
     ("C-6" digit-argument)
     ("C-7" digit-argument)
     ("C-8" digit-argument)
     ("C-9" digit-argument)
     ("M-0" digit-argument)
     ("M-1" digit-argument)
     ("M-2" digit-argument)
     ("M-3" digit-argument)
     ("M-4" digit-argument)
     ("M-5" digit-argument)
     ("M-6" digit-argument)
     ("M-7" digit-argument)
     ("M-8" digit-argument)
     ("M-9" digit-argument)
     
     ("C-<prior>" scroll-right)
     ("C-<next>" scroll-left)
     ("C-@" set-mark-command)
     ("C-M-%" query-replace-regexp)
     ("C-_" undo)
     ("C-a" move-beginning-of-line)
     ("C-b" backward-char)
     ("C-c" mode-specific-command-prefix)
     ("C-d" delete-char)
     ("C-e" move-end-of-line)
     ("C-f" forward-char)
     ("C-k" kill-line)
     ("C-l" recenter-top-bottom)
     ("RET" newline)
     ("C-n" next-line)
     ("C-o" open-line)
     ("C-p" previous-line)
     ("C-q" quoted-insert)
     ("C-r" isearch-backward)
     ("C-s" isearch-forward)
     ("C-t" transpose-chars)
     ("C-u" universal-argument)
     ("C-v" scroll-up-command)
     ("C-w" kill-region)
     
     ("C-x 0" delete-window)
     ("C-x 1" delete-other-windows)
     ("C-x 2" split-window-below)
     ("C-x 3" split-window-right)
     
     ("C-x 5 0" delete-frame)
     ("C-x 5 2" make-frame-command)
     
     ("C-x C-d" list-directory)
     ("C-x C-f" find-file)
     ("C-x C-s" save-buffer)
     ("C-x C-w" write-file)
     ("C-x h" mark-whole-buffer)
     ("C-x o" other-window)
     ("C-y" yank-pop)
     ("C-z" suspend-frame)
     ("M--" negative-argument)
     ("M-<" beginning-of-buffer)
     ("M->" end-of-buffer)
     ("M-@" mark-word)
     ("M-\\" delete-horizontal-space)
     ("M-a" backward-sentence)
     ("M-b" backward-word)
     ("M-c" capitalize-word)
     ("M-d" kill-word)
     ("M-e" forward-sentence)
     ("M-f" forward-word)
     ("M-g" Prefix Command)
     ("M-h" mark-paragraph)
     ("M-i" tab-to-tab-stop)
     ("M-j" indent-new-comment-line)
     ("M-k" kill-sentence)
     ("M-l" downcase-word)
     ("M-m" back-to-indentation)
     ("M-o" facemenu-keymap)
     ("M-q" fill-paragraph)
     ("M-r" move-to-window-line-top-bottom)
     ("M-s" Prefix Command)
     ("M-t" transpose-words)
     ("M-u" upcase-word)
     ("M-v" scroll-down-command)
     ("M-w" kill-ring-save)
     ("M-x" execute-extended-command)
     ("M-y" yank-pop)
     ("M-z" zap-to-char)
     ("M-{" backward-paragraph)
     ("M-}" forward-paragraph))
  "These are the redundant key bindings in emacs that ErgoEmacs unbinds.  Some exceptions we do not want to unset are:

Some exceptions we don't want to unset.
\"C-g\" 'keyboard-quit
\"C-i\" 'indent-for-tab-command
\"C-m\" 'newline-and-indent
\"C-q\" 'quote-insert
\"C-u\" 'universal-argument
\"C-h\" ; (help-map)
\"C-x\" ; (ctl-x-map)
\"C-c\" ; (prefix)
\"M-g\" ; (prefix)

"
  :type '(repeat
          (list :tag "Old Emacs Key"
                (choice (string :tag "Kbd Code")
                        (sexp :tag "Key"))
                (symbol :tag "Old Function")))
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)



(defvar ergoemacs-overridden-global-keys '()
  "Alist to store overridden keyboard shortcuts in
  `current-global-map' and other maps. Each item looks like '(MAP KEY OLD-COMMAND).")

(defun ergoemacs-unset-global-key (map key-s)
  "Sets to nil the associated command for the specified key in specified map.
It is like:

  \(define-key map (kbd key-s) nil))

But it saves the old command associated with the
specified key, so we can restore it when ergoemacs minor mode is
disabled at `ergoemacs-restore-global-keys'."
  (let (key oldcmd)
    (setq key (edmacro-parse-keys key-s))
    ;; get the old command associated with this key
    (setq oldcmd (lookup-key map key))
    ;; save that shortcut in ergoemacs-overridden-global-keys
    (if oldcmd
	(add-to-list 'ergoemacs-overridden-global-keys (cons map (cons key-s (cons oldcmd nil)))))
    ;; redefine the key in the ergoemacs-keymap
    (define-key map key nil)))

(defun ergoemacs-unset-redundant-global-keys ()
  "Unsets redundant keyboard shortcuts that should not be used in ErgoEmacs."
  (mapc (lambda (x)
          (if (eq (lookup-key (current-global-map) (read-kbd-macro (nth 0 x)) t) (nth 1 x))
              (ergoemacs-unset-global-key (current-global-map) (nth 0 x))
            (define-key ergoemacs-keymap (read-kbd-macro (nth 0 x)) (lookup-key (current-global-map) (read-kbd-macro (nth 0 x)) t))))
	ergoemacs-redundant-keys))

(defun ergoemacs-restore-global-keys ()
  "Restores all keyboard shortcuts that were overwritten by `ergoemacs-unbind-global-key'."
  (mapc (lambda (x)
	  (define-key
	    (car x)
	    (edmacro-parse-keys (car (cdr x)))
	    (car (cdr (cdr x)))))
	ergoemacs-overridden-global-keys)
  (setq ergoemacs-overridden-global-keys '()) ; clear the list
  )

;; Based on describe-key-briefly
(defun where-is-old-binding (&optional key)
  "Print the name of the function KEY invoked before to start ErgoEmacs minor mode."
  (interactive
   (let ((enable-disabled-menus-and-buttons t)
	 (cursor-in-echo-area t)
	 saved-yank-menu)
     (unwind-protect
	 (let (key)
	   ;; If yank-menu is empty, populate it temporarily, so that
	   ;; "Select and Paste" menu can generate a complete event.
	   (when (null (cdr yank-menu))
	     (setq saved-yank-menu (copy-sequence yank-menu))
	     (menu-bar-update-yank-menu "(any string)" nil))
	   (setq key (read-key-sequence "Describe old key (or click or menu item): "))
	   ;; If KEY is a down-event, read and discard the
	   ;; corresponding up-event.  Note that there are also
	   ;; down-events on scroll bars and mode lines: the actual
	   ;; event then is in the second element of the vector.
	   (and (vectorp key)
		(let ((last-idx (1- (length key))))
		  (and (eventp (aref key last-idx))
		       (memq 'down (event-modifiers (aref key last-idx)))))
		(read-event))
	   (list key))
       ;; Put yank-menu back as it was, if we changed it.
       (when saved-yank-menu
	 (setq yank-menu (copy-sequence saved-yank-menu))
	 (fset 'yank-menu (cons 'keymap yank-menu))))))
  
  (let (key-desc item item-key item-cmd old-cmd)
    (setq key-desc (key-description key))
    (setq item ergoemacs-overridden-global-keys)
    (while (and item (not old-cmd))
      (setq item-key (car (cdr (car item))))
      (setq item-cmd (car (cdr (cdr (car item)))))
      (if (string= item-key key-desc)
	  (setq old-cmd item-cmd))
      (setq item (cdr item))
      )
    (if old-cmd
	(with-temp-buffer
	  (where-is old-cmd t)
	  (message "Key %s was bound to %s which is now invoked by %s"
		   key-desc old-cmd (buffer-string)))
      (message "Key %s was not bound to any command" key-desc))))
