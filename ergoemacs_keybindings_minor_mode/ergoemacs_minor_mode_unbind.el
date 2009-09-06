;-*- coding: utf-8 -*-

;; this file define keys that we want to set/unset because they are already defined by ergoemacs minor mode

;; (require 'edmacro) ; i think this is loaded by default

(defvar ergoemacs-redundant-keys
'(
("C-/" . undo)
("C-0" . digit-argument)
("C-1" . digit-argument)
("C-2" . digit-argument)
("C-3" . digit-argument)
("C-4" . digit-argument)
("C-5" . digit-argument)
("C-6" . digit-argument)
("C-7" . digit-argument)
("C-8" . digit-argument)
("C-9" . digit-argument)
("C-<backspace>" . backward-kill-word)
("C-<next>" . scroll-left)
("C-<prior>" . scroll-right)
("C-@" . set-mark-command)
("C-M-%" . query-replace-regexp)
("C-_" . undo)
("C-a" . move-beginning-of-line)
("C-b" . backward-char)
("C-d" . delete-char)
("C-e" . move-end-of-line)
("C-f" . forward-char)
("C-j" . newline-and-indent)
("C-k" . kill-line)
("C-l" . recenter)
("C-n" . next-line)
("C-o" . open-line)
("C-p" . previous-line)
("C-r" . isearch-backward)
("C-s" . isearch-forward)
("C-t" . transpose-chars)
("C-v" . scroll-up)
("C-w" . kill-region)
("C-x 0" . delete-window)
("C-x 1" . delete-other-windows)
("C-x 2" . split-window-vertically)
("C-x 3" . split-window-horizontally)
("C-x 5 0" . delete-frame)
("C-x 5 2" . make-frame-command)
("C-x C-d" . list-directory)
("C-x C-f" . find-file)
("C-x C-s" . save-buffer)
("C-x C-w" . write-file)
("C-x d" . dired)
("C-x h" . mark-whole-buffer)
("C-x o" . other-windows)
("C-y" . yank)
("C-z" . iconify-or-deiconify-frame)
("M--" . negative-argument)
("M-0" . digit-argument)
("M-1" . digit-argument)
("M-2" . digit-argument)
("M-3" . digit-argument)
("M-4" . digit-argument)
("M-5" . digit-argument)
("M-6" . digit-argument)
("M-7" . digit-argument)
("M-8" . digit-argument)
("M-9" . digit-argument)
("M-<" . beginning-of-buffer)
("M->" . end-of-buffer)
("M-@" . mark-word)
("M-\\" . delete-horizontal-space)
("M-a" . backward-sentence)
("M-b" . backward-word)
("M-c" . capitalize-word)
("M-d" . kill-word)
("M-e" . forward-sentence)
("M-f" . forward-word)
("M-h" . mark-paragraph)
("M-i" . tab-to-tab-stop)
("M-j" . indent-new-comment-line)
("M-k" . kill-sentence)
("M-l" . downcase-word)
("M-m" . back-to-indentation)
("M-n" . nil)
("M-o" . nil)
("M-p" . nil)
("M-q" . fill-paragraph)
("M-r" . move-to-window-line)
("M-s" . nil)
("M-t" . transpose-words)
("M-u" . upcase-word)
("M-v" . scroll-down)
("M-w" . kill-ring-save)
("M-x" . execute-extended-command)
("M-y" . yank-pop)
("M-z" . zap-to-char)
("M-{" . backward-paragraph)
("M-}" . forward-paragraph)
)
)

;; Some exceptions we don't want to unset.
;; "C-g" 'keyboard-quit
;; "C-i" 'indent-for-tab-command
;; "C-m" 'newline-and-indent
;; "C-q" 'quote-insert
;; "C-u" 'universal-argument
;; "C-h" ; (help-map)
;; "C-x" ; (ctl-x-map)
;; "C-c" ; (prefix)
;; "M-g" ; (prefix)

(defvar ergoemacs-overriden-global-keys '()
  "Alist to store overriden keyboard shortcuts in
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
    ;; save that shortcut in ergoemacs-overriden-global-keys
    (if oldcmd
	(add-to-list 'ergoemacs-overriden-global-keys (cons map (cons key-s (cons oldcmd nil)))))
    ;; redefine the key in the ergoemacs-keymap
    (define-key map key nil)
    )
  )

(defun ergoemacs-unset-redundant-global-keys ()
  "Unsets redundant keyboard shortcuts that should not be used in ErgoEmacs."
  (mapc (lambda (x)
	  (ergoemacs-unset-global-key (current-global-map) (car x)))
	ergoemacs-redundant-keys)
  )

(defun ergoemacs-restore-global-keys ()
  "Restores all keyboard shortcuts that were overwritten by `ergoemacs-unbind-global-key'."
  (mapc (lambda (x)
	  (define-key
	    (car x)
	    (edmacro-parse-keys (car (cdr x)))
	    (car (cdr (cdr x))))
	  )
	ergoemacs-overriden-global-keys)
  (setq ergoemacs-overriden-global-keys '()) ; clear the list
  )
