;-*- coding: utf-8 -*-

(defun ergoemacs-unbind-globals ()

  (global-unset-key (kbd "M-1"))	; digit-argument
  (global-unset-key (kbd "M-2"))	; digit-argument
  (global-unset-key (kbd "M-3"))	; digit-argument
  (global-unset-key (kbd "M-4"))	; digit-argument
  (global-unset-key (kbd "M-5"))	; digit-argument
  (global-unset-key (kbd "M-6"))	; digit-argument
  (global-unset-key (kbd "M-7"))	; digit-argument
  (global-unset-key (kbd "M-8"))	; digit-argument
  (global-unset-key (kbd "M-9"))	; digit-argument
  (global-unset-key (kbd "M-0"))	; digit-argument

  (global-unset-key (kbd "M-a"))	; backward-sentence
  (global-unset-key (kbd "M-b"))	; backward-word
  (global-unset-key (kbd "M-c"))	; capitalize-word
  (global-unset-key (kbd "M-d"))	; kill-word
  (global-unset-key (kbd "M-e"))	; forward-sentence
  (global-unset-key (kbd "M-f"))	; forward-word
  (global-unset-key (kbd "M-g"))	; (prefix)
  (global-unset-key (kbd "M-h"))	; mark-paragraph
  (global-unset-key (kbd "M-i"))	; tab-to-tab-stop
  (global-unset-key (kbd "M-j"))	; indent-new-comment-line
  (global-unset-key (kbd "M-k"))	; kill-sentence
  (global-unset-key (kbd "M-l"))	; downcase-word
  (global-unset-key (kbd "M-m"))	; back-to-indentation
  (global-unset-key (kbd "M-n"))	; nil
  (global-unset-key (kbd "M-o"))	; nil
  (global-unset-key (kbd "M-p"))	; nil
  (global-unset-key (kbd "M-q"))	; fill-paragraph
  (global-unset-key (kbd "M-r"))	; move-to-window-line
  (global-unset-key (kbd "M-s"))	; nil
  (global-unset-key (kbd "M-t"))	; transpose-words
  (global-unset-key (kbd "M-u"))	; upcase-word
  (global-unset-key (kbd "M-v"))	; scroll-down
  (global-unset-key (kbd "M-w"))	; kill-ring-save
  (global-unset-key (kbd "M-x"))	; execute-extended-command
  (global-unset-key (kbd "M-y"))	; yank-pop
  (global-unset-key (kbd "M-z"))	; zap-to-char

  (global-unset-key (kbd "M-\\"))	; delete-horizontal-space
  (global-unset-key (kbd "M-@"))	; mark-word
  (global-unset-key (kbd "M--"))	; negative-argument
  (global-unset-key (kbd "M-<"))	; beginning-of-buffer
  (global-unset-key (kbd "M->"))	; end-of-buffer
  (global-unset-key (kbd "M-{"))	; backward-paragraph
  (global-unset-key (kbd "M-}"))	; forward-paragraph

  (global-unset-key (kbd "C-1"))	; digit-argument
  (global-unset-key (kbd "C-2"))	; digit-argument
  (global-unset-key (kbd "C-3"))	; digit-argument
  (global-unset-key (kbd "C-4"))	; digit-argument
  (global-unset-key (kbd "C-5"))	; digit-argument
  (global-unset-key (kbd "C-6"))	; digit-argument
  (global-unset-key (kbd "C-7"))	; digit-argument
  (global-unset-key (kbd "C-8"))	; digit-argument
  (global-unset-key (kbd "C-9"))	; digit-argument
  (global-unset-key (kbd "C-0"))	; digit-argument

  (global-unset-key (kbd "C-a"))	; move-beginning-of-line
  (global-unset-key (kbd "C-b"))	; backward-char
					;(global-unset-key (kbd "C-c")) ; (prefix)
  (global-unset-key (kbd "C-d"))	; delete-char
  (global-unset-key (kbd "C-e"))	; move-end-of-line
  (global-unset-key (kbd "C-f"))	; forward-char
					;(global-unset-key (kbd "C-g")) ; keyboard-quit
					;(global-unset-key (kbd "C-h")) ; (prefix)
					;(global-unset-key (kbd "C-i")) ; indent-for-tab-command; this is tab key
  (global-unset-key (kbd "C-j"))	; newline-and-indent
  (global-unset-key (kbd "C-k"))	; kill-line
  (global-unset-key (kbd "C-l"))	; recenter
					;(global-unset-key (kbd "C-m")) ; newline-and-indent; This is the Return key
  (global-unset-key (kbd "C-n"))	; next-line
  (global-unset-key (kbd "C-o"))	; open-line
  (global-unset-key (kbd "C-p"))	; previous-line
					;(global-unset-key (kbd "C-q")) ; quote-insert
  (global-unset-key (kbd "C-r"))	; isearch-backward
  (global-unset-key (kbd "C-s"))	; isearch-forward
  (global-unset-key (kbd "C-t"))	; transpose-chars
					;(global-unset-key (kbd "C-u")) ; universal-argument
  (global-unset-key (kbd "C-v"))	; scroll-up
  (global-unset-key (kbd "C-w"))	; kill-region
					;(global-unset-key (kbd "C-x")) ; (prefix)
  (global-unset-key (kbd "C-y"))	; yank
  (global-unset-key (kbd "C-z"))	; iconify-or-deiconify-frame

  (global-unset-key (kbd "C-/"))	 ; undo
  (global-unset-key (kbd "C-_"))	 ; undo
  (global-unset-key (kbd "C-<backspace>")) ; backward-kill-word

  (global-unset-key (kbd "C-@"))       ; cua-set-mark set-mark-command

  (global-unset-key (kbd "C-<prior>"))	; scroll-right
  (global-unset-key (kbd "C-<next>"))	; scroll-left

  (global-unset-key (kbd "C-x d"))	; dired
  (global-unset-key (kbd "C-x h"))	; mark-whole-buffer

  (global-unset-key (kbd "C-x C-d"))	; list-directory
  (global-unset-key (kbd "C-x C-f"))	; find-file
  (global-unset-key (kbd "C-x C-s"))	; save-buffer
  (global-unset-key (kbd "C-x C-w"))	; write-file

  (global-unset-key (kbd "C-x 0"))	; delete-window
  (global-unset-key (kbd "C-x 1"))	; delete-other-windows
  (global-unset-key (kbd "C-x 2"))	; split-window-vertically
  (global-unset-key (kbd "C-x 3"))	; split-window-horizontally
  (global-unset-key (kbd "C-x o"))	; other-windows

  (global-unset-key (kbd "C-x 5 0"))	; delete-frame
  (global-unset-key (kbd "C-x 5 2"))	; make-frame-command

  (global-unset-key (kbd "C-M-%"))	; query-replace-regexp
  )
