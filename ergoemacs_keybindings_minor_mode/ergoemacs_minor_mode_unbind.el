;-*- coding: utf-8 -*-

;; this file define keys that we want to set/unset because they are already defined by ergoemacs minor mode

;; (require 'edmacro) ; i think this is loaded by default

(setq redundant-keys 
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

;; TODO: Note: there's the function current-global-map. Could we actually switch global map? that way, we don't need to set/unset keys.



;; older version for unbind
;; (defun ergoemacs-unbind-globals ()

;;   (global-unset-key (kbd "M-1"))	; digit-argument
;;   (global-unset-key (kbd "M-2"))	; digit-argument
;;   (global-unset-key (kbd "M-3"))	; digit-argument
;;   (global-unset-key (kbd "M-4"))	; digit-argument
;;   (global-unset-key (kbd "M-5"))	; digit-argument
;;   (global-unset-key (kbd "M-6"))	; digit-argument
;;   (global-unset-key (kbd "M-7"))	; digit-argument
;;   (global-unset-key (kbd "M-8"))	; digit-argument
;;   (global-unset-key (kbd "M-9"))	; digit-argument
;;   (global-unset-key (kbd "M-0"))	; digit-argument

;;   (global-unset-key (kbd "M-a"))	; backward-sentence
;;   (global-unset-key (kbd "M-b"))	; backward-word
;;   (global-unset-key (kbd "M-c"))	; capitalize-word
;;   (global-unset-key (kbd "M-d"))	; kill-word
;;   (global-unset-key (kbd "M-e"))	; forward-sentence
;;   (global-unset-key (kbd "M-f"))	; forward-word
;;   (global-unset-key (kbd "M-g"))	; (prefix)
;;   (global-unset-key (kbd "M-h"))	; mark-paragraph
;;   (global-unset-key (kbd "M-i"))	; tab-to-tab-stop
;;   (global-unset-key (kbd "M-j"))	; indent-new-comment-line
;;   (global-unset-key (kbd "M-k"))	; kill-sentence
;;   (global-unset-key (kbd "M-l"))	; downcase-word
;;   (global-unset-key (kbd "M-m"))	; back-to-indentation
;;   (global-unset-key (kbd "M-n"))	; nil
;;   (global-unset-key (kbd "M-o"))	; nil
;;   (global-unset-key (kbd "M-p"))	; nil
;;   (global-unset-key (kbd "M-q"))	; fill-paragraph
;;   (global-unset-key (kbd "M-r"))	; move-to-window-line
;;   (global-unset-key (kbd "M-s"))	; nil
;;   (global-unset-key (kbd "M-t"))	; transpose-words
;;   (global-unset-key (kbd "M-u"))	; upcase-word
;;   (global-unset-key (kbd "M-v"))	; scroll-down
;;   (global-unset-key (kbd "M-w"))	; kill-ring-save
;;   (global-unset-key (kbd "M-x"))	; execute-extended-command
;;   (global-unset-key (kbd "M-y"))	; yank-pop
;;   (global-unset-key (kbd "M-z"))	; zap-to-char

;;   (global-unset-key (kbd "M-\\"))	; delete-horizontal-space
;;   (global-unset-key (kbd "M-@"))	; mark-word
;;   (global-unset-key (kbd "M--"))	; negative-argument
;;   (global-unset-key (kbd "M-<"))	; beginning-of-buffer
;;   (global-unset-key (kbd "M->"))	; end-of-buffer
;;   (global-unset-key (kbd "M-{"))	; backward-paragraph
;;   (global-unset-key (kbd "M-}"))	; forward-paragraph

;;   (global-unset-key (kbd "C-1"))	; digit-argument
;;   (global-unset-key (kbd "C-2"))	; digit-argument
;;   (global-unset-key (kbd "C-3"))	; digit-argument
;;   (global-unset-key (kbd "C-4"))	; digit-argument
;;   (global-unset-key (kbd "C-5"))	; digit-argument
;;   (global-unset-key (kbd "C-6"))	; digit-argument
;;   (global-unset-key (kbd "C-7"))	; digit-argument
;;   (global-unset-key (kbd "C-8"))	; digit-argument
;;   (global-unset-key (kbd "C-9"))	; digit-argument
;;   (global-unset-key (kbd "C-0"))	; digit-argument

;;   (global-unset-key (kbd "C-a"))	; move-beginning-of-line
;;   (global-unset-key (kbd "C-b"))	; backward-char
;; 					;(global-unset-key (kbd "C-c")) ; (prefix)
;;   (global-unset-key (kbd "C-d"))	; delete-char
;;   (global-unset-key (kbd "C-e"))	; move-end-of-line
;;   (global-unset-key (kbd "C-f"))	; forward-char
;; 					;(global-unset-key (kbd "C-g")) ; keyboard-quit
;; 					;(global-unset-key (kbd "C-h")) ; (prefix)
;; 					;(global-unset-key (kbd "C-i")) ; indent-for-tab-command; this is tab key
;;   (global-unset-key (kbd "C-j"))	; newline-and-indent
;;   (global-unset-key (kbd "C-k"))	; kill-line
;;   (global-unset-key (kbd "C-l"))	; recenter
;; 					;(global-unset-key (kbd "C-m")) ; newline-and-indent; This is the Return key
;;   (global-unset-key (kbd "C-n"))	; next-line
;;   (global-unset-key (kbd "C-o"))	; open-line
;;   (global-unset-key (kbd "C-p"))	; previous-line
;; 					;(global-unset-key (kbd "C-q")) ; quote-insert
;;   (global-unset-key (kbd "C-r"))	; isearch-backward
;;   (global-unset-key (kbd "C-s"))	; isearch-forward
;;   (global-unset-key (kbd "C-t"))	; transpose-chars
;; 					;(global-unset-key (kbd "C-u")) ; universal-argument
;;   (global-unset-key (kbd "C-v"))	; scroll-up
;;   (global-unset-key (kbd "C-w"))	; kill-region
;; 					;(global-unset-key (kbd "C-x")) ; (prefix)
;;   (global-unset-key (kbd "C-y"))	; yank
;;   (global-unset-key (kbd "C-z"))	; iconify-or-deiconify-frame

;;   (global-unset-key (kbd "C-/"))	 ; undo
;;   (global-unset-key (kbd "C-_"))	 ; undo
;;   (global-unset-key (kbd "C-<backspace>")) ; backward-kill-word

;;   (global-unset-key (kbd "C-@"))       ; cua-set-mark set-mark-command

;;   (global-unset-key (kbd "C-<prior>"))	; scroll-right
;;   (global-unset-key (kbd "C-<next>"))	; scroll-left

;;   (global-unset-key (kbd "C-x d"))	; dired
;;   (global-unset-key (kbd "C-x h"))	; mark-whole-buffer

;;   (global-unset-key (kbd "C-x C-d"))	; list-directory
;;   (global-unset-key (kbd "C-x C-f"))	; find-file
;;   (global-unset-key (kbd "C-x C-s"))	; save-buffer
;;   (global-unset-key (kbd "C-x C-w"))	; write-file

;;   (global-unset-key (kbd "C-x 0"))	; delete-window
;;   (global-unset-key (kbd "C-x 1"))	; delete-other-windows
;;   (global-unset-key (kbd "C-x 2"))	; split-window-vertically
;;   (global-unset-key (kbd "C-x 3"))	; split-window-horizontally
;;   (global-unset-key (kbd "C-x o"))	; other-windows

;;   (global-unset-key (kbd "C-x 5 0"))	; delete-frame
;;   (global-unset-key (kbd "C-x 5 2"))	; make-frame-command

;;   (global-unset-key (kbd "C-M-%"))	; query-replace-regexp
;;   )
