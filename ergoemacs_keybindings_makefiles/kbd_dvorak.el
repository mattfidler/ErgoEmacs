;-*- coding: utf-8 -*-
;; ergonomic_keybinding_dvorak.el -- A ergonomic keybinding for Dvorak keyboard.

«header_file.el»

;;; History:

«version_history.el»

;;; Code:

;;; --------------------------------------------------
;;; UNBIND DEFAULT KEYS

«unbind.el»

;;; --------------------------------------------------
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-n") 'forward-char)
(global-set-key (kbd "M-c") 'previous-line)
(global-set-key (kbd "M-t") 'next-line)

;; Move by word
(global-set-key (kbd "M-g") 'backward-word) ; was (prefix)
(global-set-key (kbd "M-r") 'forward-word)

;; Move by paragraph
(global-set-key (kbd "M-G") 'backward-paragraph)
(global-set-key (kbd "M-R") 'forward-paragraph)

;; Move to beginning/ending of line
(global-set-key (kbd "M-d") 'move-beginning-of-line)
(global-set-key (kbd "M-D") 'move-end-of-line)

;; Move by screen (page up/down)
(global-set-key (kbd "M-T") 'scroll-up)
(global-set-key (kbd "M-C") 'scroll-down)

;; Move to beginning/ending of file
(global-set-key (kbd "M-H") 'beginning-of-buffer)
(global-set-key (kbd "M-N") 'end-of-buffer)

;; isearch
(global-set-key (kbd "M-s") 'isearch-forward)
(global-set-key (kbd "M-S") 'isearch-backward)

(global-set-key (kbd "M-l")
                (if (fboundp 'recenter-top-bottom)
                    'recenter-top-bottom
                  'recenter
                  ))

;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(global-set-key (kbd "M-e") 'delete-backward-char)
(global-set-key (kbd "M-u") 'delete-char)

; Delete previous/next word.
(global-set-key (kbd "M-.") 'backward-kill-word)
(global-set-key (kbd "M-p") 'kill-word)

; Copy Cut Paste, Paste previous
(global-set-key (kbd "M-q") 'kill-region)
(global-set-key (kbd "M-j") 'kill-ring-save)
(global-set-key (kbd "M-k") 'yank)
(global-set-key (kbd "M-K") 'yank-pop)
(global-set-key (kbd "M-J") 'copy-all)
(global-set-key (kbd "M-Q") 'cut-all)

;; undo and redo
(global-set-key (kbd "M-:") 'redo)
(global-set-key (kbd "M-;") 'undo)

; Kill line
(global-set-key (kbd "M-i") 'kill-line)
(global-set-key (kbd "M-I") 'kill-line-backward)

;;; Textual Transformation

(global-set-key (kbd "M-S-SPC") 'mark-paragraph)
(global-set-key (kbd "M-,") 'shrink-whitespaces)
(global-set-key (kbd "M--") 'comment-dwim)
(global-set-key (kbd "M-z") 'toggle-letter-case)

; keyword completion, because Alt+Tab is used by OS
(global-set-key (kbd "M-y") 'call-keyword-completion) 

; Hard-wrap/un-hard-wrap paragraph
(global-set-key (kbd "M-'") 'compact-uncompact-block)

;;; EMACS'S SPECIAL COMMANDS

; Mark point.
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-A") 'shell-command)

;;; WINDOW SPLITING
(global-set-key (kbd "M-o") 'move-cursor-next-pane) ; was prefix
(global-set-key (kbd "M-O") 'move-cursor-previous-pane)

;;; --------------------------------------------------
;;; STANDARD SHORTCUTS

«common_bindings.el»

;;; --------------------------------------------------
;;; RECLAIM SOME BINDINGS

;; isearch
(add-hook 'isearch-mode-hook
 (lambda ()
 (define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
 (define-key isearch-mode-map (kbd "M-S") 'isearch-repeat-backward)

 (define-key isearch-mode-map (kbd "M-n") 'forward-char) ; was isearch-ring-advance
 (define-key isearch-mode-map (kbd "M-p") 'kill-word) ; was isearch-ring-retreat

 (define-key isearch-mode-map (kbd "M-c") 'previous-line) ; was isearch-toggle-case-fold
 (define-key isearch-mode-map (kbd "M-r") 'forward-word) ; was isearch-toggle-regexp
 (define-key isearch-mode-map (kbd "M-e") 'delete-backward-char) ; was isearch-edit-string

 (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
 (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
 )
)

;; reclaim some bindings used in minibuffer
(define-key minibuffer-local-map (kbd "M-p") 'kill-word) ; was previous-history-element. Use ↑ key or f11.
(define-key minibuffer-local-map (kbd "M-n") 'forward-char) ; was next-history-element. Use ↓ key or f12.
(define-key minibuffer-local-map (kbd "M-r") 'forward-word) ; was previous-matching-history-element
(define-key minibuffer-local-map (kbd "M-s") 'isearch-forward) ; was next-matching-history-element
(define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
(define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
(define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
(define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element)


;; reclaim some binding used by shell mode and shell-command.
(add-hook 'comint-mode-hook
 (lambda ()
   (define-key comint-mode-map (kbd "M-p") 'kill-word) ; was comint-previous-input. Use Ctrl+↑ or f11
   (define-key comint-mode-map (kbd "M-n") 'forward-char) ; was comint-next-input. Use Ctrl+↓ or f12
   (define-key comint-mode-map (kbd "M-r") 'forward-word) ; was comint-previous-matching-input.
   (define-key comint-mode-map (kbd "M-s") 'isearch-forward) ; was comint-next-matching-input.

   (define-key comint-mode-map (kbd "<f11>") 'comint-previous-input)
   (define-key comint-mode-map (kbd "<f12>") 'comint-next-input)
   (define-key comint-mode-map (kbd "S-<f11>") 'comint-previous-matching-input)
   (define-key comint-mode-map (kbd "S-<f12>") 'comint-next-matching-input)
))

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-n") 'new-empty-buffer) ; was dired-next-line
  (define-key dired-mode-map (kbd "M-o") 'other-window) ; was dired-omit-mode
  (define-key dired-mode-map (kbd "M-s") 'isearch-forward) ; was prefix in emacs 23.
 ))

(add-hook 'Info-mode-hook
 (lambda ()
 (define-key Info-mode-map (kbd "M-n") 'forward-char) ; was clone-buffer
 (define-key Info-mode-map (kbd "M-s") 'isearch-forward) ; was Info-search; just press “s” instead for isearch-forward
 )
)

(add-hook 'text-mode-hook
 (lambda ()
 (define-key text-mode-map (kbd "M-s") 'isearch-forward) ; was center-line
 (define-key text-mode-map (kbd "M-S") 'isearch-backward) ; was center-paragraph
 )
)

;; prevent cua-mode from going into selection mode when commands with Shift key is used.
(add-hook 'cua-mode-hook
 (lambda ()
    (put 'cua-scroll-down 'CUA nil)
    (put 'cua-scroll-up 'CUA nil)
    (put 'backward-paragraph 'CUA nil)
    (put 'forward-paragraph 'CUA nil)
    (put 'beginning-of-buffer 'CUA nil)
    (put 'end-of-buffer 'CUA nil)
    (put 'move-end-of-line 'CUA nil)
   )
 )

;; reclaim some binding used by ibuffer.el
(add-hook 'ibuffer-mode-hook
 (lambda ()
   (define-key ibuffer-mode-map (kbd "M-s") 'isearch-forward) ; was prefix in emacs 23.
   (define-key ibuffer-mode-map (kbd "M-g") 'backward-word) ; was ibuffer-jump-to-buffer. Use “j” instead.
   (define-key ibuffer-mode-map (kbd "M-p") 'kill-word) ; was ibuffer-backward-filter-group Use “←” instead.
   (define-key ibuffer-mode-map (kbd "M-n") 'forward-char) ; was ibuffer-forward-filter-group. Use “→” instead.
   (define-key ibuffer-mode-map (kbd "M-j") 'kill-ring-save) ; was ibuffer-jump-to-filter-group.
   (define-key ibuffer-mode-map (kbd "M-o") 'other-window) ; was ibuffer-visit-buffer-1-window
   (define-key ibuffer-mode-map (kbd "C-o") 'find-file) ; was ibuffer-visit-buffer-other-window-noselect.
))

(add-hook 'html-mode-hook
 (lambda ()
 (define-key html-mode-map (kbd "M-s") 'isearch-forward) ; was center-line
 (define-key html-mode-map (kbd "M-S") 'isearch-backward) ; was center-paragraph
 )
)

(add-hook 'nxml-mode-hook
 (lambda ()
 (define-key nxml-mode-map (kbd "M-h") 'backward-char) ; was nxml-mark-paragraph
 (define-key nxml-mode-map (kbd "C-M-SPC") 'nxml-mark-paragraph)
 )
)

(add-hook 'diff-mode-hook
 (lambda ()
  (define-key diff-mode-map (kbd "M-n") 'forward-char) ; was diff-hunk-next
  (define-key diff-mode-map (kbd "M-N") 'end-of-buffer) ; was diff-file-next

  (define-key diff-mode-map (kbd "M-p") 'kill-word) ; was diff-hunk-prev
  (define-key diff-mode-map (kbd "M-k") 'yank) ; was diff-hunk-kill
  (define-key diff-mode-map (kbd "M-K") 'yank-pop) ; was diff-file-kill

 (define-key diff-mode-map (kbd "<f11>") 'diff-hunk-prev)
 (define-key diff-mode-map (kbd "<f12>") 'diff-hunk-next)
 (define-key diff-mode-map (kbd "S-<f11>") 'diff-file-prev)
 (define-key diff-mode-map (kbd "S-<f12>") 'diff-file-next)
 ))

(add-hook 'w3m-mode-hook
 (lambda ()
  (define-key w3m-mode-map (kbd "M-a") 'execute-extended-command) ; was w3m-bookmark-add-this-url
  (define-key w3m-mode-map (kbd "M-g") 'backward-word) ; was goto-line
  (define-key w3m-mode-map (kbd "M-n") 'forward-char) ; was w3m-copy-buffer
  (define-key w3m-mode-map (kbd "M-l") 'recenter) ; was w3m-horizontal-recenter

  (define-key w3m-mode-map (kbd "M-i") 'kill-line) ; was w3m-save-image
  (define-key w3m-mode-map (kbd "M-k") 'yank) ; was w3m-cookie
))

(add-hook 'rcirc-mode-hook
 (lambda ()
  (define-key rcirc-mode-map (kbd "M-p") 'kill-word) ; was rcirc-insert-prev-input
  (define-key rcirc-mode-map (kbd "M-n") 'forward-char) ; was rcirc-insert-next-input
  (define-key rcirc-mode-map (kbd "<f11>") 'rcirc-insert-prev-input)
  (define-key rcirc-mode-map (kbd "<f12>") 'rcirc-insert-next-input)
 ))

(add-hook 'awk-mode-hook
 (lambda ()
  (define-key awk-mode-map (kbd "M-a") 'execute-extended-command) ; was c-beginning-of-statement
  (define-key awk-mode-map (kbd "M-e") 'delete-backward-char) ; was c-end-of-statement
 ))

(add-hook 'message-mode-hook
; M-; comment-region
; M-n message-display-abbrev
 (lambda ()
  (define-key message-mode-map (kbd "M-;") 'undo)
  (define-key message-mode-map (kbd "M-n") 'forward-char)
 ))

;; nothing to fix: c-mode, c++-mode, java, sh, js, perl, php, python

;;; --------------------------------------------------
;;; FUNCTIONS

«functions.el»