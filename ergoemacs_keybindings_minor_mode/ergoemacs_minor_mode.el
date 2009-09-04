;-*- coding: utf-8 -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(load "functions.el")

;; ErgoEmacs minor mode keymap
(defvar ergoemacs-keymap (make-sparse-keymap))

(load "ergoemacs_minor_mode_unbind")

;; Load the keyboard layout looking the ERGOEMACS_KEYBOARD_LAYOUT
;; enviroment variable (this variable is set by ErgoEmacs runner)
(defvar ergoemacs-keyboard-layout (getenv "ERGOEMACS_KEYBOARD_LAYOUT")
  "It is set with the value of ERGOEMACS_KEYBOARD_LAYOUT
enviroment variable.  The possible values are:

  us = US English QWERTY keyboard layout
  dv = US-Dvorak keyboard layout
  sp = Spanish keyboard layout")

(cond
 ((string= ergoemacs-keyboard-layout "us")
  (load "ergoemacs_minor_mode_qwerty")
  )
 ((or (string= ergoemacs-keyboard-layout "us_dvorak")
      (string= ergoemacs-keyboard-layout "dv"))
  (load "ergoemacs_minor_mode_dvorak")
  )
 ((string= ergoemacs-keyboard-layout "sp")
  (load "ergoemacs_minor_mode_qwerty_sp")
  )
 ((string= ergoemacs-keyboard-layout "colemak")
  (load "ergoemacs_minor_mode_colemak")
  )
 ;; Qwerty by default
 (t
  (load "ergoemacs_minor_mode_qwerty")
  )
 )

;; Single char cursor movement
(define-key ergoemacs-keymap ergoemacs-backward-char-key 'backward-char)
(define-key ergoemacs-keymap ergoemacs-forward-char-key 'forward-char)
(define-key ergoemacs-keymap ergoemacs-previous-line-key 'previous-line)
(define-key ergoemacs-keymap ergoemacs-next-line-key 'next-line)

;; Move by word
(define-key ergoemacs-keymap ergoemacs-backward-word-key 'backward-word)
(define-key ergoemacs-keymap ergoemacs-forward-word-key 'forward-word)

;; Move by paragraph
(define-key ergoemacs-keymap ergoemacs-backward-paragraph-key 'backward-paragraph)
(define-key ergoemacs-keymap ergoemacs-forward-paragraph-key 'forward-paragraph)

;; Move to beginning/ending of line
(define-key ergoemacs-keymap ergoemacs-move-beginning-of-line-key 'move-beginning-of-line)
(define-key ergoemacs-keymap ergoemacs-move-end-of-line-key 'move-end-of-line)

;; Move by screen (page up/down)
(define-key ergoemacs-keymap ergoemacs-scroll-down-key 'scroll-down)
(define-key ergoemacs-keymap ergoemacs-scroll-up-key 'scroll-up)

;; Move to beginning/ending of file
(define-key ergoemacs-keymap ergoemacs-beginning-of-buffer-key 'beginning-of-buffer)
(define-key ergoemacs-keymap ergoemacs-end-of-buffer-key 'end-of-buffer)

;; isearch
(define-key ergoemacs-keymap ergoemacs-isearch-forward-key 'isearch-forward)
(define-key ergoemacs-keymap ergoemacs-isearch-backward-key 'isearch-backward)

(define-key ergoemacs-keymap ergoemacs-recenter-key 'recenter)

;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(define-key ergoemacs-keymap ergoemacs-delete-backward-char-key 'delete-backward-char)
(define-key ergoemacs-keymap ergoemacs-delete-char-key 'delete-char)

; Delete previous/next word.
(define-key ergoemacs-keymap ergoemacs-backward-kill-word-key 'backward-kill-word)
(define-key ergoemacs-keymap ergoemacs-kill-word-key 'kill-word)

; Copy Cut Paste, Paste previous
(define-key ergoemacs-keymap ergoemacs-kill-region-key 'kill-region)
(define-key ergoemacs-keymap ergoemacs-kill-ring-save-key 'kill-ring-save)
(define-key ergoemacs-keymap ergoemacs-yank-key 'yank)
(define-key ergoemacs-keymap ergoemacs-yank-pop-key 'yank-pop)
(define-key ergoemacs-keymap ergoemacs-copy-all-key 'copy-all)
(define-key ergoemacs-keymap ergoemacs-cut-all-key 'cut-all)

;; undo and redo
(define-key ergoemacs-keymap ergoemacs-redo-key 'redo)
(define-key ergoemacs-keymap ergoemacs-undo-key 'undo)

; Kill line
(define-key ergoemacs-keymap ergoemacs-kill-line-key 'kill-line)
(define-key ergoemacs-keymap ergoemacs-kill-line-backward-key 'kill-line-backward)

;;; Textual Transformation

(define-key ergoemacs-keymap ergoemacs-mark-paragraph-key 'mark-paragraph)
(define-key ergoemacs-keymap ergoemacs-shrink-whitespaces-key 'shrink-whitespaces)
(define-key ergoemacs-keymap ergoemacs-comment-dwim-key 'comment-dwim)
(define-key ergoemacs-keymap ergoemacs-toggle-letter-case-key 'toggle-letter-case)

; keyword completion, because Alt+Tab is used by OS
(define-key ergoemacs-keymap ergoemacs-call-keyword-completion-key 'call-keyword-completion)

; Hard-wrap/un-hard-wrap paragraph
(define-key ergoemacs-keymap ergoemacs-compact-uncompact-block-key 'compact-uncompact-block)

;;; EMACS'S SPECIAL COMMANDS

; Mark point.
(define-key ergoemacs-keymap ergoemacs-set-mark-command-key 'set-mark-command)

(define-key ergoemacs-keymap ergoemacs-execute-extended-command-key 'execute-extended-command)
(define-key ergoemacs-keymap ergoemacs-shell-command-key 'shell-command)

;;; WINDOW SPLITING
(define-key ergoemacs-keymap ergoemacs-move-cursor-next-pane-key 'move-cursor-next-pane)
(define-key ergoemacs-keymap ergoemacs-move-cursor-previous-pane-key 'move-cursor-previous-pane)

;;; --------------------------------------------------
;;; STANDARD SHORTCUTS

(define-key ergoemacs-keymap (kbd "C-n") 'new-empty-buffer)
(define-key ergoemacs-keymap (kbd "C-S-n") 'make-frame-command)
(define-key ergoemacs-keymap (kbd "C-o") 'find-file)
(define-key ergoemacs-keymap (kbd "C-w") 'close-current-buffer)
(define-key ergoemacs-keymap (kbd "C-s") 'save-buffer)
(define-key ergoemacs-keymap (kbd "C-S-s") 'write-file)
(define-key ergoemacs-keymap (kbd "C-p") 'print-buffer)
(define-key ergoemacs-keymap (kbd "C-a") 'mark-whole-buffer)
(define-key ergoemacs-keymap (kbd "C-S-w") 'delete-frame)

(define-key ergoemacs-keymap (kbd "C-f") 'search-forward)

(define-key ergoemacs-keymap (kbd "<delete>") 'delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.

(define-key ergoemacs-keymap (kbd "C-<prior>") 'previous-user-buffer)
(define-key ergoemacs-keymap (kbd "C-<next>") 'next-user-buffer)

(define-key ergoemacs-keymap (kbd "C-S-<prior>") 'previous-emacs-buffer)
(define-key ergoemacs-keymap (kbd "C-S-<next>") 'next-emacs-buffer)

(define-key ergoemacs-keymap (kbd "M-S-<prior>") 'backward-page)
(define-key ergoemacs-keymap (kbd "M-S-<next>") 'forward-page)

(define-key ergoemacs-keymap (kbd "C-x C-b") 'ibuffer)
(define-key ergoemacs-keymap (kbd "C-h m") 'describe-major-mode)

;; Ctrl+Break is a common IDE shortcut to stop compilation/find/grep
(define-key ergoemacs-keymap (kbd "C-<pause>") 'kill-compilation)

;;; --------------------------------------------------
;;; OTHER SHORTCUTS

(define-key ergoemacs-keymap ergoemacs-switch-to-previous-frame-key 'switch-to-previous-frame)
(define-key ergoemacs-keymap ergoemacs-switch-to-next-frame-key 'switch-to-next-frame)

(define-key ergoemacs-keymap ergoemacs-query-replace-key 'query-replace)
(define-key ergoemacs-keymap ergoemacs-query-replace-regexp-key 'query-replace-regexp)

(define-key ergoemacs-keymap ergoemacs-delete-other-windows-key 'delete-other-windows)
(define-key ergoemacs-keymap ergoemacs-delete-window-key 'delete-window)

(define-key ergoemacs-keymap ergoemacs-split-window-vertically-key 'split-window-vertically)
(define-key ergoemacs-keymap ergoemacs-split-window-horizontally-key 'split-window-horizontally)

(define-key ergoemacs-keymap ergoemacs-extend-selection-key 'extend-selection)
(define-key ergoemacs-keymap ergoemacs-select-text-in-quote-key 'select-text-in-quote)

;;----------------------------------------------------------------------
;; ErgoEmacs minor mode

;; Prevent cua-mode from going into selection mode when commands with Shift key is used.
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

(defun ergoemacs-isearch-hook ()
  "Hook for `isearch-mode-hook' so ergoemacs keybindings are not lost."

  (define-key isearch-mode-map (kbd "M-p") 'nil) ; was isearch-ring-retreat
  (define-key isearch-mode-map (kbd "M-n") 'nil) ; was isearch-ring-advance
  (define-key isearch-mode-map (kbd "M-y") 'nil) ; was isearch-yank-kill
  (define-key isearch-mode-map (kbd "M-c") 'nil) ; was isearch-toggle-case-fold
  (define-key isearch-mode-map (kbd "M-r") 'nil) ; was isearch-toggle-regexp
  (define-key isearch-mode-map (kbd "M-e") 'nil) ; was isearch-edit-string

  (define-key isearch-mode-map ergoemacs-isearch-forward-key 'isearch-repeat-forward)
  (define-key isearch-mode-map ergoemacs-isearch-backward-key 'isearch-repeat-backward)
  (define-key isearch-mode-map ergoemacs-recenter-key 'recenter)
  (define-key isearch-mode-map ergoemacs-yank-key 'isearch-yank-kill)

  ;; isearch-other-control-char sends the key to the original buffer and cancels isearch
  (define-key isearch-mode-map ergoemacs-kill-ring-save-key 'isearch-other-control-char)
  (define-key isearch-mode-map ergoemacs-kill-word-key 'isearch-other-control-char)
  (define-key isearch-mode-map ergoemacs-backward-kill-word-key 'isearch-other-control-char)

  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
  )

;; Hook for interpreters
(defun ergoemacs-comint-hook ()
  "Hook for `comint-mode-hook'."

  (define-key comint-mode-map (kbd "<f11>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<f12>") 'comint-next-input)
  (define-key comint-mode-map (kbd "S-<f11>") 'comint-previous-matching-input)
  (define-key comint-mode-map (kbd "S-<f12>") 'comint-next-matching-input)
  )

(defun ergoemacs-eshell-hook ()
  "Hook for `eshell-mode-hook', to redefine some ErgoEmacs keys so they are more useful."

  ;; Redefining ergoemacs-move-beginning-of-line-key to eshell-bol in eshell-mode-map
  ;; does not work, we have to use minor-mode-overriding-map-alist in this case
  (defvar ergoemacs-eshell-keymap (copy-keymap ergoemacs-keymap))

  (define-key ergoemacs-eshell-keymap ergoemacs-move-beginning-of-line-key 'eshell-bol)
  (define-key ergoemacs-eshell-keymap (kbd "<home>") 'eshell-bol)

  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-eshell-keymap))
  )

(defun ergoemacs-iswitchb-hook ()
  "Hooks for `iswitchb-minibuffer-setup-hook'."

  (defvar ergoemacs-iswitchb-keymap (copy-keymap ergoemacs-keymap))

  (define-key ergoemacs-iswitchb-keymap ergoemacs-isearch-backward-key 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap ergoemacs-isearch-forward-key 'iswitchb-next-match)

  (define-key ergoemacs-iswitchb-keymap (kbd "<f11>") 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "<f12>") 'iswitchb-next-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "S-<f11>") 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "S-<f12>") 'iswitchb-next-match)

  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-iswitchb-keymap))
  )

;; Adds/Removes all ErgoEmacs hooks
(defun ergoemacs-hook-modes ()
  (let ((modify-hook (if ergoemacs-mode 'add-hook 'remove-hook)))

    ;; when ergoemacs-mode is being activated
    (when ergoemacs-mode
      ;; TODO if ergoemacs-mode is nil we should restore global-keymap
      (ergoemacs-unbind-globals)

      (define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
      (define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
      (define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
      (define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element)
      )

    (funcall modify-hook 'isearch-mode-hook 'ergoemacs-isearch-hook)
    (funcall modify-hook 'comint-mode-hook 'ergoemacs-comint-hook)
    (funcall modify-hook 'eshell-mode-hook 'ergoemacs-eshell-hook)
    (funcall modify-hook 'iswitchb-minibuffer-setup-hook 'ergoemacs-iswitchb-hook)
    )
  )

;; ErgoEmacs minor mode
(define-minor-mode ergoemacs-mode
  "ErgoEmacs mode."
  nil
  :lighter " ErgoEmacs"	;; TODO this should be nil (it is for testing purposes)
  :global t
  :keymap ergoemacs-keymap

  (ergoemacs-hook-modes)
  )
