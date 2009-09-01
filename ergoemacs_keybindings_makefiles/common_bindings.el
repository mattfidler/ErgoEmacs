;-*- coding: utf-8 -*-

(setq mac-pass-command-to-system nil) ; so that Cmd+H won't activate Hide Current App and Cmd+Shift+q won't logout user.

(global-set-key (kbd "C-n") 'new-empty-buffer) ; Open New File
(global-set-key (kbd "C-S-n") 'make-frame-command) ; open a new window.
(global-set-key (kbd "C-o") 'find-file) ; Open
(global-set-key (kbd "C-w") 'close-current-buffer) ; Close
(global-set-key (kbd "C-s") 'save-buffer) ; Save
(global-set-key (kbd "C-S-s") 'write-file) ; Save As.
(global-set-key (kbd "C-p") 'print-buffer) ; Print
(global-set-key (kbd "C-a") 'mark-whole-buffer) ; Select All
(global-set-key (kbd "C-S-w") 'delete-frame) ; close Window.

(global-set-key (kbd "C-f") 'search-forward) ;; Find. (Note: perhaps in the future consider setting this to isearch-forward. Not sure now because isearch behavior is rather different then common Find command. For now, i think mapping Ctrl+f ease initial emacs using pain, but if user keep with emacs, isearch is far more useful. Also, should we add Ctrl+Shift+f for search-backword?. 2009-08-31 Xah Lee.) 

(global-set-key (kbd "<delete>") 'delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.

(global-set-key (kbd "M-~") 'switch-to-previous-frame)
(global-set-key (kbd "M-`") 'switch-to-next-frame)

(global-set-key (kbd "C-<prior>") 'previous-user-buffer)
(global-set-key (kbd "C-<next>") 'next-user-buffer)

(global-set-key (kbd "C-S-<prior>") 'previous-emacs-buffer)
(global-set-key (kbd "C-S-<next>") 'next-emacs-buffer)

(global-set-key (kbd "M-S-<prior>") 'backward-page)
(global-set-key (kbd "M-S-<next>") 'forward-page)

(global-set-key (kbd "M-5") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-!") 'delete-window)

(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-@") 'split-window-horizontally)

(global-set-key (kbd "M-8") 'extend-selection)
(global-set-key (kbd "M-*") 'select-text-in-quote)


(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-h m") 'describe-major-mode)

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

(add-hook 'w3m-mode-hook
 (lambda ()
  (define-key w3m-mode-map (kbd "<up>") 'previous-line) ; was w3m-previous-anchor. Use Shift+Tab.
  (define-key w3m-mode-map (kbd "<down>") 'next-line) ; was w3m-next-anchor. Use Tab.
  (define-key w3m-mode-map (kbd "<left>") 'backward-char) ; was w3m-view-previous-page. Use B.
  (define-key w3m-mode-map (kbd "<right>") 'forward-char) ; was w3m-view-this-url. Use Enter.
))

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-o") 'find-file) ; was dired-display-file
 ))

