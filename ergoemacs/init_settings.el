; -*- coding: utf-8 -*-

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; ----------------------------------------------------------------------------

;; For htmlize.el.
;; Rationale: use unicode whenever possible, since it's widely supported today.
(setq htmlize-convert-nonascii-to-entities nil) ; make htmlize generate unicode directly instead of html entities
(setq htmlize-html-charset "utf-8") ; make the output html use utf-8 charset 

;; ----------------------------------------------------------------------------

;; No backup or auto-save
(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; make cursor movement stop in between camelCase words.
(when (fboundp 'global-subword-mode ) (global-subword-mode 1))

;; Save minibuffer history
(savehist-mode 1)

;; Make lines not dissapear into the right margin while in org-mode
(add-hook 'org-mode-hook 'soft-wrap-lines)

;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
(setq-default save-place t)

;; ----------------------------------------------------------------------------
;; Make emacs open all files in last emacs session.
;;
;; This functionality is provided by desktop-save-mode (“feature”
;; name: “desktop”). The mode is not on by default in emacs 23.1, and
;; has a lot options. The following is init settings for the mode for
;; ErgoEmacs.
;;
;; Goal: have emacs always auto open the set of opend files in last
;; session, even if emacs crashed in last session or the OS crashed in
;; last session. Also, don't bother users by asking questions like “do
;; you want to save desktop?” or “do you want to override last session
;; file?”, because these are annoying and terms like “session” or
;; “desktop” are confusing to most users because it can have many
;; meanings.

;; Some tech detail: set the desktop session file 〔.emacs.desktop〕 at the variable
;; “user-emacs-directory” (default value is “~/.emacs.d/”).  This file
;; is our desktop file. It will be auto created and or over-written.
;; if a emacs expert has other desktop session files elsewhere, he can
;; still use or manage those.

(require 'desktop)

(defun desktop-settings-setup ()
  "Some settings setup for desktop-save-mode."
  (interactive)
  
  ;; At this point the desktop.el hook in after-init-hook was
  ;; executed, so (desktop-read) is avoided.
  (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
    ;; Here we activate the desktop mode
    (desktop-save-mode 1)

    ;; The default desktop is saved always
    (setq desktop-save t)

    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)

    ;; Set the location to save/load default desktop
    (setq desktop-dirname user-emacs-directory)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    (add-hook 'find-file-hook
     (lambda ()
       (run-with-timer 5 nil
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save user-emacs-directory)))))

    ;; Read default desktop
    (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook
              (lambda ()
                ;; Reset desktop modification time so the user is not bothered
                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
    )
  )

(add-hook 'after-init-hook
          'desktop-settings-setup
          (lambda ()
            ;; No splash screen
            (setq inhibit-startup-screen t)

            ;; If the *scratch* buffer is the current one, then create a new
            ;; empty untitled buffer to hide *scratch*
            (if (string= (buffer-name) "*scratch*")
                (new-empty-buffer))
            )
          t) ;; append this hook to the tail

;; ----------------------------------------------------------------------------

;; Lets user type y and n instead of the full yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; ----------------------------------------------------------------------------

(show-paren-mode 1)
(setq show-paren-style 'expression)

(setq dired-dwim-target t)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(setq mouse-drag-copy-region nil)

(delete-selection-mode 1)
(cua-mode 1)
(iswitchb-mode 1)

;; Alt+y is not cua-repeat-replace-region
(define-key cua--cua-keys-keymap [(meta v)] 'nil)

;; ----------------------------------------------------------------------------

(progn 
  ;; Make whitespace-mode with very basic background coloring for whitespaces
  (setq whitespace-style (quote ( spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and ▷ for tab.
  (setq whitespace-display-mappings
        '( 
          (space-mark 32 [183] [46]) ; normal space, MIDDLE DOT, FULL STOP.
          (space-mark 160 [164] [95]) 
          (space-mark 2208 [2212] [95]) 
          (space-mark 2336 [2340] [95]) 
          (space-mark 3616 [3620] [95]) 
          (space-mark 3872 [3876] [95]) 
          (newline-mark 10 [182 10]) ; newlne
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )) )

;; ----------------------------------------------------------------------------
;; make the formfeed char display as a line
;; (setq pp^L-^L-string "                                                           ")
;; (pretty-control-l-mode 1)

;; ----------------------------------------------------------------------------

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-ignored-file-name ((t (:foreground "#bebebe"))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:background "black" :foreground "white"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2")))))
