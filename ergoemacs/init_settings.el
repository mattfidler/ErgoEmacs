; -*- coding: utf-8 -*-

; load unicode data; used by describe-char and what-cursor-position for showing full unicode info
;; commented out because emacs 23 has this info, or vast majority of the info contained in this file.
;; (if (string= (substring-no-properties emacs-version 0 2) "23" )
;;     nil
;;   (setq describe-char-unicodedata-file
;;       (concat (file-name-directory
;;                (or load-file-name buffer-file-name)) "UnicodeData.txt" ))
;;   )

;; UTF-8 as default encoding

(set-language-environment "UTF-8")

;; for xml files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


;; For htmlize.el.
;; Rationale: use unicode whenever possible, since it's widely supported today.
(setq htmlize-convert-nonascii-to-entities nil) ; make htmlize generate unicode directly instead of html entities
(setq htmlize-html-charset "utf-8") ; make the output html use utf-8 charset 

;;;; no backup or auto-save

(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)

; save minibuffer history
(savehist-mode 1)

; make lines not dissapear into the right margin while in org-mode
(add-hook 'org-mode-hook 'soft-wrap-lines)

; The following code is necessary to reopen last opened files when
; emacs starts.
(require 'desktop)
(add-hook 'after-init-hook
 (lambda ()
   ;; No splash screen
   (setq inhibit-startup-screen t)

   ;; At this point the desktop.el hook in after-init-hook was
   ;; executed, so (desktop-read) is avoided.
   (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
     ;; Here we activate the desktop mode
     (desktop-save-mode 1)

     (setq desktop-save t)
     (setq desktop-dirname "~/.emacs.d/")

     ;; Make sure that even if emacs or PC crashed, emacs
     ;; still have last opened files.
     (add-hook 'find-file-hook
	       (lambda ()
		 (run-with-timer 5 nil 'desktop-save "~/.emacs.d/")))

     ;; Inhibit the splash screen if the desktop file exists
     (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
	 ;; Read the existent desktop
	 (desktop-read desktop-dirname)))

   ;; If the *scratch* buffer is the current one, let us create a new
   ;; empty untitled buffer to hide *scratch*
   (if (string= (buffer-name) "*scratch*")
       (new-empty-buffer))
   )
 t) ;; append this hook to the tail



(defalias 'yes-or-no-p 'y-or-n-p); Lets user type y and n instead of the full yes and no.

(show-paren-mode 1)
(setq show-paren-style 'expression)

(setq dired-dwim-target t)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(setq mouse-drag-copy-region nil)

(delete-selection-mode 1)
(cua-mode 1)
(iswitchb-mode 1)

;; alt+y is not cua-repeat-replace-region
(define-key cua--cua-keys-keymap [(meta v)] 'nil)

;; make whitespace-mode with very basic background coloring for whitespaces
(setq whitespace-style (quote ( spaces tabs newline space-mark tab-mark newline-mark )))

;; make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and ▷ for tab.
(setq
 whitespace-display-mappings
 '( 
   (space-mark 32 [183] [46]) ; normal space, MIDDLE DOT, FULL STOP.
   (space-mark 160 [164] [95]) 
   (space-mark 2208 [2212] [95]) 
   (space-mark 2336 [2340] [95]) 
   (space-mark 3616 [3620] [95]) 
   (space-mark 3872 [3876] [95]) 
   (newline-mark 10 [182 10]) ; newlne
   (tab-mark 9 [9655 9] [92 9]) ; tab
))


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(pp^L-highlight ((((type x w32 mac graphic) (class color)) (:underline "maroon"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2")))))
