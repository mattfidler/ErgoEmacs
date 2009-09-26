; -*- coding: utf-8 -*-

; load unicode data; used by describe-char and what-cursor-position for showing full unicode info
;; commented out because emacs 23 has this info, or vast majority of the info contained in this file.
;; (if (string= (substring-no-properties emacs-version 0 2) "23" )
;;     nil
;;   (setq describe-char-unicodedata-file
;;       (concat (file-name-directory
;;                (or load-file-name buffer-file-name)) "UnicodeData.txt" ))
;;   )


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

; turn on Open Recent menu under File menu
(require 'recentf)
(recentf-mode 1)

; reopen last opened files when emacs starts. Make sure that even if emacs or PC crashed, emacs still have last opened files.
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-dirname "~/.emacs.d/")
(add-hook 'find-file-hook (lambda () (run-with-timer 5 nil 'desktop-save "~/.emacs.d/")))

; save minibuffer history
(savehist-mode 1)

; make lines not dissapear into the right margin while in org-mode
(add-hook 'org-mode-hook 'soft-wrap-lines)


(defalias 'yes-or-no-p 'y-or-n-p); Lets user type y and n instead of the full yes and no.
(defalias 'center-line 'isearch-forward) ; center-line is bound to M-s. html mode and Text mode seems to redifine the M-s. Easier to just alias the center-line instead of hook on each mode.


(tool-bar-mode 0) ;; not sure we should have this on. The way it is right now, is rather useless for anyone who would use emacs, and i don't think it really provide any UI improvement because there's the menu already. The icons are rather very ugly. Possibly we can improve the icons, and or add a Close button to it.  
(show-paren-mode 1)
(setq show-paren-style 'expression)
(setq inhibit-startup-screen t)

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
