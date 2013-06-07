; -*- coding: utf-8 -*-

;; UTF-8 as default encoding
(set-language-environment "UTF-8")


;;; dired, file, related

;; for ergoemacs-new-empty-buffer
(setq initial-major-mode (quote text-mode))

;; don't create backup~ or #auto-save# files
(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; make dired suggest target dir (for copy, move, …) that's in the other dired pane
(setq dired-dwim-target t)

;; make dired allow deleting/copy whole dir
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; Save minibuffer history
(savehist-mode 1)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
(setq-default save-place t)

(setq enable-recursive-minibuffers t )

;; used standard emacs dir
(setq bookmark-default-file (concat user-emacs-directory "bookmarks.el") )

;; apache per dir config file
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-unix-mode))


;;; Make emacs open all files in last emacs session.

;; This functionality is provided by desktop-save-mode (“feature” name: “desktop”). The mode is not on by default in emacs 23.1, and has a lot options. The following is init settings for the mode for ErgoEmacs.

;; Goal: have emacs always auto open the set of opened files in last session, even if emacs crashed in last session or the OS crashed in last session. Also, don't bother users by asking questions like “do you want to save desktop?” or “do you want to override last session file?”, because these are annoying and terms like “session” or “desktop” are confusing to most users because it can have many meanings.

;; Some tech detail: set the desktop session file 〔.emacs.desktop〕 at the variable “user-emacs-directory” (default value is “~/.emacs.d/”).  This file is our desktop file. It will be auto created and or over-written.  if a emacs expert has other desktop session files elsewhere, he can still use or manage those.

(require 'desktop)

(defun desktop-file-modtime-reset ()
  "Reset `desktop-file-modtime' so the user is not bothered."
  (interactive)
  (run-with-timer 5 nil
          (lambda ()
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save user-emacs-directory))))

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
    (add-hook 'find-file-hook 'desktop-file-modtime-reset)

    ;; Read default desktop
    (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook 'desktop-file-modtime-reset)
    )
  )

(defun hide-init-buffers ()
  "hide some buffers when emacs starts.
No splash screen. and If the *scratch* buffer is the current one, then create a new empty untitled buffer to hide *scratch*
"
  (interactive)
  (progn
    (setq inhibit-startup-screen t)
    (if (string= (buffer-name) "*scratch*")
        (ergoemacs-new-empty-buffer))
    ))

(add-hook 'after-init-hook 'desktop-settings-setup "APPEND")
(add-hook 'after-init-hook 'hide-init-buffers "APPEND")


;;; editing related

;; make cursor movement stop in between camelCase words.
(when (fboundp 'global-subword-mode ) (global-subword-mode 1))

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'expression)

(when (>= emacs-major-version 24)
;; typing opening brackets auto insert closing one
(electric-pair-mode 1)
;; setting for auto-close brackets for electric-pair-mode regardless of current major mode syntax table
(setq electric-pair-pairs '( (?\" . ?\") (?\{ . ?\}) ) )
 )

;; automatically copy text when mouse drag. Similar to Linux X11 behavior
(setq mouse-drag-copy-region t)

;; make 【Ctrl+c】 for copy, 【Ctrl+x】 for cut, etc.
(cua-mode 1)

;; Alt+y is not cua-repeat-replace-region
(define-key cua--cua-keys-keymap [(meta v)] 'nil)

;; make buffer switch command do suggestions
(ido-mode 1)

;; interactive name completion for describe-function, describe-variable, etc.
(icomplete-mode 1)

;; display line numbers at margin
(global-linum-mode 1)



;; (setq tab-always-indent 'complete)

;; majority of code formatting conventions do no recommend mixed tabs and spaces. So, here.
(setq-default indent-tabs-mode nil)     ; emacs 23.1 default is t

;; seems 4 is more popular than 8. Need more research.
(setq tab-width 4)   ; width for display tabs. emacs 23.1 default is 8


;;; org-mode

;; Make lines not dissapear into the right margin while in “org-mode”
(add-hook 'org-mode-hook 'soft-wrap-lines)

;; make “org-mode” syntax color code sections
(setq org-src-fontify-natively t)



;; when calling “list-colors-display”, make result sorted by hue.
(when (>= emacs-major-version 24) (setq list-colors-sort 'hsv ) )



;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)



;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )


;; make buffer names unique when files of the same name of diff dir are opened
(require 'uniquify) ; bundled with GNU emacs 23.2.1 or before
(setq uniquify-buffer-name-style 'forward)


;; 2011-07-29 yasnippet. Make the “yas/minor-mode”'s expansion behavior to take input word including hyphen.
(setq yas/key-syntaxes '("w_" "w_." "^ ")) ; default is '("w" "w_" "w_." "^ ") as of 2011-07-29


;; compile elisp files after save, do so only if there's exists a byte-compiled file
;; thanks to Adolfo Benedetti, 2011-07-15
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

;; auto compile elisp files after save.
;; (add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)) )



(progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-style (quote ( spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )) )


;; make the formfeed char display as a line
;; 2011-07-14 commented out due to a display problem with whitespace-mode
;; http://groups.google.com/group/gnu.emacs.help/browse_frm/thread/12e5a1e6a8b22c14/c642875edeb7ea20
(setq pp^L-^L-string "                                                           ")
;; (pretty-control-l-mode 1) ;; it has conflicts with “whitespace-mode” settings


;; For htmlize.el.
;; Rationale: use unicode whenever possible, since it's widely supported today.
(setq htmlize-convert-nonascii-to-entities nil) ; make htmlize generate unicode directly instead of html entities
(setq htmlize-html-charset "utf-8") ; make the output html use utf-8 charset



;; (progn
;; ;; use cperl-mode instead of perl-mode
;; ;; 2013-05-13 turn off for now. seems too hairy, colorin is better but doesn't color some simple variable 「$xxxx」.
;;   (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
;;   (add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))
;;   (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
;;   (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
;;   (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
;;   (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
;;   )


;; some syntax color setup

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 ;; '(diredp-ignored-file-name ((t (:foreground "#bebebe"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
 ;; '(isearch ((((class color) (min-colors 88) (background light)) (:background "black" :foreground "white"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2"))))
)
