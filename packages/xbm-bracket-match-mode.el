;-*- coding: utf-8 -*-
;; xbm-bracket-match-mode.el -- Major mode for editing bbcode.

;; Copyright © 2009 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: bracket, quote

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License version 3, as published by the
;; Free Software Foundation.

;;; DESCRIPTION

;; A major mode for highlighting brackets.
;; for download location and documentation, see:
;; http://xahlee.org/emacs/xbbcode-mode.html

;;; INSTALL

;; Open the file, then type “Alt+x eval-buffer”. You are done. When
;; you need to work in bbcode, just type “Alt+x xbbcode-mode”,
;; you'll see bbcode tags colored.

;; To have emacs automatically load the file when it restarts, and
;; automatically load the mode when opening files whose name ends in
;; “.bbcode”, do the following:

;; Place this file in the directory 〔~/.emacs.d/〕. For example:

;; on Windows
;; C:\Users\mary\.emacs.d\xbbcode-mode.el

;; on Mac OS X
;; /Users/mary/.emacs.d\xbbcode-mode.el

;; then, put the following lines in your emacs init file 〔~/.emacs〕:

;; (add-to-list 'load-path "~/.emacs.d/") ;; add a dir to load path
;; (autoload 'xbmm "xbm-bracket-match-mode" "Load xbmm for highlighting matching pairs." t)

;; Then, restart emacs.

;;; DOCUMENTATION

;; Full documentation is at: TODO

;; To see the inline documentation in emacs, type “C-h m”
;; (describe-mode). (if you have not load the mode, first type
;; Alt+x xbmm)

;;; HISTORY

;; version 1.0, 2010-08-29 First version.

;;; Code:

(setq xbmm-version "1.0")

(defgroup xbmm nil
  "Major mode for editing bbcode."
  :group 'languages)

(defvar xbmm-hook nil "Standard hook for xbmm.")

(defvar xbmm-version nil "xbmm version string.")

(defvar xbmm-map nil "Keymap for xbmm")

(defface xbmm-curly-double-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight AHK command names."
  :group 'languages)

;;; font-lock

(setq xbmm-font-lock-keywords
      '(
        ("〔[^〕]+?〕" . font-lock-type-face)
        ("〈[^〉]+?〉" . font-lock-constant-face)
        ("《[^》]+?》" . font-lock-builtin-face)
        ("【[^】]+?】" . font-lock-function-name-face)
        ("『[^』]+?』" . font-lock-keyword-face)
        ("「[^」]+?」" . font-lock-constant-face)
        ("‹[^›]+?›" . font-lock-constant-face)
        ("«[^»]+?»" . font-lock-constant-face)
        ("“[^”]+?”" . xbmm-curly-double-face)
        ("‘[^’]+?’" . font-lock-constant-face)
        )
      )

(defun xbmm-mode ()
  "Major mode for coloring matching pairs."
  (interactive)
  (kill-all-local-variables)
  
  (setq major-mode 'xbmm)
  (setq mode-name "xBracket Match")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((xbmm-font-lock-keywords) nil nil))

  (run-mode-hooks 'xbmm-hook))

(provide 'xbmm)
