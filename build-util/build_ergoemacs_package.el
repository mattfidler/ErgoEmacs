-*- coding: utf-8 -*-

; 2009-10-01
; run this file to build a ErgoEmacs package for installation on any emacs binary.

; steps:
; copy the whole dir into some dest dir.
; remove all .svn dirs.
; remove other files and dir such as Makefile and win32-setup etc.

(defvar sourceDir nil "The ergoemacs source code dir in repository.")
(setq sourceDir "../")

(defvar destDir nil "The output dir.")
(setq destDir "../ergoemacs_1.6.2/")

;; main
(make-directory destDir t)
(shell-command (concat "cp -R " sourceDir " " destDir) )

(shell-command (concat "find " destDir " -name \".svn\" -type d -exec rmdir {} ';'" ) )
