;;; xah-elisp-mode.el --- Major mode for editing emacs lisp. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-03-23
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; Commentary:
;; Major mode for editing emacs lisp. Beta stage.

;;; HISTORY

;; version 0.1, 2013-03-23 first version

(defvar xah-elisp-mode-hook nil "Standard hook for `xah-elisp-mode'")

(defvar xem-elisp-kwds nil "a list of elisp function names")
(setq xem-elisp-kwds '( "add-hook"
"append"
"apply"
"autoload"
"backward-char"
"beginning-of-line"
"bounds-of-thing-at-point"
"buffer-file-name"
"buffer-modified-p"
"buffer-substring-no-properties"
"buffer-substring"
"called-interactively-p"
"consp"
"copy-directory"
"copy-file"
"current-buffer"
"custom-autoload"
"custom-set-faces"
"defalias"
"defconst"
"defcustom"
"define-key"
"defsubst"
"defvar"
"delete-char"
"delete-directory"
"delete-file"
"delete-region"
"directory-files"
"dolist"
"dotimes"
"end-of-line"
"error"
"expand-file-name"
"file-directory-p"
"file-exists-p"
"file-name-directory"
"file-name-extension"
"file-name-nondirectory"
"file-name-sans-extension"
"file-regular-p"
"file-relative-name"
"find-file"
"forward-char"
"forward-line"
"generate-new-buffer"
"global-set-key"
"global-unset-key"
"local-set-key"
"goto-char"  "insert-file-contents"
"insert"
"kbd"
"kill-buffer"
"line-beginning-position"
"line-end-position"
"looking-at"
"make-directory"
"make-local-variable"
"match-beginning"
"match-end"
"match-string"
"max"
"min"
"narrow-to-region"
"nth"
"null"
"number-to-string"
"point-max"
"point-min"
"point"
"read-directory-name"
"read-file-name"
"read-regexp"
"read-string"
"regexp-opt"
"regexp-quote"
"region-active-p"
"region-beginning"
"region-end"
"rename-file"
"repeat"
"replace-match"
"replace-regexp-in-string"
"replace-regexp"
"require"
"re-search-backward"
"re-search-forward"
"reverse"
"save-buffer"
"save-excursion"
"save-restriction"
"search-backward-regexp"
"search-backward"
"search-forward-regexp"
"search-forward"
"set-buffer"
"set-file-modes"
"set-mark"
"shell-command"
"skip-chars-backward"
"skip-chars-forward"
"split-string"
"string-match-p"
"string-match"
"stringp"
"string-to-number"
"substring-no-properties"
"substring"
"thing-at-point"
"widget-get"
"with-current-buffer"
"with-temp-buffer"
"with-temp-file"
"write-file"
"write-region"
"yes-or-no-p"
"y-or-n-p") )

(defvar xem-keyword-builtin nil "a list of elisp  names")
(setq xem-keyword-builtin '(
) )

(defvar xem-keyword-keyword nil "a list of elisp keyword names")
(setq xem-keyword-keyword '(
"prin1"
"push"
"put"
"add-to-list"
"random"
"rassoc"
"princ"
"print"
"progn"
"and"
"aref"
"aset"
"assoc"
"assq"
"boundp"
"car"
"catch"
"cdr"
"concat"
"cond"
"condition-case"
"cons"
"defun"
"elt"
"eq"
"equal"
"fboundp"
"format"
"funcall"
"function"
"get"
"if"
"defmacro"
"interactive"
"lambda"
"length"
"let"
"list"
"mapc"
"mapcar"
"mapconcat"
"member"
"memq"
"message"
"not"
"or"
"set"
"setq"
"string"
"string="
"throw"
"unless"
"vector"
"when"
"while"
) )

(defvar xem-elisp-vars nil "a list elisp variables names")
(setq xem-elisp-vars '(
"font-lock-builtin-face"
"font-lock-comment-delimiter-face"
"font-lock-comment-face"
"font-lock-constant-face"
"font-lock-doc-face"
"font-lock-function-name-face"
"font-lock-keyword-face"
"font-lock-negation-char-face"
"font-lock-preprocessor-face"
"font-lock-reference-face"
"font-lock-string-face"
"font-lock-syntactic-face-function"
"font-lock-type-face"
"font-lock-variable-name-face"
"font-lock-warning-face"
) )


;; syntax coloring related

(setq xem-font-lock-keywords
(let (
(elispWords (regexp-opt xem-elisp-kwds 'words))
(elispVars (regexp-opt xem-elisp-vars 'words))
(elispBuiltin (regexp-opt xem-keyword-builtin 'words))
(elispKeyword (regexp-opt xem-keyword-keyword 'words))
 )
`(
(,elispWords . font-lock-function-name-face)
(,elispVars . font-lock-constant-face)
(,elispBuiltin . font-lock-builtin-face)
(,elispKeyword . font-lock-keyword-face)

;font-lock-comment-delimiter-face
;font-lock-comment-face
;font-lock-doc-face
;font-lock-negation-char-face
;font-lock-preprocessor-face
;font-lock-reference-face
;font-lock-string-face
;font-lock-syntactic-face-function
;font-lock-type-face
;font-lock-variable-name-face
;font-lock-warning-face

) ) )


;; keybinding

(defvar xem-keymap nil "Keybinding for `xah-elisp-mode'")
(progn
  (setq xem-keymap (make-sparse-keymap))
  (define-key xem-keymap [remap comment-dwim] 'xem-comment-dwim)
)


;; syntax table
(defvar xem-syntax-table nil "Syntax table for `xah-elisp-mode'.")
(setq xem-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\; "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))



;; define the mode
(define-derived-mode xah-elisp-mode fundamental-mode
  "εlisp"
  "A simple major mode for emacs lisp.

elisp keywords are colored. Basically that's it.

\\{xem-keymap}"

  (emacs-lisp-mode)
  (setq font-lock-defaults '((xem-font-lock-keywords)))

  (set-syntax-table xem-syntax-table)
  (use-local-map xem-keymap)
  (run-mode-hooks 'xah-elisp-mode-hook)
)

(provide 'xah-elisp-mode)
