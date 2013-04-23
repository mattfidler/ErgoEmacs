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

(defvar xem-emacs-words nil "a list of keywords more or less related to emacs system.")
(setq xem-emacs-words '(

"add-hook"
"autoload"
"backward-char"
"beginning-of-line"
"bounds-of-thing-at-point"
"buffer-file-name"
"buffer-modified-p"
"buffer-substring"
"buffer-substring-no-properties"
"called-interactively-p"
"completing-read"
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
"goto-char"  "insert-file-contents"
"insert"
"kbd"
"kill-buffer"
"line-beginning-position"
"line-end-position"
"local-set-key"
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
"point"
"point-max"
"point-min"
"re-search-backward"
"re-search-forward"
"read-directory-name"
"read-file-name"
"read-from-minibuffer"
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
"replace-regexp"
"replace-regexp-in-string"
"reverse"
"save-buffer"
"save-excursion"
"save-restriction"
"search-backward"
"search-backward-regexp"
"search-forward"
"search-forward-regexp"
"set-buffer"
"set-file-modes"
"set-mark"
"shell-command"
"skip-chars-backward"
"skip-chars-forward"
"split-string"
"string-match"
"string-match-p"
"string-to-number"
"stringp"
"substring"
"substring-no-properties"
"thing-at-point"
"widget-get"
"with-current-buffer"
"with-temp-buffer"
"with-temp-file"
"write-file"
"write-region"
"y-or-n-p"
"yes-or-no-p"


) )

(defvar xem-elisp-lang-words nil "a list of elisp keyword more or less related to elisp the language.")
(setq xem-elisp-lang-words '(

"add-to-list"
"and"
"append"
"apply"
"aref"
"aset"
"assoc"
"assq"
"boundp"
"car"
"catch"
"cdr"
"commandp"
"concat"
"cond"
"condition-case"
"cons"
"consp"
"defmacro"
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
"prin1"
"princ"
"print"
"progn"
"push"
"put"
"random"
"rassoc"
"require"
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

(defvar xem-keyword-builtin nil "a list of elisp  names")
(setq xem-keyword-builtin '( "&optional") )

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
            (emacsWords (regexp-opt xem-emacs-words 'symbols))
            (elispVars (regexp-opt xem-elisp-vars 'symbols))
            (elispBuiltin (regexp-opt xem-keyword-builtin 'symbols))
            (elispLangWords (regexp-opt xem-elisp-lang-words 'symbols))
            )
        `(
          (,emacsWords . font-lock-function-name-face)
          (,elispVars . font-lock-constant-face)
          (,elispBuiltin . font-lock-type-face)
          (,elispLangWords . font-lock-keyword-face)

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
        (modify-syntax-entry ?` "'   " synTable)
        (modify-syntax-entry ?' "'   " synTable)
        (modify-syntax-entry ?, "'   " synTable)
        (modify-syntax-entry ?@ "'   " synTable)

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
