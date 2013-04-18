;;; xah-php-mode.el --- Major mode for editing PHP code. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-04-18
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; Commentary:
;; Major mode for editing PHP code. Alpha stage.

;;; HISTORY

;; version 0.1, 2013-04-18 first version

(defvar xah-php-mode-hook nil "Standard hook for `xah-php-mode'")

(defvar xpm-php-kwds nil "a list of PHP lang keywords")
(setq xpm-php-kwds
'( "__halt_compiler" "abstract" "and" "array" "as" "break" "callable" "case" "catch" "class" "clone" "const" "continue" "declare" "default" "die" "do" "echo" "else" "elseif" "empty" "enddeclare" "endfor" "endforeach" "endif" "endswitch" "endwhile" "eval" "exit" "extends" "final" "for" "foreach" "function" "global" "goto" "if" "implements" "include" "include_once" "instanceof" "insteadof" "interface" "isset" "list" "namespace" "new" "or" "print" "private" "protected" "public" "require" "require_once" "return" "static" "switch" "throw" "trait" "try" "unset" "use" "var" "while" "xor" ) )

(defvar xpm-constant-kwds nil "a list of PHP lang constants")
(setq xpm-constant-kwds
'("__CLASS__" "__DIR__" "__FILE__" "__FUNCTION__" "__LINE__" "__METHOD__" "__NAMESPACE__" "__TRAIT__") )


;; syntax coloring related

(setq xpm-font-lock-keywords
      (let (
            (phpWords (regexp-opt xpm-php-kwds 'words))
            (phpConstants (regexp-opt xpm-constant-kwds 'words))
            )
        `(
;          (,phpWords . font-lock-function-name-face)
          (,phpWords . font-lock-keyword-face)
          (,phpConstants . font-lock-constant-face)
          ("$[A-Za-z]+" . font-lock-variable-name-face)
          ("$_[A-Z]+" . font-lock-constant-face)
          ("'[^']+'" . font-lock-string-face)
          ) ) )


;; keybinding

(defvar xpm-keymap nil "Keybinding for `xah-php-mode'")
(progn
  (setq xpm-keymap (make-sparse-keymap))
  (define-key xpm-keymap [remap comment-dwim] 'xpm-comment-dwim)
)


;; syntax table
(defvar xpm-syntax-table nil "Syntax table for `xah-php-mode'.")
(setq xpm-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?#   "< b" synTable) ; comment
        (modify-syntax-entry ?\n  "> b" synTable)
        (modify-syntax-entry ?\/ ". 12b" synTable) ; double slash comment
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))



;; define the mode
(define-derived-mode xah-php-mode fundamental-mode
  "ξphp "
  "A major mode for PHP.

PHP keywords are colored. Basically that's it.

\\{xpm-keymap}"
  (setq font-lock-defaults '((xpm-font-lock-keywords)))

  (set-syntax-table xpm-syntax-table)
  (use-local-map xpm-keymap)
  (run-mode-hooks 'xah-php-mode-hook)
)

(provide 'xah-php-mode)
