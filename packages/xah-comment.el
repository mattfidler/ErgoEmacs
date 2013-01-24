;;; xah-comment.el --- comment . -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )

;; unfinished. 2013-01-19

(defvar xc-line-comment-syntax-table nil "a alist that maps major mode name to the lang's line comment beginning chars syntax.")

(setq xc-line-comment-syntax-table
      ;; mode name, string
'(

           (ahk-mode . ";")
           (c++-mode . "//")
           (c-mode . "//")
           (clojure-mode . ";")  ; (comment …)
           (cperl-mode . "#")
           (emacs-lisp-mode . ";")
           (fundamental-mode . "#")
           (haskell-mode . "--") ; block 「{-…-}」 
           (java-mode . "//")
           (js-mode . "//")
           (lisp-mode . ";")
           (perl-mode . "#")
           (php-mode . "#") ;; also 「//」. 「/*…*/」
           (pov-mode . "//")
           (powershell-mode . "#")
           (python-mode . "#")
           (ruby-mode . "#")
           (scala-mode . "//") ; /* */
           (scheme-mode . ";") ; #|…|#  #;sexp
           (sh-mode . "#")
           (shell-script-mode . "#")
           (visual-basic-mode . "'")
           (xlsl-mode . "//")

;; following is todo
 ;; (* Applescript, Mathematica, Pascal, OCaml *)
;;           (dos-mode . "#")

           (xbbcode-mode . "#")
           (css-mode . nil)   ; /* … */

           (html-mode . nil)
           (sql-mode . "#") ;; mysql, 「#」 or 「-- 」 or 「/* … */」. postgresql, 「--」 or 「/*…*/」
           (sgml-mode . nil)
           (html6-mode . nil)
           (tuareg-mode . nil) ; ocaml (* … *)
           (org-mode . "#")

           (shen-mode . "#")
           (snippet-mode . "#")
           ) )

(defvar xc-line-comment-marker nil "Current line comment string. e.g. # // ;")

(defun xc-set-line-comment-syntax ()
  "Sets `xc-line-comment-marker' and returns its value.
e.g. “#”, “//”, “;”."
  (let ()
    (setq xc-line-comment-marker (cdr (assoc major-mode xc-line-comment-syntax-table)) )
    xc-line-comment-marker
    ))

(defun xc-comment-smart ()
  "Comment or uncomment the current line or text selection."
  (interactive)
  ;; If there's no text selection, comment or uncomment the line depending whether the WHOLE line is a comment. If there is a text selection, using the first line to determine whether to comment/uncomment.
  (let (p1 p2)
    (xc-set-line-comment-syntax)
    (if (region-active-p)
        (save-excursion
          (setq p1 (region-beginning) p2 (region-end))
          (goto-char p1)
          (if (xc-whole-line-is-comment-p)
              (xc-uncomment-region p1 p2)
            (xc-comment-region p1 p2)
            ))
      (progn
        (if (xc-whole-line-is-comment-p)
            (xc-uncomment-current-line)
          (xc-comment-current-line)
          )) )))

(defun xc-whole-line-is-comment-p ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at (concat "[ \t]*" xc-line-comment-marker))
    ))

(defun xc-comment-current-line ()
  (interactive)
  (xc-set-line-comment-syntax)
  (beginning-of-line 1)
  (insert xc-line-comment-marker)
  )

(defun xc-uncomment-current-line ()
  "Remove line comment string (if any) in the beginning of current line."
  (interactive)
  (xc-set-line-comment-syntax)
  (when (xc-whole-line-is-comment-p)
    (beginning-of-line 1)
    (search-forward xc-line-comment-marker)
    (delete-char -1)
    ))

(defun xc-comment-region (p1 p2)
  "Add line comment string to the beginning of each line of selected text."
  (interactive "r")
  (xc-set-line-comment-syntax)
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (xc-comment-current-line)
        (previous-line)
        ))))

(defun xc-uncomment-region (p1 p2)
  "Remove line comment string (if any) in the beginning of each line of selected text."
  (interactive "r")
  (xc-set-line-comment-syntax)
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (xc-uncomment-current-line)
        (previous-line) )) ))

(provide 'xah-comment)

