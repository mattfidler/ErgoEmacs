;;; xah-comment.el --- a mode for comment/uncomment source code. -*- coding: utf-8 -*-

;; Copyright © 2012, 2013 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Created: 2012
;; Keywords: comment TODO add proper keywords here

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2.

;;; DESCRIPTION

;; A mode for comment/uncomment source code.

;; The primary command is xc-comment-smart. It is similar to GNU emacs's builtin comment-dwim.
;; The difference is that xc-comment-smart will toggle comment/uncomment the current line, while comment-dwim will uncoment or add comment to end of line.
;; xc-comment-smart will add comment to end of line only if cursor is at end of line.

;; for download location and documentation, see:
;; coming TODO

;;; INSTALL

;; place this file in the dir 〔~/.emacs.d/〕. Create the 〔.emacs.d〕 folder if you don't have it.

;; Now, put the following lines in your emacs init file “.emacs”:

;; (require 'xah-comment )
;; (global-set-key (kbd "M-;") 'xc-comment-smart) ; or any key of your choice

;; Then, restart emacs.

;; Donation of $3 is appreciated. Paypal to 〔xah@xahlee.org〕

;;; HISTORY

;; v1.0, 2013-01-24 First working prototype. Currently only works for language that has a line syntax.

;;; TODO
;; in xc-comment-smart, when a region is selected, ignore blank lines in the beginning when determining whether to comment or uncomment


(require 'newcomment )

(defvar xc-use-comment-dwim-p nil "whether to use emacs's builtin comment-dwim 〔newcomment.el〕 commands.")

(setq xc-use-comment-dwim-p t )

(defvar xc-line-comment-syntax-table nil "a alist that maps major mode name to the lang's line comment beginning chars syntax.")

(setq xc-line-comment-syntax-table
      ;; first element is the value of `major-mode' variable. second element is a string
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
           (org-mode . "# ")

;; following is todo
 ;; (* Applescript, Mathematica, Pascal, OCaml *)
;;           (dos-mode . "#")

           (css-mode . nil)   ; /* … */

           (html-mode . nil)
           (sql-mode . "#") ;; mysql, 「#」 or 「-- 」 or 「/* … */」. postgresql, 「--」 or 「/*…*/」
           (sgml-mode . nil)
           (html6-mode . nil)
           (tuareg-mode . nil) ; ocaml (* … *)

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
  "Comment or uncomment the current line or text selection.

① If there's no text selection, comment/uncomment current line. But if cursor is at the end, add comment at the end when the line isn't already a comment.

② If there is a text selection, use the first line to determine whether to comment/uncomment the whole region."
  (interactive)
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
            (progn (xc-uncomment-line))

          (progn (let ()
                   (if (equal (point) (line-end-position)) ; if cursor is at end of line, comment at the end.
                       (progn (xc-comment-line "end"))
                     (progn (xc-comment-line)) ) )) )) )))

(defun xc-whole-line-is-comment-p ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at (concat "[ \t]*" xc-line-comment-marker))
    ))

(defun xc-comment-line ( &optional at-end-p)
  "Add line comment string to the beginning of current line.
If at-end-p is true, then add comment at end."
  (interactive "P")
  (if xc-use-comment-dwim-p
      (progn
        (let* ((p1 (line-beginning-position))
               (p2 (line-end-position))
               (inputStr (buffer-substring-no-properties p1 p2))
               )
          (if at-end-p
              (progn (comment-dwim nil))
            (progn
              (if (string-match "\\`[ \t\n]+\\'"  inputStr)
                  (progn (comment-dwim nil))
                (progn (comment-region (line-beginning-position) (line-end-position)
                ) ) ) ) ) ) )
    (progn
      (xc-set-line-comment-syntax)
      (beginning-of-line 1)
      (insert xc-line-comment-marker) ) ) )

(defun xc-uncomment-line ()
  "Remove line comment string (if any) in the beginning of current line."
  (interactive)
  (when (xc-whole-line-is-comment-p)
(if xc-use-comment-dwim-p
      (progn
        (uncomment-region (line-beginning-position) (line-end-position) )
        )
    (progn
  (xc-set-line-comment-syntax)
    (beginning-of-line 1)
    (search-forward xc-line-comment-marker)
    (delete-char -1)
       ) )
    ))

(defun xc-comment-region (p1 p2)
  "Add line comment string to the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (if xc-use-comment-dwim-p
        (progn
          (comment-region p1 p2 )
          )
      (progn
        (xc-set-line-comment-syntax)
        (save-excursion
          (goto-char p2)
          (while (>= (point) p1)
            (xc-comment-line)
            (previous-line) )) ) ) ))

(defun xc-uncomment-region (p1 p2)
  "Remove line comment string (if any) in the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (if xc-use-comment-dwim-p
        (progn
          (uncomment-region p1 p2 )
          )
      (progn
        (xc-set-line-comment-syntax)
        (save-excursion
          (goto-char p2)
          (while (>= (point) p1)
            (xc-uncomment-line)
            (previous-line) )) ) )
    ))

(provide 'xah-comment)

