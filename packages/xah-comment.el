;;; xah-comment.el --- comment . -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )

;; unfinished. 2013-01-19


(defvar xc-line-comment-syntax-table nil "a alist that maps major mode name to the lang's line comment beginning chars syntax.")

(setq xc-line-comment-syntax-table
      ;; mode name, string
'(

           ("emacs-lisp-mode" . ";")
           ("python-mode" . "#")
           ("ruby-mode" . "#")
           ("cperl-mode" . "#")
           ("perl-mode" . "#")
           ("shell-script-mode" . "#")
           ("sh-mode" . "#")
           ("php-mode" . "#")


           ("c-mode" . "//")
           ("c++-mode" . "//")
           ("java-mode" . "//")
           ("js-mode" . "//")

 ;; (* Applescript, Mathematica, Pascal, OCaml *)

           ("ahk-mode" . ";")

           ("fundamental-mode" . "#")

;           ("dos-mode" . "#")

           ("xbbcode-mode" . "#")
           ("lisp-mode" . "#")
           ("clojure-mode" . "#")
           ("css-mode" . "#")
           ("haskell-mode" . "#")
           ("html-mode" . "#")
           ("sql-mode" . "#")
           ("sgml-mode" . "#")
           ("html6-mode" . "#")
           ("xlsl-mode" . "#")
           ("tuareg-mode" . "#")
           ("org-mode" . "#")
           ("pov-mode" . "#")
           ("powershell-mode" . "#")

           ("shen-mode" . "#")
           ("scala-mode" . "#")
           ("scheme-mode" . "#")
           ("snippet-mode" . "#")
           ("visual-basic-mode" . "#")
           ("visual-basic-mode" . "#")
           ("fundamental-mode" . "#")
           ) )

(defvar xc-comment-line-begin-marker nil "e.g. # // ;")

(defun xc-get-current-mode-line-comment-syntax ()
  "Returns a string that's the char for line comment of current language.
e.g. “#”, “//”, “;”."
  (let ()
xc-line-comment-syntax-table    
  ))

(defun xc-comment-smart ()
  "Comment or uncomment the current line or text selection."
  (interactive)

  ;; If there's no text selection, comment or uncomment the line
  ;; depending whether the WHOLE line is a comment. If there is a text
  ;; selection, using the first line to determine whether to
  ;; comment/uncomment.
  (let (p1 p2)
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
    (looking-at "[ \t]*#")
    ))

(defun xc-comment-current-line ()
  (interactive)
  (beginning-of-line 1)
  (insert "#")
  )

(defun xc-uncomment-current-line ()
  "Remove “#” (if any) in the beginning of current line."
  (interactive)
  (when (xc-whole-line-is-comment-p)
    (beginning-of-line 1)
    (search-forward "#")
    (delete-backward-char 2)
    ))

(defun xc-comment-region (p1 p2)
  "Add “#” to the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (xc-comment-current-line)
        (previous-line)
        ))))

(defun xc-uncomment-region (p1 p2)
  "Remove “#” (if any) in the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (xc-uncomment-current-line)
        (previous-line) )) ))

(provide 'xc-comment)

