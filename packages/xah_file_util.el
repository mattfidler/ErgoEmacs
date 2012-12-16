;;; xah_file_util.el --- xah's misc elisp utility similar to unix grep/sed. -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Created: 2012-04-02
;; Keywords: emacs lisp, utility, file

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; DESCRIPTION

;; this package is some misc emacs commands for find or find/replace on multiple files. It's similar in purpose to unix's {grep, sed}. It's entirely in emacs lisp.

;; The reason it's written because it avoids many problems when running emacs on Windows (e.g. no grep, lots problems interfacing with Windows ports of grep, unicode or encoding problems, emacs regex vs unix regex syntax mismatch for user, …, etc. See http://ergoemacs.org/emacs/emacs_grep_problem.html )

;; currently, this package is in alpha stage. I use it daily for months, but lots improvement in documentation and coding can be made.

;; This package exports the follow functions:
;; xah-find-text               → grep
;; xah-find-text-regex         → regex grep
;; xah-find-count              → grep count
;; xah-find-replace-text       → sed
;; xah-find-replace-text-regex → sed

;; Donation of $3 is appreciated. Paypal to 〔xah@xahlee.org〕

;;; INSTALL

;;; HISTORY

;; version 1.6.6, 2012-12-16 Now, the backup file's suffix is same for all backup files created during one command call. Before, each backup file has timestamp when the backup file is created.
;; version 1.6.5, 2012-12-08 improved the prompt for “xah-find-count” and also its output.
;; version 1.6.4, 2012-12-06 Backup file name now has this format: 「~‹x›~‹datetimestamp›~」 where ‹x› is 「t」 for plain text replace and 「r」 for regex replace. e.g. 「x.html~r~20121206_095642~」 Also, modified the prompt for 「xah-find-replace-text-regex」 so it is consistent with the function's argument.
;; version 1.6.3, 2012-11-30 fixed a bug: when one of the find or find/replace is called, and the temp output buffer already exits, the highlighting doesn't work. Now it does work.
;; version 1.6.2, 2012-11-29 trival change. Changed output file names to consistently start with “•” instead of some “◆”
;; version 1.6.1, 2012-11-20 improved the highlighting for xah-find-replace-text. It now highlighting the replaced text, instead of the find text.
;; version 1.6, 2012-08-12 added xah-find-count.
;; version 1.5, 2012-07-24 minor modification to the output format, made more consistent, added a utf-8 header.
;; version 1.4, 2012-07-21 added prompt for a dir on “xah-find-text” and all others.
;; version 1.3, 2012-07-19 added “xah-find-replace-text-regex”
;; version 1.2, 2012-07-14 added “xah-find-replace-text”
;; version 1.1, 2012-05-11 modified xah-find-text so that same line are not printed.
;; version 1.0, 2012-04-02 First version.


;;; Code:

(require 'find-lisp)
(require 'xeu_elisp_util)

(defvar xah-printContext-p nil "Whether to print context, for `xah-find-text', `xah-find-text-regex'.")
(setq xah-printContext-p t)


(defun xah-backup-suffix (ξss)
  "return a string of the form 「~‹ξss›~‹date-time-stamp›~」"
  (concat "~" ξss "~" (format-time-string "%Y%m%d_%H%M%S") "~"))

(defun xah-find-text (searchStr1 inputDir ξpathRegex )
  "Report how many occurances of a string, of a given dir.
Also print context.
TODO more/correct description here
Similar to grep, written in elisp.

case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change."

  (interactive
   (list
    (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    )
   )

  (let (
        (ξcount 0)
        (ξoutputBuffer "*xah-find-text output*")
        (textBlock "008991033174968")
        (textBlock-prev "092695046507792-random")
        )

    ;; add a ending slash if not there
    (when (not (string= "/" (substring inputDir -1) )) (setq inputDir (concat inputDir "/") ) )

    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-text result.
Search string 「%s」
Directory 「%s」
Path Regex 「%s」

" (current-date-time-string) searchStr1 inputDir ξpathRegex))
      (mapc
       (lambda (ξfp)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents ξfp)
           (setq case-fold-search case-fold-search)
           (while (search-forward searchStr1 nil "NOERROR if not found")
             (setq ξcount (1+ ξcount))
             (setq textBlock
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position) ) )
             (when (not (string= textBlock textBlock-prev))
               (princ (format "「%s」\n" textBlock)))
             (setq textBlock-prev textBlock))
           (when (> ξcount 0)
             (princ (format "• %d %s\n" ξcount ξfp))
             )
           )
         )
       (find-lisp-find-files inputDir ξpathRegex))

      (switch-to-buffer ξoutputBuffer)
      (hi-lock-mode 0)
      (funcall 'fundamental-mode)
      (highlight-phrase (regexp-quote searchStr1) (quote hi-yellow))
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      )
    ))


(defun xah-find-text-regex (searchRegex inputDir ξpathRegex )
  "Report how many occurances of a string, of a given dir.
Also print context.
TODO more/correct description here
Similar to grep, written in elisp.

Search case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change.
Replacement
"

  (interactive
   (list
    (read-string (format "Search regex (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    )
   )

  (let (
        (ξcount 0)
        (ξoutputBuffer "*xah-find-text-regex output*")
        (ξpos1 1) ; beginning of line
        (ξpos2 1)
        (ξpos-prev-end 1)
        )

    ;; add a ending slash if not there
    (when (not (string= "/" (substring inputDir -1) )) (setq inputDir (concat inputDir "/") ) )

    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-text-regex result.
Search regex 「%s」
Directory 「%s」
Path Regex 「%s」

" (current-date-time-string) searchRegex inputDir ξpathRegex))
      (mapc
       (lambda (ξfp)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents ξfp)
           (setq case-fold-search case-fold-search)
           (while (search-forward-regexp searchRegex nil t)
             (setq ξpos-prev-end ξpos2)
             (setq ξpos1 (- (match-beginning 0) 30))
             (setq ξpos2 (+ (match-end 0) 30))
             (setq ξpos1 (line-beginning-position))
             (setq ξpos2 (line-end-position))

             (if xah-printContext-p
                 (when (> (point) ξpos-prev-end)
                   (princ (format "「%s」\n" (buffer-substring-no-properties ξpos1 ξpos2 ))))
               (princ (format "「%s」\n" (match-string 0)))
               )
             (setq ξcount (1+ ξcount))
             )
           (when (> ξcount 0)
             (princ (format "• %d %s\n" ξcount ξfp))
             )
           )
         )
       (find-lisp-find-files inputDir ξpathRegex))

      (switch-to-buffer ξoutputBuffer)
      (hi-lock-mode 0)
      (funcall 'fundamental-mode)
      (highlight-phrase searchRegex (quote hi-yellow))
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      )
    ))


(defun xah-find-replace-text (ξsearchStr ξreplaceStr ξinputDir ξpathRegex )
  "Find/Replace string in all files of a directory.
SearchStr can span multiple lines.
This is case-literal. No automatic case conversion anywhere. No regex."
  (interactive
   (list
    (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (read-string (format "Replace string: ") nil 'query-replace-history)
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    )
   )

  (let (
        (ξoutputBuffer "*xah-find-replace-text output*")
        (backupSuffix (xah-backup-suffix "t"))
        )
    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-replace-text result.
Search string 「%s」
Replace string 『%s』
Directory 〔%s〕

" (current-date-time-string) ξsearchStr ξreplaceStr ξinputDir))
      (mapc
       (lambda (ξf)
         (let ( (case-fold-search nil)
                (ξcount 0)
                ξmatchStrFound ξmatchStrReplaced
                ξoldTextBlock
                ξnewTextBlock
                p3 p4 ; line begin, end
                (p3-old 0)  ; line begin, previous
                (ξnewTextBlock-previous "")
                )
           (with-temp-buffer
             (insert-file-contents ξf)
             (while (search-forward ξsearchStr nil t)
               (setq ξmatchStrFound (match-string 0) )
               (let (p1 p2 )
                 (setq p1 (match-beginning 0) )
                 (setq p2 (match-end 0) )
                 (goto-char p1)
                 (setq p3 (line-beginning-position))
                 (goto-char p2)
                 (setq p4 (line-end-position))
                 (setq ξoldTextBlock (buffer-substring-no-properties p3 p4) )
                 )

               (replace-match ξreplaceStr "fixedcase" "literalreplace")
               (setq ξmatchStrReplaced (match-string 0))
               (setq ξnewTextBlock (buffer-substring-no-properties (line-beginning-position) (line-end-position)) )

               (setq ξcount (1+ ξcount) )
               (when (not (= p3 p3-old))
                 (princ (format "▷%s\n" ξnewTextBlock)))
               (setq p3-old p3)
               )

             (when (> ξcount 0)
               (copy-file ξf (concat ξf backupSuffix) t)
               (write-region 1 (point-max) ξf)
               (princ (format "• %d %s\n" ξcount ξf))
               ) )
           ))

       (find-lisp-find-files ξinputDir ξpathRegex))
      (princ "Done.")
      )
    (switch-to-buffer ξoutputBuffer)
    (hi-lock-mode 0)
    (funcall 'fundamental-mode)
    (progn
      (when (not (string= ξreplaceStr ""))
        (highlight-phrase (regexp-quote ξreplaceStr) (quote hi-yellow))
        )
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      )
    )
  )

(defun xah-find-replace-text-regex (ξregex ξreplaceStr ξinputDir ξpathRegex ξwriteToFile-p ξcaseFoldSearch-p ξfixedCaseReplace-p)
  "Find/Replace by regex in all files of a directory.

ξregex is a regex pattern.
ξreplaceStr is replacement string.
ξinputDir is input directory to search (includes all nested subdirectories).
ξpathRegex is a regex to filter file paths.
ξwriteToFile-p, when true, write to file, else, print a report of changes only.
ξcaseFoldSearch-p sets `case-fold-search' for this operation.
ξfixedCaseReplace-p, if true, then the letter-case in replacement is literal. (this is relevant only if ξcaseFoldSearch-p is true.)
"
  (interactive
   (list
    (read-regexp "regex: " )
    (read-string (format "Replace string: ") nil 'query-replace-history)
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Write changes to file?")
    (y-or-n-p "Ignore letter case in search?")
    (y-or-n-p "Match case in replacement as you have it?")
    )
   )

  (let (
        (ξoutputBuffer "*xah-find-replace-text-regex output*")
        (backupSuffix (xah-backup-suffix "r"))
        )
    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-replace-text-regex result.
Search string 「%s」
Replace with 『%s』
Directory 〔%s〕

" (current-date-time-string) ξregex ξreplaceStr ξinputDir))
      (mapc
       (lambda (ξfp)
         (let (
                (ξcount 0)
                matchStrFound matchStrReplaced )

           (when t
             (with-temp-buffer
               (insert-file-contents ξfp)
               (setq case-fold-search ξcaseFoldSearch-p)
               (while (re-search-forward ξregex nil t)
                 (setq matchStrFound (match-string 0))
                 (replace-match ξreplaceStr ξfixedCaseReplace-p)
                 (setq matchStrReplaced (match-string 0))
                 (setq ξcount (1+ ξcount) )
                 (princ (format "「%s」\n" matchStrFound))
                 (princ (format "『%s』\n" matchStrReplaced))
                 )

               (when (> ξcount 0)
                 (when ξwriteToFile-p
                   (copy-file ξfp (concat ξfp backupSuffix) t)
                   (write-region 1 (point-max) ξfp)
                   )
                 (princ (format "• %d %s\n" ξcount ξfp))
                 ) )
             )

           ))

       (find-lisp-find-files ξinputDir "\\.html$"))
      (princ "Done ☺")
      )

    (switch-to-buffer ξoutputBuffer)
    (hi-lock-mode 0)
    (funcall 'fundamental-mode)
    (progn
      (when (not (string= ξreplaceStr ""))
        (highlight-phrase (regexp-quote ξregex) (quote hi-yellow))
        )
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      )
    )
  )

(defun xah-find-count (ξsearchStr ξcountExpr ξcountNumber ξinputDir ξpathRegex)
  "Report how many occurances of a string, of a given dir.
Similar to grep, written in elisp.

Case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change."
  (interactive
   (let* ( ξoperator)
     (list
      (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
      (setq ξoperator (read-string "Greater less equal unqual, any of < > <= >= = /=: " nil nil ">") )
      (read-string (format "Count %s: "  ξoperator) "0")
      (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
      (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
      ))
   )

  (let* (
         (outputBuffer "*xah-find-count output*")
         (countOperator
          (cond
           ((string-equal "<" ξcountExpr ) '<)
           ((string-equal "<=" ξcountExpr ) '<=)
           ((string-equal ">" ξcountExpr ) '>)
           ((string-equal ">=" ξcountExpr ) '>=)
           ((string-equal "=" ξcountExpr ) '=)
           ((string-equal "/=" ξcountExpr ) '/=)
           (t (error "your count expression 「%s」 is wrong!" ξcountExpr ))
           )
          )
         (countNumber (string-to-number ξcountNumber))
         )

    (with-output-to-temp-buffer outputBuffer
(princ (format "-*- coding: utf-8 -*-
Date: %s
Command “xah-find-count” result.
Search string: 「%s」
Count expression: 「%s %s」
Input dir: 「%s」
Path regex: 「%s」
" (current-date-time-string) ξsearchStr ξcountExpr ξcountNumber ξinputDir ξpathRegex))
      (mapc
       (lambda (ξf)
         (let ((ξcount 0)
               )
           (when t
             (with-temp-buffer
               (insert-file-contents ξf)
               (goto-char 1)
               (while (search-forward ξsearchStr nil "NOERROR if not found")
                 ;; (princ (format "「%s」\n" (buffer-substring-no-properties (line-beginning-position) (line-end-position) )))
                 (setq ξcount (1+ ξcount))
                 )

               ;; report if the occurance is not n times
               (when
                   (funcall countOperator ξcount countNumber)
                 (princ (format "• %d %s\n" ξcount ξf))
                 )
               )
             )

           )
         )
       (find-lisp-find-files ξinputDir "\\.html$"))
      (princ "Done deal!")
      )

    (switch-to-buffer outputBuffer)
    (hi-lock-mode 0)
    (funcall 'fundamental-mode)
    (highlight-phrase ξsearchStr (quote hi-yellow))
    (highlight-lines-matching-regexp "^• " (quote hi-pink))

    ))


(provide 'xah_file_util)
