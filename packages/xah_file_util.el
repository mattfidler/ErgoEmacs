                                        ;-*- coding: utf-8 -*-
;; xah_file_util.el -- xah's misc elisp utility similar for unix grep/sed

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: emacs lisp, utility, file

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; DESCRIPTION

;; TODO this package is some misc emacs lisp utility.

;; Donation of $3 is appreciated. Paypal to 〔xah@xahlee.org〕

;;; INSTALL

;;; HISTORY

;; version 1.1, 2012-05-11 modified xah-find-text so that same line are not printed.
;; version 1.0, 2012-04-02 First version.


;;; Code:

(require 'find-lisp)

(defvar xah-printContext-p nil "Whether to print context, for `xah-find-text', `xah-find-text-regex'.")
(setq xah-printContext-p t)

(defun xah-find-text (searchStr1 inputDir pathRegex )
  "Report how many occurances of a string, of a given dir.
Also print context.
TODO more/correct description here
Similar to grep, written in elisp.

case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change."

  (interactive
   (list
    (read-string (format "Search string (default %s):" (current-word)) nil 'query-replace-history (current-word))
    default-directory
    (read-from-minibuffer "Path regex:" nil nil nil 'dired-regexp-history)
    )
   )

  (let (
        (ξcount 0)
        (outputBuffer "*xah-find-text*")
        (textBlock "008991033174968")
        (textBlock-prev "092695046507792-random")
        )

    ;; add a ending slash if not there
    (when (not (string= "/" (substring inputDir -1) )) (setq inputDir (concat inputDir "/") ) )

    (with-output-to-temp-buffer outputBuffer
      (princ (format "xah-find-text results.
Search string 「%s」
Directory 「%s」
Path Regex 「%s」

" searchStr1 inputDir pathRegex))
      (mapc
       (lambda (fPath)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents fPath)
           (setq case-fold-search case-fold-search)
           (while (search-forward searchStr1 nil "NOERROR if not found")
             (setq ξcount (1+ ξcount))
             (setq textBlock
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position) ) )
             (when (not (string= textBlock textBlock-prev))
               (princ (format "「%s」\n" textBlock)))
             (setq textBlock-prev textBlock))
           (when (> ξcount 0)
             (princ (format "• %d %s\n" ξcount fPath))
             )
           )
         )
       (find-lisp-find-files inputDir pathRegex))
      
      (switch-to-buffer outputBuffer)
      (highlight-phrase (regexp-quote searchStr1) (quote hi-yellow))
      (highlight-lines-matching-regexp "• " (quote hi-pink))
      )
    ))


(defun xah-find-text-regex (searchRegex inputDir pathRegex )
  "Report how many occurances of a string, of a given dir.
Also print context.
TODO more/correct description here
Similar to grep, written in elisp.

Search case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change.
Replacement 
"

  (interactive
   (list
    (read-string (format "Search regex (default %s):" (current-word)) nil 'query-replace-history (current-word))
    default-directory
    (read-from-minibuffer "Path regex:" nil nil nil 'dired-regexp-history)
    )
   )

  (let (
        (ξcount 0)
        (outputBuffer "*xah-find-text*")
        (ξpos1 1) ; beginning of line
        (ξpos2 1)
        (ξpos-prev-end 1)
        )

    ;; add a ending slash if not there
    (when (not (string= "/" (substring inputDir -1) )) (setq inputDir (concat inputDir "/") ) )

    (with-output-to-temp-buffer outputBuffer
      (princ (format "xah-find-text-regex results.
Search regex 「%s」
Directory 「%s」
Path Regex 「%s」

" searchRegex inputDir pathRegex))
      (mapc
       (lambda (fPath)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents fPath)
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
             (princ (format "• %d %s\n" ξcount fPath))
             )
           )
         )
       (find-lisp-find-files inputDir pathRegex))
      
      (switch-to-buffer outputBuffer)
      (highlight-phrase searchRegex (quote hi-yellow))
      (highlight-lines-matching-regexp "• " (quote hi-pink))
      )
    ))


(provide 'xah_file_util)
