;;; xah-comment.el --- comment . -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )

(defun xc-comment-dwim ()
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
          (if (wholeLineIsCmt-p)
              (xc-uncomment-region p1 p2)
            (xc-comment-region p1 p2)
            ))
      (progn
        (if (wholeLineIsCmt-p)
            (xc-uncomment-current-line)
          (xc-comment-current-line)
          )) )))

(defun wholeLineIsCmt-p ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*//")
    ))

(defun xc-comment-current-line ()
  (interactive)
  (beginning-of-line 1)
  (insert "//")
  )

(defun xc-uncomment-current-line ()
  "Remove “//” (if any) in the beginning of current line."
  (interactive)
  (when (wholeLineIsCmt-p)
    (beginning-of-line 1)
    (search-forward "//")
    (delete-backward-char 2)
    ))

(defun xc-comment-region (p1 p2)
  "Add “//” to the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (xc-comment-current-line)
        (previous-line)
        ))))

(defun xc-uncomment-region (p1 p2)
  "Remove “//” (if any) in the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (xc-uncomment-current-line)
        (previous-line) )) ))