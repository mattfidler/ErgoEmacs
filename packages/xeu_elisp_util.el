;-*- coding: utf-8 -*-
;; xeu_elisp_util.el -- xah's misc elisp utility

;; Copyright © 2011 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: emacs lisp, utility, file

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; DESCRIPTION

;; this package is some misc emacs lisp utility. The purpose is similar to “thing-at-point”.
;; It provides the following functions:

;; unit-at-cursor
;; get-selection-or-unit
;; get-image-dimensions
;; get-image-dimensions-imk
;; get-string-from-file
;; read-lines
;; get-html-file-title

;; For detailed discussion, see:
;; 〈Emacs Lisp: Using thing-at-point〉 http://xahlee.org/emacs/elisp_thing-at-point.html
;; 〈Emacs Lisp: get-selection-or-unit〉 http://xahlee.org/emacs/elisp_get-selection-or-unit.html

;;; INSTALL

;; Place the file in your emacs load path. Then
;; (require 'xeu_elisp_util)

;;; HISTORY

;; version 1.1, 2011-05-28. Added some comment in source code.
;; version 1.0, 2011-03-02. First version.


;;; Code:

(defun unit-at-cursor (unit)
  "Return the string and boundary of UNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

UNIT can be:
• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.
• 'glyphs — sequence of visible glyphs. Useful for file name, url, …, that doesn't have spaces in it.
• 'line — delimited by “\\n”.
• 'block — delimited by “\\n\\n” or beginning/end of buffer.
• 'buffer — whole buffer. (respects `narrow-to-region')
• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
 (setq bds (unit-at-cursor 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:
• this function returns the text and the 2 boundaries as a vector in one shot.
• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.
• 'word does not depend on syntax table.
• 'block does not depend on syntax table."
  (let (p1 p2)
    (save-excursion
        (cond
         ( (eq unit 'word)
           (let ((wordcharset "-A-Za-zÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
             (skip-chars-backward wordcharset)
             (setq p1 (point))
             (skip-chars-forward wordcharset)
             (setq p2 (point)))
           )

         ( (eq unit 'glyphs)
           (progn
             (skip-chars-backward "[:graph:]")
             (setq p1 (point))
             (skip-chars-forward "[:graph:]")
             (setq p2 (point)))
           )

         ( (eq unit 'buffer)
           (progn
             (setq p1 (point-min))
             (setq p2 (point-max))
             )
           )

         ((eq unit 'line)
          (progn
            (setq p1 (line-beginning-position))
            (setq p2 (line-end-position))))
         ((eq unit 'block)
          (progn
            (if (re-search-backward "\n\n" nil t)
                (progn (forward-char 2)
                       (setq p1 (point) ) )
              (setq p1 (line-beginning-position) )
              )

            (if (re-search-forward "\n\n" nil t)
                (progn (backward-char)
                       (setq p2 (point) ))
              (setq p2 (line-end-position) ) ) ))

         ((vectorp unit)
          (let (p0)
             (setq p0 (point))
             (skip-chars-backward (elt unit 0))
             (setq p1 (point))
             (goto-char p0)
             (skip-chars-forward (elt unit 1))
             (setq p2 (point))))
         ) )

    (vector (buffer-substring-no-properties p1 p2) p1 p2 )
    ) )

(defun get-selection-or-unit (unit)
  "Return the string and boundary of text selection or UNIT under cursor.

If `region-active-p' is true, then the region is the unit.  Else,
it depends on the UNIT. See `unit-at-cursor' for detail about
UNIT.

Returns a vector [text a b], where text is the string and a and b
are its boundary.

Example usage:
 (setq bds (get-selection-or-unit 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )"
  (interactive)

  (let (mytext p1 p2)
    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          (setq mytext (buffer-substring p1 p2) )
          (vector (buffer-substring-no-properties p1 p2) p1 p2 )
          )
      (unit-at-cursor unit)
 ) ) )

(defun get-image-dimensions (img-file-relative-path)
  "Returns a image file's width and height as a vector.
Support png jpg and any image type emacs supports.
 (Does not support gif.)
See also: `get-image-dimensions-imk'"
  (let (tmp dimen)
    (clear-image-cache)
    (setq tmp (create-image (concat default-directory img-file-relative-path)))
    (setq dimen (image-size tmp t))
    (vector (car dimen) (cdr dimen))
))

(defun get-image-dimensions-imk (img-file-path)
  "Returns a image file's width and height as a vector.
This function requires ImageMagick's “identity” shell command.
See also: `get-image-dimensions'."
  (let (cmd-name sh-output width height)
    (setq cmd-name "identify")
    (setq sh-output (shell-command-to-string (concat cmd-name " " img-file-path)))
    ;;  sample output from “identify”:  “xyz.png PNG 520x429+0+0 DirectClass 8-bit 9.1k 0.0u 0:01”
    (string-match "^[^ ]+ [^ ]+ \\([0-9]+\\)x\\([0-9]+\\)" sh-output)
    (setq width (match-string 1 sh-output))
    (setq height (match-string 2 sh-output))
    (vector (string-to-number width) (string-to-number height))))

(defun get-string-from-file (filePath)
  "Return FILEPATH's content."
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman <zzbba...@aol.com>”. 2010-09-02
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines (filePath) 
  "Return a list of lines of a file at FILEPATH." 
  (with-temp-buffer 
    (insert-file-contents filePath) 
    (split-string (buffer-string) "\n" t)))

(defun get-html-file-title (fname)
"Return FNAME <title> tag's text.
Assumes that the file contains the string
“<title>...</title>”."
 (let (x1 x2 linkText)

   (with-temp-buffer
     (goto-char 1)
     (insert-file-contents fname nil nil nil t)

     (setq x1 (search-forward "<title>"))
     (search-forward "</title>")
     (setq x2 (search-backward "<"))
     (buffer-substring-no-properties x1 x2)
     )
   ))

(provide 'xeu_elisp_util)
