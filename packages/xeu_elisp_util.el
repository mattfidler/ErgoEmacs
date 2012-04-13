;-*- coding: utf-8 -*-
;; xeu_elisp_util.el -- xah's misc elisp utility

;; Copyright © 2011 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: emacs lisp, utility, file

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; DESCRIPTION

;; this package is some misc emacs lisp utility.
;; It provides the following functions:

;; unit-at-cursor
;; get-selection-or-unit
;; get-image-dimensions
;; get-image-dimensions-imk
;; get-string-from-file
;; read-lines
;; get-html-file-title
;; trim-string
;; asciify-text
;; title-case-string-region-or-line

;; The most used two are “unit-at-cursor” and “get-selection-or-unit”. They are intended as improvemnt of “thing-at-point”. For detailed discussion, see:〈Emacs Lisp: get-selection-or-unit〉 @ http://xahlee.org/emacs/elisp_get-selection-or-unit.html

;; This package requires 〔xfrp_find_replace_pairs.el〕, available at 
;; http://code.google.com/p/ergoemacs/source/browse/trunk/packages/xfrp_find_replace_pairs.el

;; Donation of $3 is appreciated. Paypal to 〔xah@xahlee.org〕

;;; INSTALL

;; Place the file in your emacs load path. Then
;; (require 'xfrp_find_replace_pairs)
;; (require 'xeu_elisp_util)

;;; HISTORY

;; version 1.4.9, 2012-03-15 more trivial improved implementation of “get-image-dimensions-imk”.
;; version 1.4.8, 2012-03-03 trivially improved implementation of “get-image-dimensions-imk”.
;; version 1.4.7, 2011-11-26 major change on “get-image-dimensions”. It now supports svn and gif. For gif, it calls “get-image-dimensions-imk”.
;; version 1.4.6, 2011-11-18 Added a “title-case-string-region-or-line”.
;; version 1.4.5, 2011-11-14 corrected a critical error in “asciify-text”.
;; version 1.4.4, 2011-11-14 added function “asciify-text”.
;; version 1.4.3, 2011-11-06 unit-at-cursor with 「'block」 argument will work when the text block is at beginning/end of buffer. Also, lines with just space or tab is also considered a empty line.
;; version 1.4.2, 2011-10-30 trivial implementation change on “get-html-file-title”. No user visible effect.
;; version 1.4.1, 2011-09-29 fixed a error in “trim-string”.
;; version 1.4, 2011-09-16 added “trim-string”.
;; version 1.3, 2011-08-27 fixed a bug in “unit-at-cursor” when argument is 「'block」. Now it doesn't grab a extra line ending.
;; version 1.2, 2011-07-02 inline doc improvement for “get-image-dimensions” “get-image-dimensions-imk”.
;; version 1.1, 2011-05-28 Added some comment in source code.
;; version 1.0, 2011-03-02 First version.


;;; Code:

(defun unit-at-cursor (unit)
  "Return the string and boundary of UNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

UNIT can be:

• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.

• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, anything doesn't have white spaces in it.

• 'line — delimited by “\\n”. (captured text does not include a ending “\\n”.)

• 'block — delimited by empty lines or beginning/end of buffer. Lines with just spaces or tabs are also considered empty line. (captured text does not include a ending “\\n”.)

• 'buffer — whole buffer. (respects `narrow-to-region')

• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
 (setq bds (unit-at-cursor 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:

• This function returns the text and the 2 boundaries as a vector in one shot.

• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.

• This function's behavior does not depend on syntax table. e.g. for units 「'word」, 「'block」, etc."
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
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq p1 (point) ) )
              (setq p1 (point) )
              )
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq p2 (point) ))
              (setq p2 (point) ) ) ))

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

  (let (myText p1 p2)
    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          (setq myText (buffer-substring p1 p2) )
          (vector (buffer-substring-no-properties p1 p2) p1 p2 )
          )
      (unit-at-cursor unit)
 ) ) )



(defun get-image-dimensions (ξfile-path)
  "Returns a image file's width and height as a vector.
Support png jpg svg gif and any image type emacs supports.
 (for gif, it calls `get-image-dimensions-imk')
Bug: for large size png, sometimes this returns a wrong dimension 30×30."
  (let (ξx ξy)
    (cond
     ((string-match "\.gif$" ξfile-path) (get-image-dimensions-imk ξfile-path))
     ((string-match "\.svg$" ξfile-path) 
      (with-temp-buffer
        (insert-file-contents ξfile-path)
        (goto-char (point-min))
        (search-forward-regexp "width=\"\\([0-9]+\\).*\"")
        (setq ξx (match-string 1 ))
        (goto-char (point-min))
        (search-forward-regexp "height=\"\\([0-9]+\\).*\"")
        (setq ξy (match-string 1 ))
        (vector (string-to-number ξx) (string-to-number ξy))
        ))
     (t (let (ξxy )
          (progn 
            (clear-image-cache t)
            (setq ξxy (image-size
                       (create-image
                        (if (file-name-absolute-p ξfile-path)
                            ξfile-path
                          (concat default-directory ξfile-path) ))
                       t))
            )
          (vector (car ξxy) (cdr ξxy)) )
        ) ) ))

;; (defun get-image-dimensions-imk (img-file-path)
;;   "Returns a image file's width and height as a vector.
;; This function requires ImageMagick's “identify” shell command.
;; See also: `get-image-dimensions'."
;;   (let (cmd-name sh-output width height)
;;     (setq cmd-name "identify")
;;     (setq sh-output (shell-command-to-string (concat cmd-name " " img-file-path)))
;;     ;;  sample output from “identify”:  “xyz.png PNG 520x429+0+0 DirectClass 8-bit 9.1k 0.0u 0:01”
;;     (string-match "^[^ ]+ [^ ]+ \\([0-9]+\\)x\\([0-9]+\\)" sh-output)
;;     (setq width (match-string 1 sh-output))
;;     (setq height (match-string 2 sh-output))
;;     (vector (string-to-number width) (string-to-number height))))

(defun get-image-dimensions-imk (img-file-path)
  "Returns a image file's width and height as a vector.
This function requires ImageMagick's “identify” shell command.
See also: `get-image-dimensions'."
  (let ( widthHeightList )
    (setq widthHeightList (split-string (shell-command-to-string (concat "identify -format \"%w %h\" " img-file-path))) )
    (vector
     (string-to-number (elt widthHeightList 0))
     (string-to-number (elt widthHeightList 1)) ) ))


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


(defun get-html-file-title (fName)
  "Return FNAME <title> tag's text.
Assumes that the file contains the string
“<title>…</title>”."
  (with-temp-buffer
      (insert-file-contents fName nil nil nil t)
      (goto-char 1)
      (buffer-substring-no-properties
       (search-forward "<title>") (- (search-forward "</title>") 8))
      ))


(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
(replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
)
(defun asciify-text (ξstring &optional ξfrom ξto)
"Change some Unicode characters into equivalent ASCII ones.
For example, “passé” becomes “passe”.

This function works on chars in European languages, and does not transcode arbitrary unicode chars (such as Greek).  Un-transformed unicode char remains in the string.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξfrom is nil, returns a changed string, else, change text in the region between positions ξfrom ξto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (car bds) (cdr bds)) ) ) )

  (require 'xfrp_find_replace_pairs)

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξfrom nil t))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let ((case-fold-search t))
            (replace-regexp-pairs-in-string inputStr
     [
 ["á\\|à\\|â\\|ä\\|ã\\|å" "a"]
 ["é\\|è\\|ê\\|ë" "e"]
 ["í\\|ì\\|î\\|ï" "i"]
 ["ó\\|ò\\|ô\\|ö\\|õ\\|ø" "o"]
 ["ú\\|ù\\|û\\|ü" "u"]
 ["ñ" "n"]
 ["ç" "c"]
 ["ð" "d"]
 ["þ" "th"]
 ["ß" "ss"]
 ["æ" "ae"]
      ]
     ) )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun title-case-string-region-or-line (ξstring &optional ξregion-boundary)
  "Capitalize the current line or text selection, following title conventions.

Capitalize first letter of each word, except words like {to, of,
the, a, in, or, and, …}. If a word already contains cap letters
such as HTTP, URL, they are left as is.

When called in a elisp program, if ΞREGION-BOUNDARY is nil,
returns the changed ΞSTRING, else, work on the region.
ΞREGION-BOUNDARY is a pair [from to], it can be a vector or
list."
  (interactive
   (let ((bds (get-selection-or-unit 'line)))
     (list nil (vector (elt bds 1) (elt bds 2)) ) ) )

  (let ( replacePairs
         (workOnStringP (if ξregion-boundary nil t ) )
         (p1 (elt ξregion-boundary 0))
         (p2 (elt ξregion-boundary 1))
         )
    
    (setq replacePairs '(
                         [" A " " a "]
                         [" And " " and "]
                         [" At " " at "]
                         [" As " " as "]
                         [" By " " by "]
                         [" Be " " be "]
                         [" Into " " into "]
                         [" In " " in "]
                         [" Is " " is "]
                         [" It " " it "]
                         [" For " " for "]
                         [" Of " " of "]
                         [" Or " " or "]
                         [" On " " on "]
                         [" The " " the "]
                         [" That " " that "]
                         [" To " " to "]
                         [" Vs " " vs "]
                         [" With " " with "]
                         [" From " " from "]
                         ))

    (let ((case-fold-search nil))
      (if workOnStringP
          (progn 
            (replace-pairs-in-string-recursive (upcase-initials ξstring) replacePairs)
            )
        (progn 
          (save-restriction 
            (narrow-to-region p1 p2)
            (upcase-initials-region (point-min) (point-max) )
            (replace-regexp-pairs-region (point-min) (point-max) replacePairs t t)
            ) ) ) ) ) )

(provide 'xeu_elisp_util)
