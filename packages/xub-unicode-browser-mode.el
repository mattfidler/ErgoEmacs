;-*- coding: utf-8 -*-
;; xub-unicode-browser-mode.el -- Major mode for browsing unicode characters.

;; Copyright © 2010 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: unicode, character map

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License version 3, as published by the
;; Free Software Foundation.

;;; DESCRIPTION

;; A major mode for browsing unicode characters

;;; INSTALL

;; Open the file, then type Alt+x eval-buffer.  You are done.  When
;; you need to browse a file full of unicode characters, just type M-x
;; xub-mode.

;; For more detail setting up automatic loading when emacs starts,
;; please see the doc at home page:
;; http://xahlee.org/emacs/unicode-browser.html

;;; DOCUMENTATION

;; When this mode is on, pressing the arrow keys will automatically
;; display character info of the char under cursor.
;; Mouse left click on a char also works.

;; Get unicode files to browse at: http://xahlee.org/emacs/unicode-browser.html

;; To see the inline documentation in emacs, type “C-h m”
;; (describe-mode). (if you have not load the mode, first type
;; Alt+x xub-mode)

;;; HISTORY

;; version 1.1, 2010-06-20 • Fixed first-mouse-click problem. (changed keybinding from "<down-mouse-1>" to "<mouse-1>"). • Added a menu. • Added commands xub-zoom-in xub-zoom-out.
;; version 1.0, 2010-06-20 First version.

;;; Code:

(setq xub-version "1.1")

(defvar xub-map nil "Keymap for xub")

(setq xub-map (make-sparse-keymap))
(define-key xub-map (kbd "<left>") 'xub-show-left)
(define-key xub-map (kbd "<right>") 'xub-show-right)
(define-key xub-map (kbd "<up>") 'xub-show-up)
(define-key xub-map (kbd "<down>") 'xub-show-down)
(define-key xub-map (kbd "<mouse-1>") 'xub-left-click)

(define-key xub-map (kbd "<M-f12>") 'xub-zoom-in)
(define-key xub-map (kbd "<M-f11>") 'xub-zoom-out)

(define-key xub-map [menu-bar] (make-sparse-keymap))
(let ((menuMap (make-sparse-keymap "XUB")))
    (define-key xub-map [menu-bar xub] (cons "XUB" menuMap))

    (define-key menuMap [about] '("About xub-mode" . xub-about))
    (define-key menuMap [zoom-out] '("Zoom out" . xub-zoom-out))
    (define-key menuMap [zoom-in] '("Zoom in" . xub-zoom-in))
)

(defun xub-about ()
  "Show the author, version number, and description about this package."
  (interactive)
  (with-output-to-temp-buffer "*About xub-mode*"
    (princ
     (concat "Mode name: xub-mode.\n\n"
             "Author: Xah Lee\n\n"
             "Version: " xub-version "\n\n"
             "To see inline documentation, type “Alt+x describe-mode” while you are in xub-mode.\n\n"
             "Home page: URL `http://xahlee.org/emacs/unicode-browser.html' \n\n")
     )
    )
  )

(defun xub-zoom-in ()
  "Make font size larger."
  (interactive)
  (text-scale-increase 1)
  )

(defun xub-zoom-out ()
  "Make font size smaller."
  (interactive)
  (text-scale-decrease 1)
  )

(defun xub-left-click ()
  "Show info about the character under cursor."
  (interactive)
  (describe-char (point))
  )

(defun xub-show-right ()
  "Move cursor forward then show info about the character under cursor."
  (interactive)
 (forward-char)
 (describe-char (point))
  )

(defun xub-show-left ()
  "Move cursor backward then show info about the character under cursor."
  (interactive)
 (backward-char)
 (describe-char (point))
  )

(defun xub-show-up ()
  "Move cursor up then show info about the character under cursor."
  (interactive)
 (previous-line)
 (describe-char (point))
  )

(defun xub-show-down ()
  "Move cursor down then show info about the character under cursor."
  (interactive)
 (next-line)
 (describe-char (point))
  )

(defun xub-mode ()
  "Major mode for browsing unicode characters.

When this mode is on, pressing the arrow keys will move
cursor and display info about the character under cursor.
Pressing mouse left button on a character also works.

The info will contain the character's unicode code point in
decimal, octal, hexadecimal, and its unicode name, unicode
category, font used, case class (lower/upper), etc.

You can get files unicode character files at:
  URL `http://xahlee.org/emacs/unicode-browser.html'

Tips:

In emacs 23.x, to insert a unicode by name or by hex code, call
`ucs-insert'. You can use the tab and * wildcard for name completion.

You need emacs 23.x to enjoy this mode. Because emacs 22's
`describe-char' does not provide full unicode info."
  (interactive)
;  (kill-all-local-variables)
  
  (setq major-mode 'xub)
  (setq mode-name "xub Char Browser")
  (use-local-map xub-map)
  
  (run-mode-hooks 'xub-hook))

(provide 'xub-unicode-browser-mode)
