;-*- coding: utf-8 -*-
;; Copyright © 2007, 2008, 2009 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: qwerty, dvorak, keybinding, ergonomic

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; DESCRIPTION

;; This keybinding set puts the most frequently used emacs keyboard
;; shortcuts into the most easy-to-type spots.
;;
;; For complete detail, see: 
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

;;; INSTALL

;; Place this file in your favorite directory, such as “~/.emacs.d/”.
;; Then, place the following code in your emacs init file (the "~/.emacs"):
;; (load-file "~/emacs/ergonomic_keybinding_qwerty.el")
;; ; or 
;; (load-file "~/emacs/ergonomic_keybinding_dvorak.el")
;; Then, restart emacs.

;;; ACKNOWLEDGMENT
;; Thanks to Nikolaj Schumacher for his implementation of extend-selection.
;; Thanks to Andreas Politz and Nikolaj Schumacher for correcting/improving implementation of toggle-letter-case.
;; Thanks to Lennart Borgman for several suggestions on code to prevent shortcuts involving shift key to start select text when CUA-mode is on.
;; Thanks to David Capello for contribution to shrink-whitespaces.
;; Thanks to marciomazza for spotting several default bindings that should have been unbound.
;; Thanks to those who have created and improved the version for Colemak layout. They are (by date): “vockets”, “postivan”, Graham Poulter.
;; Thanks to lwarxx for bug report on diff-mode
;; Thanks to many users who send in comments and appreciations on this.
