;;; ergoemacs-mode.el --- A minor mode, a keybinding set based on ergonomics. -*- coding: utf-8 -*-

;; Copyright © 2007, 2008, 2009 by Xah Lee
;; Copyright © 2009, 2010 by David Capello
;; Copyright © 2012 by Matthew Fidler

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;;     David Capello <davidcapello@gmail.com>  ( http://www.davidcapello.com.ar/ )
;;     Matthew Fidler <matthew.fidler@gmail.com> ( http://github.com/mlf176f2/ )
;; Maintainer: Matthew Fidler, Xah Lee, David Capello
;; Created: August 01 2007
;; Version: 5.7.3
;; Keywords: convenience, qwerty, dvorak, keybinding, ergonomic, colemak

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; Commentary:
;; This keybinding set puts the most frequently used emacs keyboard
;; shortcuts into the most easy-to-type spots.
;;
;; For complete detail, see:
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
;; Also see the file "_README.txt"
;;
;; Install:
;; See the file “_INSTALL.txt”.

;;; Change Log:
;; See the file “_HISTORY.txt”.

;;; Acknowledgment:
;; Thanks to Andrey Kotlarski (aka m00naticus) for a patch on 2012-12-08
;; Thanks to Nikolaj Schumacher for his implementation of extend-selection.
;; Thanks to Andreas Politz and Nikolaj Schumacher for correcting/improving implementation of toggle-letter-case.
;; Thanks to Lennart Borgman for several suggestions on code to prevent shortcuts involving shift key to start select text when CUA-mode is on.
;; Thanks to marciomazza for spotting several default bindings that should have been unbound.
;; Thanks to lwarxx for bug report on diff-mode
;; Thanks to maddin for ergoemacs-global/local-set-key functions and ergoemacs-hook-modes improvements.
;; Thanks to many users who send in comments and appreciations on this.
;; Layout contributors:
;; ergoemacs-layout-da.el Contributor: Michael Budde
;; ergoemacs-layout-dv.el Contributor: Xah Lee, David Capello
;; ergoemacs-layout-gb-dv.el Contributor: Phillip Wood
;; ergoemacs-layout-gb.el Contributor: Jorge Dias (aka theturingmachine)
;; ergoemacs-layout-fr.el Contributor: Alexander Doe
;; ergoemacs-layout-it.el Contributor: David Capello, Francesco Biccari


;; Ergoemacs-keybindings version
(defconst ergoemacs-mode-version "5.7.3"
  "Ergoemacs-keybindings minor mode version number.")

(defgroup ergoemacs-mode nil
  "Emulate CUA key bindings including C-x and C-c."
  :group 'editing-basics
  :group 'convenience
  :group 'emulations)

(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs Directory")

;; Include extra files
(add-to-list 'load-path  ergoemacs-dir)

(load "functions")
(load "ergoemacs-unbind")

(defvar ergoemacs-layout-da
  '("" "½" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "+" "’" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "á" "\"" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "æ" "ø" "'" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "§" "!" "@" "#" "¤" "%" "&" "/" "(" ")" "=" "?" "`" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "Á" "^" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Æ" "Ø" "*" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Danish Layout")

(defvar ergoemacs-layout-pt-nativo
  '("" "+" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "º" "<" ""
    "" ""  "'" "," "." "h" "x" "w" "l" "t" "c" "p" "~" "-" "" 
    "" ""  "i" "e" "a" "o" "u" "m" "d" "s" "r" "n" "'" "|" ""
    "" "«"  "y" "ç" "j" "b" "k" "q" "v" "g" "f" "z" "" "" ""
    ;; Shifted
    "" "*" "!" "\"" "#" "$" "%" "&" "/" "(" ")" "=" "ª" ">" ""
    "" ""  "?" ";" ":" "H" "X" "W" "L" "T" "C" "P" "^" "_" "" 
    "" ""  "I" "E" "A" "O" "U" "M" "D" "S" "R" "N" "`" "\\" ""
    "" "»"  "Y" "Ç" "J" "B" "K" "Q" "V" "G" "F" "Z" "" "" "")
  "PT Nativo `http://xahlee.info/kbd/pt-nativo_keyboard_layout.html'")

(defvar ergoemacs-layout-us
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
  "US Engilsh QWERTY Keyboard")

(defvar ergoemacs-layout-dv
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "[" "]" ""
    "" ""  "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" ""  ""
    "" ""  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" ""  ""  ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "{" "}"  ""
    "" ""  "\"" "," "." "P" "Y" "F" "G" "C" "R" "L" "?" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Dvorak Keyboard")

(defvaralias 'ergoemacs-layout-us_dvorak 'ergoemacs-layout-dv)


(defvar ergoemacs-layout-programmer-dv
  '("" "$" "&" "[" "{" "}" "(" "=" "*" ")" "+" "]" "!" "#" ""
    "" ""  "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "" ""
    "" ""  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" "" "" ""
    ;; Shifted
    "" "" "%" "7" "5" "3" "1" "9" "0" "2" "4" "6" "8" "`"  ""
    "" ""  "\"" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "?" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Programmer Dvorak")

(defvar ergoemacs-layout-gb-dv
  '("" "`" "[" "7" "5" "3" "1" "9" "0" "2" "4" "6" "8" "]"  ""
    "" ""  "/" "," "." "p" "y" "f" "g" "c" "r" "l" "'" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "#" ""
    "" "\\"  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" "" "" ""
    ;; Shifted
    "" "¬" "{" "&" "%" "£" "!" "(" ")" "\"" "$" "^" "*" "}" ""
    "" ""  "?" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "@" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "~" ""
    "" "|"  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "UK Dvorak Keyboard")

(defvar ergoemacs-layout-colemak
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "f" "p" "g" "j" "l" "u" "y" ";" "[" "]" "\\"
    "" ""  "a" "r" "s" "t" "d" "h" "n" "e" "i" "o" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "k" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "F" "P" "G" "J" "L" "U" "Y" ":" "{" "}" "|"
    "" ""  "A" "R" "S" "T" "D" "H" "N" "E" "I" "O" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "K" "M" "<" ">" "?" "" "" "")
  "Ergonomic US Colemak Keyboard URL `http://colemak.com/'")

(defvar ergoemacs-layout-asset
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "j" "f" "g" "y" "p" "u" "l" ";" "[" "]" "\\"
    "" ""  "a" "s" "e" "t" "d" "h" "n" "i" "o" "r" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "k" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "J" "F" "G" "Y" "P" "U" "L" ":" "{" "}" "|"
    "" ""  "A" "S" "E" "T" "D" "H" "N" "I" "O" "R" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "K" "M" "<" ">" "?" "" "" "")
  "US Asset Keyboard")

(defvar ergoemacs-layout-workman
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "d" "r" "w" "b" "j" "f" "u" "p" ";" "[" "]" "\\"
    "" ""  "a" "s" "h" "t" "g" "y" "n" "e" "o" "i" "'" "" ""
    "" ""  "z" "x" "m" "c" "v" "k" "l" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "D" "R" "W" "B" "J" "F" "U" "P" ":" "{" "}" "|"
    "" ""  "A" "S" "H" "T" "G" "Y" "N" "E" "O" "I" "\"" "" ""
    "" ""  "Z" "X" "M" "C" "V" "K" "L" "<" ">" "?" "" "" "")
  "US Workman Layout")

(defvar ergoemacs-layout-gb
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "#" ""
    "" "\\"  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "¬" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "@" "~" ""
    "" "|"  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
  "UK QWERTY")


(defvar ergoemacs-layout-it
  '("" "\\" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "'" "¡" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "è" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ò" "à" "ù" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "|" "!" "\"" "£" "$" "%" "&" "/" "(" ")" "=" "?" "^" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "é" "+" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "ç" "°" "§" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Italian QWERTY")

(defvar ergoemacs-layout-sp
  '("" "°" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "'" "¡" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "`" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ñ" "'" "ç" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "ª" "!" "\"" "£" "$" "%" "&" "/" "(" ")" "=" "?" "¿" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "^" "*" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ñ" "\"" "Ç" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Spanish Layout")

(defvar ergoemacs-layout-fr
  '("" "²" "&" "é" "\"" "'" "(" "-" "è" "_" "ç" "à" ")" "=" ""
    "" ""  "a" "z" "e" "r" "t" "y" "u" "i" "o" "p" "^" "$" ""
    "" ""  "q" "s" "d" "f" "g" "h" "j" "k" "l" "m" "ù" "*" ""
    "" "<"  "w" "x" "c" "v" "b" "n" "," ";" ":" "!" "" "" ""
    ;; Shifted
    "" "³" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "°" "+" ""
    "" ""  "A" "Z" "E" "R" "T" "Y" "U" "I" "O" "P" "" "£" ""
    "" ""  "Q" "S" "D" "F" "G" "H" "J" "K" "L" "M" "%" "μ" ""
    "" ">"  "W" "X" "C" "V" "B" "N" "?" "." "/" "§" "" "" "")
  "French AZERTY keyboard")

(defun ergoemacs-get-layouts-type ()
  "Gets the customization types for `ergoemacs-keyboard-layout'"
  `(choice ,@(mapcar
              (lambda(elt)
                `(const :tag ,elt :value ,elt))
              (sort (ergoemacs-get-layouts) 'string<))))

(defun ergoemacs-get-layouts-doc ()
  "Gets the list of all known layouts and the documentation associated with the layouts."
  (let ((lays (sort (ergoemacs-get-layouts) 'string<)))
    (mapconcat
     (lambda(lay)
       (let* ((variable (intern (concat "ergoemacs-layout-" lay)))
              (alias (condition-case nil
                         (indirect-variable variable)
                       (error variable)))
              (is-alias nil)
              (doc nil))
         (setq doc (or (documentation-property variable 'variable-documentation)
                       (progn
                         (setq is-alias t)
                         (documentation-property alias 'variable-documentation))))
         (concat "\""lay "\" (" doc ")" (if is-alias ", alias" ""))))
     lays "\n")))

(defun ergoemacs-get-layouts (&optional ob)
  "Gets the list of all known layouts"
  (let (ret)
    (mapatoms (lambda(s)
                (let ((sn (symbol-name s)))
                  (and (string-match "^ergoemacs-layout-" sn)
                       (setq ret (cons (replace-regexp-in-string "ergoemacs-layout-" "" sn) ret)))))
              ob)
    ret))

(defcustom ergoemacs-keyboard-layout (or (getenv "ERGOEMACS_KEYBOARD_LAYOUT") "us")
  (concat "Specifies which keyboard layout to use.
This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT.

After setting this value to apply these settings you will need to type in M-x ergoemacs-setup-keys

Valid values are:

" (ergoemacs-get-layouts-doc))
  :type (ergoemacs-get-layouts-type)
  :group 'ergoemacs-mode)

;; Ergoemacs keys

(defcustom ergoemacs-variable-layout
  '(("M-j" backward-char  "← char")
    ("M-l" forward-char "→ char")
    ("M-i" previous-line "↑ line")
    ("M-k" next-line "↓ line")

    ;; Move by word
    ("M-u" backward-word "← word")
    ("M-o" forward-word "→ word")

    ;; Move by paragraph
    ("M-U" backward-block "← ¶")
    ("M-O" forward-block  "→ ¶")

    ;; Move to beginning/ending of line
    ("M-h" move-beginning-of-line "← line")
    ("M-H" move-end-of-line "→ line")

    ;; Move by screen (page up/down)
    ("M-I" scroll-down "↓ page")
    ("M-K" scroll-up "↑ page")

    ;; Move to beginning/ending of file
    ("M-J" backward-open-bracket "↑ buffer")
    ("M-L" forward-close-bracket "↓ buffer")

    ;; isearch
    ("M-y" isearch-forward "→ isearch")
    ("M-Y" isearch-backward "← isearch")

    ("M-p" recenter-top-bottom "recenter")

    ;; MAJOR EDITING COMMANDS

    ;; Delete previous/next char.
    ("M-d" delete-backward-char "⌫ char")
    ("M-f" delete-char "⌦ char")

    ;; Delete previous/next word.
    ("M-e" backward-kill-word "⌫ word")
    ("M-r" kill-word "⌦ word")

    ;; Copy Cut Paste, Paste previous
    ("M-x" cut-line-or-region "✂ region")
    ("M-c" copy-line-or-region "copy")
    ("M-v" yank "paste")
    ("M-V" yank-pop "paste ↑")
    ("M-C" copy-all "copy all")
    ("M-X" cut-all "✂ all")

    ;; undo and redo
    ("M-Z" redo "↷ redo")
    ("M-z" undo "↶ undo")

    ;; Kill line
    ("M-g" kill-line "⌦ line")
    ("M-G" kill-line-backward "⌫ line")

    ;; Textual Transformation

    ("M-S-SPC" mark-paragraph "Mark Paragraph")
    ("M-w" shrink-whitespaces "⌧ white")
    ("M-'" comment-dwim "cmt dwim")
    ("M-/" toggle-letter-case "tog. case")

    ;; keyword completion, because Alt+Tab is used by OS
    ("M-t" call-keyword-completion "↯ compl")

    ;; Hard-wrap/un-hard-wrap paragraph
    ("M-q" compact-uncompact-block "fill/unfill ¶")

    ;; EMACS'S SPECIAL COMMANDS

    ;; Cancel
    ("<escape>" keyboard-quit)

    ;; Mark point.
    ("M-SPC" set-mark-command "Set Mark")

    ("M-a" execute-extended-command "M-x")
    ("M-A" shell-command "shell cmd")

    ;; WINDOW SPLITING
    ("M-s" move-cursor-next-pane "next pane")
    ("M-S" move-cursor-previous-pane "prev pane")

    ;; --------------------------------------------------
    ;; OTHER SHORTCUTS

    ("M-~" switch-to-previous-frame "prev frame")
    ("M-`" switch-to-next-frame "next frame")

    ("M-5" query-replace "rep")
    ("M-%" query-replace-regexp "rep reg")

    ("M-3" delete-other-windows "↯ expand")
    ("M-0" delete-window "close win")

    ("M-4" split-window-vertically "split |")
    ("M-$" split-window-horizontally "split —")

    ("M-8" extend-selection "←region→")
    ("M-*" select-text-in-quote "←quote→")
    ("M-6" select-current-block "Sel. Block")
    ("M-7" select-current-line "Sel. Line"))

  "Ergoemacs that vary from keyboard types.  By default these keybindings are based on QWERTY."
  :type '(repeat
          (list :tag "Keys"
                (choice (string :tag "QWERTY Kbd Code")
                        (sexp :tag "Key"))
                (symbol :tag "Function")
                (choice (const :tag "No Label" nil)
                        (string :tag "Label"))))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-fixed-layout
  `( ;; --------------------------------------------------
    ;; STANDARD SHORTCUTS
    ("C-n" new-empty-buffer "New Buffer")
    ("C-S-n" make-frame-command "New Frame")
    ("C-o" find-file "Edit File")
    ("C-S-o" open-in-external-app "OS Open")
    ("C-S-t" open-last-closed "Open Last")
    ("C-w" close-current-buffer "Close Buf.")
    ("C-s" save-buffer "Save")
    ("C-S-s" write-file "Save As")
    ("C-p" print-buffer-confirm "Print")
    ("C-a" mark-whole-buffer "Select all")
    ("C-S-z" redo "↷ redo")
    ("C-y" redo "↷ redo")
    ("C-z" undo "↶ undo")

    ("C-f" isearch-forward "Search")

    ("<delete>" delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.

    ("C-<prior>" previous-user-buffer)
    ("C-<next>" next-user-buffer)

    ("C-S-<prior>" previous-emacs-buffer)
    ("C-S-<next>" next-emacs-buffer)

    ("M-S-<prior>" backward-page)
    ("M-S-<next>" forward-page)

    ("C-x C-b" ibuffer)
    ("C-h m" describe-major-mode)
    ("<f1> m" describe-major-mode)
    ("C-h o" where-is-old-binding)
    ("<f1> o" where-is-old-binding)

    ("C-<pause>" kill-compilation)      ; stop compilation/find/grep

    ("<f1> 1" describe-function)
    ("<f1> 2" describe-variable)
    ("<f1> 3" describe-key)
    ("<f1> 4" describe-char)
    ("<f1> 5" man)
    ("<f1> 7" lookup-google)
    ("<f1> 8" lookup-wikipedia)
    ("<f1> 9" lookup-word-definition)
    ("<f1> `" elisp-index-search)

    ("C-h 1" describe-function)
    ("C-h 2" describe-variable)
    ("C-h 3" describe-key)
    ("C-h 4" describe-char)
    ("C-h 5" man)
    ("C-h 7" lookup-google)
    ("C-h 8" lookup-wikipedia)
    ("C-h 9" lookup-word-definition)
    ("C-h `" elisp-index-search)

    ("<f2>" cut-line-or-region) ;cut
    ("<C-f2>" cut-all)
    ("<f3>" copy-line-or-region) ;copy
    ("<C-f3>" copy-all)
    ("<f4>" yank)
    ("<C-f4>" yank-pop)

    ("<f5>" undo)
    ("<C-f5>" redo)

    ("<f8>" ctl-x-map)
    ;; Set the menu/apps key to do emacs's M-x if on Windows
    ,@(cond
       ((string-equal system-type "windows-nt")
        '(("<apps>" execute-extended-command)))
       ((string-equal system-type "darwin")
        nil)
       ((string-equal system-type "gnu/linux")
        nil))

    ("<M-delete>" kill-word)

    ;; arrow keys to traverse brackets
    ("<M-left>" backward-open-bracket) ; Alt+←
    ("<M-right>" forward-close-bracket) ; Alt+→

    ("<M-up>" backward-block) ; Alt+↑
    ("<M-down>" forward-block) ; Alt+↓
    )
  "Key bindings that are constant regardless of they keyboard used."
  :type '(repeat
          (list :tag "Fixed Key"
                (choice (string :tag "Kbd code")
                        (sexp :tag "Key"))
                (symbol :tag "Function to Run")
                (choice (const :tag "No Label" nil)
                        (string :tag "Label"))))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-minor-mode-layout
  '(;; Key/variable command x-hook
    ;; Minibuffer hook
    (minibuffer-setup-hook
     ((keyboard-quit minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      (previous-line previous-history-element minor-mode-overriding-map-alist)
      (next-line next-history-element minor-mode-overriding-map-alist)
      ("<f11>" previous-history-element  minor-mode-overriding-map-alist)
      ("<f12>" next-history-element  minor-mode-overriding-map-alist)
      ("S-<f11>" previous-matching-history-element  minor-mode-overriding-map-alist)
      ("S-<f12>" next-matching-history-element  minor-mode-overriding-map-alist)
      ))

    ;; Isearch Hook
    (isearch-mode-hook
     (("M-p" nil isearch-mode-map) ; was isearch-ring-retreat
      ("M-n" nil isearch-mode-map) ; was isearch-ring-advance
      ("M-y" nil isearch-mode-map) ; was isearch-yank-kill
      ("M-c" nil isearch-mode-map) ; was isearch-toggle-case-fold
      ("M-r" nil isearch-mode-map) ; was isearch-toggle-regexp
      ("M-e" nil isearch-mode-map) ; was isearch-edit-string

      (keyboard-quit isearch-abort isearch-mode-map)
      (isearch-forward isearch-repeat-forward isearch-mode-map)
      (isearch-backward isearch-repeat-backward isearch-mode-map)
      (recenter recenter isearch-mode-map)
      (yank isearch-yank-kill isearch-mode-map)

      ;; CUA paste key is isearch-yank-kill in isearch mode
      ("C-v" isearch-yank-kill isearch-mode-map)

      ;; isearch-other-control-char sends the key to the original buffer and cancels isearch
      (kill-ring-save isearch-other-control-char isearch-mode-map)
      (kill-word isearch-other-control-char isearch-mode-map)
      (backward-kill-word isearch-other-control-char isearch-mode-map)

      ("<f11>" isearch-ring-retreat isearch-mode-map)
      ("<f12>" isearch-ring-advance isearch-mode-map)))

    ;; Comint
    (comint-mode-hook
     (("<f11>" comint-previous-input comint-mode-map)
      ("<f12>" comint-next-input comint-mode-map)
      ("S-<f11>" comint-previous-matching-input comint-mode-map)
      ("S-<f12>" comint-next-matching-input comint-mode-map)))

    ;; Log Edit
    (log-edit-mode-hook
     (("<f11>" log-edit-previous-comment log-edit-mode-map)
      ("<f12>" log-edit-next-comment log-edit-mode-map)
      ("S-<f11>" log-edit-previous-comment log-edit-mode-map)
      ("S-<f12>" log-edit-next-comment log-edit-mode-map)))

    ;; Eshell
    (eshell-mode-hook
     ((move-beginning-of-line eshell-bol minor-mode-overriding-map-alist)
      ("<home>" eshell-bol minor-mode-overriding-map-alist)
      ("<f11>" eshell-previous-matching-input-from-input minor-mode-overriding-map-alist)
      ("<f12>" eshell-next-matching-input-from-input minor-mode-overriding-map-alist)
      ("S-<f11>" eshell-previous-matching-input-from-input minor-mode-overriding-map-alist)
      ("S-<f12>" eshell-next-matching-input-from-input minor-mode-overriding-map-alist)))

    ;; Iswitchdb hook
    (iswitchb-minibuffer-setup-hook
     ((keyboard-quit minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      (isearch-backward iswitchb-prev-match minor-mode-overriding-map-alist)
      (isearch-forward iswitchb-next-match minor-mode-overriding-map-alist)

      ("<f11>" iswitchb-prev-match minor-mode-overriding-map-alist)
      ("<f12>" iswitchb-next-match minor-mode-overriding-map-alist)
      ("S-<f11>" iswitchb-prev-match minor-mode-overriding-map-alist)
      ("S-<f12>" iswitchb-next-match minor-mode-overriding-map-alist)))

    ;; Ido minibuffer setup hook
    (ido-minibuffer-setup-hook
     ((keyboard-quit minibuffer-keyboard-quit ido-common-completion-map)
      (forward-char ido-next-match ido-common-completion-map)
      (backward-char ido-prev-match ido-common-completion-map)
      (previous-line ido-prev-match-dir ido-file-dir-completion-map)
      (previous-line ido-prev-match-dir ido-file-completion-map)
      (next-line ido-next-match-dir ido-file-dir-completion-map)
      (next-line ido-next-match-dir ido-file-completion-map)
      ("<f11>" previous-history-element ido-common-completion-map)
      ("<f12>" next-history-element ido-common-completion-map)
      ("S-<f11>" previous-matching-history-element ido-common-completion-map)
      ("S-<f12>" next-matching-history-element ido-common-completion-map)))
    ;; Helm mode hooks
    (helm-before-initialize-hook
     (("C-w" helm-keyboard-quit helm-map)
      (next-line helm-next-line helm-map)
      (previous-line helm-previous-line helm-map)
      (forward-char helm-next-source helm-map)
      (backward-char helm-previous-source helm-map)
      (keyboard-quit helm-keyboard-quit helm-map)
      (recenter-top-bottom helm-recenter-top-bottom helm-map)
      (cut-line-or-region helm-yank-text-at-point helm-map)
      (scroll-down helm-next-page helm-map)
      (scroll-up helm-previous-page helm-map)))
    ;; Auto-complete-mode-hook
    ;; When the `auto-complete-mode' is on, and when a word completion
    ;; is in process, Ctrl+s does `ac-isearch'.
    ;; This fixes it.
    (auto-complete-mode-hook
     ((isearch-forward ac-isearch ac-completing-map)
      ("C-s" nil ac-completing-map))))
  "Key bindings that are applied as hooks to specific modes"
  :type '(repeat
          (list :tag "Keys for a particular minor/major mode"
                (symbol :tag "Hook for mode")
                (repeat
                 (list :tag "Key"
                       (choice
                        (symbol :tag "Defined Ergoemacs Function to Remap")
                        (string :tag "Kbd Code"))
                       (choice
                        (symbol :tag "Function to Run")
                        (const :tag "Unbind Key" nil))
                       (symbol :tag "Keymap to Modify")))))
  :group 'ergoemacs-mode)

(defvar ergoemacs-needs-translation nil)
(defvar ergoemacs-translation-from nil)
(defvar ergoemacs-translation-to nil)
(defvar ergoemacs-translation-assoc nil)
(defvar ergoemacs-translation-regexp nil)

(defun ergoemacs-setup-translation (layout &optional base-layout)
  "Setup translation from BASE-LAYOUT to LAYOUT."
  (let ((base (or base-layout "us"))
        lay
        len i)
    (unless (and (string= layout ergoemacs-translation-to)
                 (string= base ergoemacs-translation-from))
      (if (equal layout base)
          (progn
            (setq ergoemacs-translation-from base)
            (setq ergoemacs-translation-to layout)
            (setq ergoemacs-needs-translation nil)
            (setq ergoemacs-translation-assoc nil)
            (setq ergoemacs-translation-regexp nil))
        (setq ergoemacs-translation-from base)
        (setq ergoemacs-translation-to layout)
        (setq lay (symbol-value (intern (concat "ergoemacs-layout-" layout))))
        (setq base (symbol-value (intern (concat "ergoemacs-layout-" base))))
        (setq ergoemacs-needs-translation t)
        (setq ergoemacs-translation-assoc nil)
        (setq len (length base))
        (setq i 0)
        (while (< i len)
          (unless (or (string= "" (nth i base))
                      (string= "" (nth i lay)))
            (add-to-list 'ergoemacs-translation-assoc
                         `(,(nth i base) . ,(nth i lay))))
          (setq i (+ i 1)))
        (setq ergoemacs-translation-regexp
              (format "-\\(%s\\)$"
                      (regexp-opt (mapcar (lambda(x) (nth 0 x))
                                          ergoemacs-translation-assoc) nil)))))))

(defun ergoemacs-kbd (key &optional just-translate)
  "Translates kbd code KEY for layout `ergoemacs-translation-from' to kbd code for `ergoemacs-translation-to'.
If JUST-TRANSLATE is non-nil, just return the KBD code, not the actual emacs key sequence.
"
  (let ((new-key key))
    (when (and ergoemacs-needs-translation
               (string-match ergoemacs-translation-regexp new-key))
      (setq new-key (replace-match (concat "-" (cdr (assoc (match-string 1 new-key) ergoemacs-translation-assoc))) t t new-key)))
    ;;(message "%s -> %s" key new-key)
    (if (not just-translate)
        (read-kbd-macro (encode-coding-string new-key locale-coding-system))
      new-key)))



(defmacro ergoemacs-setup-keys-for-keymap (keymap)
  "Setups ergoemacs keys for a specific keymap"
  `(progn
     (setq ,keymap (make-sparse-keymap))
     (mapc
      (lambda(x)
        (if (eq 'string (type-of (nth 0 x)))
            (define-key ,keymap (read-kbd-macro (encode-coding-string (nth 0 x) locale-coding-system)) (nth 1 x))
          (define-key ,keymap (nth 0 x) (nth 1 x))))
      ergoemacs-fixed-layout)
     (mapc
      (lambda(x)
        (if (eq 'string (type-of (nth 0 x)))
            (define-key ,keymap (ergoemacs-kbd (nth 0 x)) (nth 1 x))
          (define-key ,keymap (nth 0 x) (nth 1 x))))
      ergoemacs-variable-layout)))

(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  (ergoemacs-setup-keys-for-keymap ergoemacs-keymap))

(require 'lookup-word-on-internet nil "NOERROR")


;; Keyboard Settings

;; SVG heavily modified from
;; http://en.wikipedia.org/wiki/File:KB_United_Kingdom.svg

;; Color scheme chose from color brewer.
(defun ergoemacs-gen-svg-quote (char)
  ;; Derived from `describe-char'
  (let* ((case-fold-search nil)
         code str)
    (save-match-data
      (cond
       ((string= char "")
        " ")
       ((string= char ">")
        "&lt;")
       ((string= char "<")
        "&gt;")
       ((string= char "\"")
        "&quot;")
       ((string-match "[A-Z0-9]" char)
        char)
       (t
        (format "&#x%04X;" (encode-char (with-temp-buffer
                                          (insert char)
                                          (char-before)) 'unicode)))))))

(defun ergoemacs-trans-mac-osx (key &optional swap-option-and-control)
  "Translates Emacs kbd code KEY to Mac OS X DefaultKeyBinding.dict"
  (let ((ret key)
        (case-fold-search t))
    (with-temp-buffer
      (insert ret)
      (goto-char (point-min))
      (while (re-search-forward "\\<M-" nil t)
        (replace-match (if swap-option-and-control "^" "~") nil t))
      (goto-char (point-min))
      (while (re-search-forward "\\<C-" nil t)
        (replace-match (if swap-option-and-control "~" "^") nil t))
      (setq ret (buffer-string)))
    (symbol-value 'ret)))

(defun ergoemacs-gen-mac-osx (layout &optional file-name extra swap-opiton-and-control)
  "Generates an Autohotkey Script for Ergoemacs Keybindings.
Currently only supports two modifier plus key."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "os_x_qwerty.dict.txt"))
        (xtra (or extra "os_x_opt_meta"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (i 0))
    ;; ergoemacs-variable-layout
    (if (not lay)
        (message "Layout %s not found" layout)
      (ergoemacs-setup-keys-for-layout layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      ;; Translate keys
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout ".dict. txt") extra-dir))
      (with-temp-file file
        (insert-file-contents (expand-file-name fn dir))
        (goto-char (point-min))
        (when (re-search-forward "QWERTY")
          (replace-match layout))
        (mapc
         (lambda(x)
           (let ((from (nth 0 x))
                 from-reg
                 (to nil))
             (setq to (ergoemacs-kbd from t))
             (if (string= from to) nil

               (setq from (ergoemacs-trans-mac-osx from t))
               (setq to (ergoemacs-trans-mac-osx to swap-opiton-and-control))
               (setq from-reg (regexp-quote from))
               (goto-char (point-min))
               (when (re-search-forward from-reg nil t)
                 (replace-match to t t)))))
         ergoemacs-variable-layout)
        (goto-char (point-min))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)))))

(defun ergoemacs-mac-osx-dicts (&optional layouts)
  "Generate Mac OS X dictionaries for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (mapc
     (lambda(x)
       (message "Generate Mac Dictionary for %s" x)
       (ergoemacs-gen-mac-osx x)
       (ergoemacs-gen-mac-osx x nil "os_x_opt-ctl" t))
     lay)))

(defun ergoemacs-trans-bash (key)
  "Translate Emacs kbd code KEY to bash kbd code"
  (let ((ret key)
        (case-fold-search nil))
    (with-temp-buffer
      (insert ret)
      (goto-char (point-min))
      (while (re-search-forward "\\([MSC]-\\)" nil t)
        (replace-match "\\\\\\1"))
      (setq ret (buffer-string)))
    (symbol-value 'ret)))

(defun ergoemacs-gen-bash (layout &optional file-name extra)
  "Generates an Autohotkey Script for Ergoemacs Keybindings.
Currently only supports two modifier plus key."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "bash-us.txt"))
        (xtra (or extra "bash"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (i 0))
    ;; ergoemacs-variable-layout
    (if (not lay)
        (message "Layout %s not found" layout)
      (ergoemacs-setup-keys-for-layout layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      ;; Translate keys
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout ".txt") extra-dir))
      (with-temp-file file
        (insert-file-contents (expand-file-name fn dir))
        (goto-char (point-min))
        (when (re-search-forward "QWERTY")
          (replace-match layout))
        (mapc
         (lambda(x)
           (let ((from (nth 0 x))
                 from-reg
                 (to nil))
             (setq to (ergoemacs-kbd from t))
             (if (string= from to) nil

               (setq from (ergoemacs-trans-bash from))
               (setq to (ergoemacs-trans-bash to))
               (setq from-reg (regexp-quote from))
               (goto-char (point-min))
               (when (re-search-forward from-reg nil t)
                 (replace-match to t t)))))
         ergoemacs-variable-layout)
        (goto-char (point-min))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)))))

(defun ergoemacs-bashs (&optional layouts)
  "Generate BASH scripts for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (mapc
     (lambda(x)
       (message "Generate bash for %s" x)
       (ergoemacs-gen-bash x))
     lay)))

(defun ergoemacs-trans-ahk (key)
  "Translates Emacs kbd code KEY to ahk kbd code. "
  (let ((ret key)
        (case-fold-search nil))
    (while (string-match "-\\([A-Z]\\)\\($\\| \\)" ret)
      (setq ret (replace-match (concat "-S-" (downcase (match-string 1 ret)) (match-string 2 ret)) t t ret )))
    (while (string-match "M-" ret)
      (setq ret (replace-match "!" t t ret)))
    (while (string-match "S-" ret)
      (setq ret (replace-match "+" t t ret)))
    (while (string-match "C-" ret)
      (setq ret (replace-match "^" t t ret)))
    (symbol-value 'ret)))

(defun ergoemacs-gen-ahk (layout &optional file-name extra)
  "Generates an Autohotkey Script for Ergoemacs Keybindings.
Currently only supports two modifier plus key."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "ahk-us.ahk"))
        (xtra (or extra "ahk"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (i 0))
    ;; ergoemacs-variable-layout
    (if (not lay)
        (message "Layout %s not found" layout)
      (ergoemacs-setup-keys-for-layout layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      ;; Translate keys
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout ".ahk") extra-dir))
      (with-temp-file file
        (insert-file-contents (expand-file-name fn dir))
        (goto-char (point-min))
        (when (re-search-forward "QWERTY")
          (replace-match layout))
        (mapc
         (lambda(x)
           (let ((from (nth 0 x))
                 from-reg
                 (to nil))
             (setq to (ergoemacs-kbd from t))
             (if (string= from to) nil
               (setq from (ergoemacs-trans-ahk from))
               (setq to (ergoemacs-trans-ahk to))
               (when (string-match "\\([^a-z0-9]\\)$" to)
                 (setq to (replace-match "`\\1" t nil to)))
               (cond
                ((string-match "^\\(\\+\\|!\\|\\^\\)\\{2\\}" from)
                 (setq from-reg (regexp-opt `(,from
                                              ,(concat (substring from 1 2) (substring from 0 1) (substring from 2)))
                                            t)))
                (t
                 (setq from-reg (regexp-quote from))))
               (goto-char (point-min))
               (when (re-search-forward from-reg nil t)
                 (replace-match to t t)))))
         ergoemacs-variable-layout)
        (goto-char (point-min))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)))))

(defun ergoemacs-ahks (&optional layouts)
  "Generate Autohotkey scripts for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (mapc
     (lambda(x)
       (message "Generate ahk for %s" x)
       (ergoemacs-gen-ahk x))
     lay)))

;;;###autoload
(defun ergoemacs-extras ( &optional layouts)
  "Generate layout diagram, and other scripts for system-wide ErgoEmacs keybinding.

The following are generated:
• SVG Diagram for ErgoEmacs command layouts in SVG format.
• Bash 〔.inputrc〕 code.
• Mac OS X 〔DefaultKeyBinding.dict〕 code.
• AutoHotkey script for Microsoft Windows.

Files are generated in the dir 〔ergoemacs-extras〕 at `user-emacs-directory'."
  (interactive)
  (ergoemacs-svgs layouts)
  (ergoemacs-ahks layouts)
  (ergoemacs-bashs layouts)
  (ergoemacs-mac-osx-dicts layouts)
  (find-file (expand-file-name "ergoemacs-extras" user-emacs-directory)) )

(defun ergoemacs-gen-svg (layout &optional file-name extra)
  "Generates a SVG picture of the layout
FILE-NAME represents the SVG template
EXTRA represents an extra file representation."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "kbd.svg"))
        (xtra (or extra "kbd-layouts"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (fix (mapcar
              (lambda(x)
                `(,(if (condition-case err
                           (string-match "-S-\\([a-z]\\)\\>" (nth 0 x))
                         (error nil))
                       (replace-match (format "-%s" (upcase (match-string 1 (nth 0 x)))) t t (nth 0 x))
                     (nth 0 x))  ,(nth 1 x) ,(nth 2 x)))
              `(,@ergoemacs-fixed-layout
                ,@(if cua-mode
                      `(("C-c" nil "Copy")
                        ("C-v" nil "Paste")
                        ("C-x" nil "Cut"))
                    nil))))
        (i 0))
    (if (not lay)
        (message "Layout %s not found" layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq lay (symbol-value lay))
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout ".svg") extra-dir))
      (with-temp-file file
        (insert-file-contents
         (expand-file-name fn dir))
        (when (string-equal system-type "windows-nt")
          ;; Use Arial Unicode MS when on windows
          (goto-char (point-min))
          (while (re-search-forward "\\(?:Helvetica\\|Sans\\)\\([\";]\\)" nil t)
            (replace-match "Arial Unicode MS\\1")))
        (while (< i (length lay))

          (goto-char (point-min))
          (when (search-forward (format ">%s<" i) nil t)
            (replace-match (format ">%s<" (ergoemacs-gen-svg-quote (nth i lay))) t t))
          (goto-char (point-min))
          (setq txt (assoc (format "M-%s" (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) ergoemacs-variable-layout))
          (if (not txt)
              (setq txt "")
            (if (>= (length txt) 3)
                (setq txt (nth 2 txt))
              (setq txt "")))

          (when (string= txt "")
            (setq txt (all-completions (format "M-%s " (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) ergoemacs-variable-layout))
            (if (= 0 (length txt))
                (setq txt "")
              (setq txt "prefix")))

          (unless (string= "" txt)
            (when (search-forward (format ">M%s<" i) nil t)
              (replace-match  (format ">%s<" txt) t t)))

          (goto-char (point-min))
          (setq txt (assoc (format "C-%s" (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) ergoemacs-variable-layout))
          (if (not txt)
              (setq txt "")
            (if (>= (length txt) 3)
                (setq txt (nth 2 txt))
              (setq txt "")))

          (when (string= txt "")
            (setq txt (all-completions (format "C-%s " (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) ergoemacs-variable-layout))
            (if (= 0 (length txt))
                (setq txt "")
              (setq txt "prefix")))

          (unless (string= "" txt)
            (when (search-forward (format ">C%s<" i) nil t)
              (replace-match  (format ">%s<" txt) t t)))

          ;; Now fill in the ergoemacs-fixed-layout.

          (goto-char (point-min))
          (setq txt (assoc (format "M-%s" (nth i lay)) fix))
          (if (not txt)
              (setq txt "")
            (if (>= (length txt) 3)
                (setq txt (nth 2 txt))
              (setq txt "")))

          (when (string= txt "")
            (setq txt (all-completions (format "M-%s " (nth i lay)) fix))
            (if (= 0 (length txt))
                (setq txt "")
              (setq txt "prefix")))

          (unless (string= "" txt)
            (when (search-forward (format ">M%s<" i) nil t)
              (replace-match  (format ">%s<" txt) t t)))

          (goto-char (point-min))
          (setq txt (assoc (format "C-%s" (nth i lay)) fix))
          (if (not txt)
              (setq txt "")
            (if (>= (length txt) 3)
                (setq txt (nth 2 txt))
              (setq txt "")))

          (when (string= txt "")
            (setq txt (all-completions (format "C-%s " (nth i lay)) fix))
            (if (= 0 (length txt))
                (setq txt "")
              (setq txt "prefix")))
          (unless (string= "" txt)
            (when (search-forward (format ">C%s<" i) nil t)
              (replace-match  (format ">%s<" txt) t t)))

          (mapc
           (lambda(x)
             (goto-char (point-min))
             (setq txt (assoc x fix))
             (if (not txt)
                 (setq txt "")
               (if (>= (length txt) 3)
                   (setq txt (nth 2 txt))
                 (setq txt "")))
             (when (string= txt "")
               (setq txt (all-completions (format "%s " x) fix))
               (if (= 0 (length txt))
                   (setq txt "")
                 (setq txt "prefix")))
             (when (string= txt "")
               (setq txt (assoc x ergoemacs-variable-layout))
               (if (not txt)
                   (setq txt "")
                 (if (>= (length txt) 3)
                     (setq txt (nth 2 txt))
                   (setq txt "")))

               (when (string= txt "")
                 (setq txt (all-completions (format "%s " x) ergoemacs-variable-layout))
                 (if (= 0 (length txt))
                     (setq txt "")
                   (setq txt "prefix"))))
             (when (search-forward (format ">%s<" x) nil t)
               (replace-match  (format ">%s<" txt) t t)))
           '("M-S-SPC" "M-SPC" "C-S-SPC" "C-SPC"))

          (setq i (+ i 1)))
        (while (re-search-forward ">[CM][0-9]+<" nil t)
          (replace-match "><")))
      (message "Layout generated to %s" file))))

(defun ergoemacs-svgs (&optional layouts)
  "Generate SVGs for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (mapc
     (lambda(x)
       (message "Generate SVG for %s" x)
       (ergoemacs-gen-svg x)
       (ergoemacs-gen-svg x "kbd-ergo.svg" "ergo-layouts"))
     lay)))




;;; ergoemacs-keymap

(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.")

;;----------------------------------------------------------------------
;; CUA fix

(let (cuaModeState cua-mode)
  (cua-mode 1) ; turn on cua-mode first so the command ergoemacs-fix-cua--pre-command-handler-1 will be able to set some symbols from cua-mode

  (defun ergoemacs-fix-cua--pre-command-handler-1 ()
    "Fixes CUA minor mode so selection is highlighted only when
Shift+<special key> is used (arrows keys, home, end, pgdn, pgup, etc.)."
    (defun cua--pre-command-handler-1 ()
      ;; Cancel prefix key timeout if user enters another key.
      (when cua--prefix-override-timer
        (if (timerp cua--prefix-override-timer)
            (cancel-timer cua--prefix-override-timer))
        (setq cua--prefix-override-timer nil))

      (cond
       ;; Only symbol commands can have necessary properties
       ((not (symbolp this-command))
        nil)

       ;; Handle delete-selection property on non-movement commands
       ((not (eq (get this-command 'CUA) 'move))
        (when (and mark-active (not deactivate-mark))
          (let* ((ds (or (get this-command 'delete-selection)
                         (get this-command 'pending-delete)))
                 (nc (cond
                      ((not ds) nil)
                      ((eq ds 'yank)
                       'cua-paste)
                      ((eq ds 'kill)
                       (if cua--rectangle
                           'cua-copy-rectangle
                         'cua-copy-region))
                      ((eq ds 'supersede)
                       (if cua--rectangle
                           'cua-delete-rectangle
                         'cua-delete-region))
                      (t
                       (if cua--rectangle
                           'cua-delete-rectangle ;; replace?
                         'cua-replace-region)))))
            (if nc
                (setq this-original-command this-command
                      this-command nc)))))

       ;; Handle shifted cursor keys and other movement commands.
       ;; If region is not active, region is activated if key is shifted.
       ;; If region is active, region is canceled if key is unshifted
       ;;   (and region not started with C-SPC).
       ;; If rectangle is active, expand rectangle in specified direction and
       ;;   ignore the movement.
       ((if window-system
            ;; Shortcut for window-system, assuming that input-decode-map is empty.

            ;; ErgoEmacs patch begin ------------------
        ;;;; (memq 'shift (event-modifiers
        ;;;;               (aref (this-single-command-raw-keys) 0)))
            (and (memq 'shift (event-modifiers
                               (aref (this-single-command-raw-keys) 0)))
                 ;; In this way, we expect to use CUA only with keys that
                 ;; are symbols (like <left>, <next>, etc.)
                 (symbolp (event-basic-type (aref (this-single-command-raw-keys) 0))))
          ;; ErgoEmacs patch end --------------------

          (or
           ;; Check if the final key-sequence was shifted.
           (memq 'shift (event-modifiers
                         (aref (this-single-command-keys) 0)))
           ;; If not, maybe the raw key-sequence was mapped by input-decode-map
           ;; to a shifted key (and then mapped down to its unshifted form).
           (let* ((keys (this-single-command-raw-keys))
                  (ev (lookup-key input-decode-map keys)))
             (or (and (vector ev) (memq 'shift (event-modifiers (aref ev 0))))
                 ;; Or maybe, the raw key-sequence was not an escape sequence
                 ;; and was shifted (and then mapped down to its unshifted form).
                 (memq 'shift (event-modifiers (aref keys 0)))))))
        (unless mark-active
          (push-mark-command nil t))
        (setq cua--last-region-shifted t)
        (setq cua--explicit-region-start nil))

       ;; Set mark if user explicitly said to do so
       ((or cua--explicit-region-start cua--rectangle)
        (unless mark-active
          (push-mark-command nil nil)))

       ;; Else clear mark after this command.
       (t
        ;; If we set mark-active to nil here, the region highlight will not be
        ;; removed by the direct_output_ commands.
        (setq deactivate-mark t)))

      ;; Detect extension of rectangles by mouse or other movement
      (setq cua--buffer-and-point-before-command
            (if cua--rectangle (cons (current-buffer) (point))))))
  (if cuaModeState (progn (cua-mode 1)) (cua-mode -1)))


;; ErgoEmacs hooks
(defun ergoemacs-key-fn-lookup (function)
  "Looks up the key binding for FUNCTION based on `ergoemacs-variable-layout'."
  (let ((ret nil))
    (mapc
     (lambda(x)
       (when (equal (nth 1 x) function)
         (setq ret (ergoemacs-kbd (nth 0 x)))))
     ergoemacs-variable-layout)
    (symbol-value 'ret)))

(defmacro ergoemacs-create-hook-function (hook keys)
  "Creates a hook function based on the HOOK and the list of KEYS defined."
  (let ((is-override (make-symbol "is-override"))
        (local-list '()))
    (setq is-override (eq 'minor-mode-overriding-map-alist (nth 2 (nth 0 keys))))
    `(progn
       ,(if is-override
            `(progn
               (defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")) nil
                 ,(concat "Ergoemacs overriding keymap for `" (symbol-name hook) "'")))
          nil)
       (defun ,(intern (concat "ergoemacs-" (symbol-name hook))) ()
         ,(concat "Hook for `" (symbol-name hook) "' so ergoemacs keybindings are not lost.
This is an automatically generated function derived from `ergoemacs-minor-mode-layout'.")
         ,(if is-override
              `(ergoemacs-setup-keys-for-keymap ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
            nil)
         ,@(mapcar
            (lambda(def)
              (if (or (eq 'string (type-of (nth 0 def)))
                      (ergoemacs-key-fn-lookup (nth 0 def)))
                `(progn
                   ,(if is-override nil
                      (if (member (nth 2 def) local-list) nil
                        (add-to-list 'local-list (nth 2 def))
                        `(set (make-local-variable ',(nth 2 def)) ,(nth 2 def))))
                   (define-key ,(if is-override
                                    (intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))
                                  (nth 2 def))
                     ,(if (eq 'string (type-of (nth 0 def)))
                          `(kbd ,(nth 0 def))
                        `(ergoemacs-key-fn-lookup ',(nth 0 def)))
                     ',(nth 1 def)))
                nil))
            keys)
         ,(if is-override
              `(add-to-list 'minor-mode-overriding-map-alist
                            (cons 'ergoemacs-mode ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
                            nil ,(if (equal hook 'minibuffer-setup-hook)
                                     '(lambda (x y)
                                         (equal (car y) (car x)))
                                   nil))
            nil)
         t)
       (ergoemacs-add-hook ',hook ',(intern (concat "ergoemacs-" (symbol-name hook)))))))

(defvar ergoemacs-hook-list (list)
  "List of hook and hook-function pairs.")

(defun ergoemacs-add-hook (hook hook-function)
  "Adds a pair of hook and hook-function to the list
ergoemacs hooks."
  (add-to-list 'ergoemacs-hook-list (cons hook hook-function)))

(defun ergoemacs-hook-modes ()
  "Installs/Removes ErgoEmacs minor mode hooks from major modes
depending the state of `ergoemacs-mode' variable.  If the mode
is being initialized, some global keybindings in current-global-map
will change."

  (let ((modify-hook (if (and (boundp 'ergoemacs-mode) ergoemacs-mode) 'add-hook 'remove-hook))
        (modify-advice (if (and (boundp 'ergoemacs-mode) ergoemacs-mode) 'ad-enable-advice 'ad-disable-advice)))

    ;; Fix CUA
    (if (and (boundp 'ergoemacs-mode) ergoemacs-mode)
        (ergoemacs-fix-cua--pre-command-handler-1))

    ;; when ergoemacs-mode is on, activate hooks and unset global keys, else do inverse
    (if (and (boundp 'ergoemacs-mode) ergoemacs-mode (not (equal ergoemacs-mode 0)))
        (progn
          (ergoemacs-unset-redundant-global-keys)

          ;; alt+n is the new "Quit" in query-replace-map
          (ergoemacs-unset-global-key query-replace-map "\e")
          (define-key query-replace-map (ergoemacs-key-fn-lookup 'keyboard-quit) 'exit-prefix))
      ;; if ergoemacs was disabled: restore original keys
      (ergoemacs-restore-global-keys))

    ;; install the mode-hooks
    (dolist (hook ergoemacs-hook-list)
      (funcall modify-hook (car hook) (cdr hook)))

    ;; enable advices
    (condition-case err
        (progn
          (funcall modify-advice 'global-set-key 'around 'ergoemacs-global-set-key-advice)
          (funcall modify-advice 'global-unset-key 'around 'ergoemacs-global-unset-key-advice)
          (funcall modify-advice 'local-set-key 'around 'ergoemacs-local-set-key-advice)
          (funcall modify-advice 'local-unset-key 'around 'ergoemacs-local-unset-key-advice)
          ;; update advices
          (ad-activate 'global-set-key)
          (ad-activate 'global-unset-key)
          (ad-activate 'local-set-key)
          (ad-activate 'local-unset-key))
      (error "Error modifying advices. %s" err))))

(defun ergoemacs-create-hooks ()
  "Creates Ergoemacs Hooks from `ergoemacs-minor-mode-layout'."
  (let ((ergoemacs-mode))
    (ergoemacs-hook-modes))
  (setq ergoemacs-hook-list nil)
  (mapc
   (lambda(x)
     (let ((f (macroexpand `(ergoemacs-create-hook-function ,(car x) ,(car (cdr x))))))
       (eval f)))
   ergoemacs-minor-mode-layout)
  (ergoemacs-hook-modes))

(defun ergoemacs-setup-keys (&optional no-check)
  "Setups keys based on a particular layout. Based on `ergoemacs-keyboard-layout'"
  (interactive)
  (let ((ergoemacs-state (if (boundp 'ergoemacs-mode) ergoemacs-mode nil))
        (cua-state cua-mode)
        (layout
         (intern-soft
          (concat "ergoemacs-layout-" ergoemacs-keyboard-layout))))
    (unless no-check
      (when ergoemacs-state
        (when (fboundp 'ergoemacs-mode)
          (ergoemacs-mode -1)
          (when cua-state
            (cua-mode -1)))))
    (cond
     (layout
      (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout))
     (t ; US qwerty by default
      (ergoemacs-setup-keys-for-layout "us")))
    (ergoemacs-create-hooks)
    ;; Redefine minor mode to update keymap.
    ;; Seems a bit hackish, but I believe it works.
    (define-minor-mode ergoemacs-mode
      "Toggle ergoemacs keybinding mode.
This minor mode changes your emacs keybindings.
Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.
Argument of t or nil should not be used.
For full documentation, see:
URL `http://xahlee.org/emacs/ergonomic_emacs_keybinding.html'

If you turned on by mistake, the key to call execute-extended-command is M-a."
      nil
      :lighter " ErgoEmacs" ;; TODO this should be nil (it is for testing purposes)
      :global t
      :keymap ergoemacs-keymap
      (ergoemacs-setup-keys t))
    (unless no-check
      (when ergoemacs-state
        (when (fboundp 'ergoemacs-mode)
          (ergoemacs-mode 1)
          (when cua-state
            (cua-mode 1)))))))

(ergoemacs-setup-keys)


;; ErgoEmacs minor mode
;;;###autoload
(define-minor-mode ergoemacs-mode
  "Toggle ergoemacs keybinding mode.
This minor mode changes your emacs keybindings.
Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.
Argument of t or nil should not be used.
For full documentation, see:
URL `http://xahlee.org/emacs/ergonomic_emacs_keybinding.html'

If you turned on by mistake, the shortcut to call execute-extended-command is M-a."
  nil
  :lighter " ErgoEmacs" ;; TODO this should be nil (it is for testing purposes)
  :global t
  :keymap ergoemacs-keymap
  (ergoemacs-setup-keys t))


;; ErgoEmacs replacements for local- and global-set-key

(defun ergoemacs-global-set-key (key command)
  "Set a key in the ergoemacs-keymap, thus
making it globally active. This allow to redefine
any key unbound or claimed by ergoemacs."
  (interactive)
  (define-key ergoemacs-keymap key command)
  ;; Needs to modify the key lists
  (let ((found))
    (setq ergoemacs-fixed-layout
          (mapcar
           (lambda(x)
             (if (not (condition-case err
                          (eq key (read-kbd-macro (encode-coding-string (nth 0 x) locale-coding-system)))
                        (error nil)))
                 x
               (setq found t)
               `(,(nth 0 x) ,command "")))
           ergoemacs-fixed-layout))
    (unless found
      (setq ergoemacs-variable-layout
            (mapcar
             (lambda(x)
               (if (not (condition-case err
                            (eq key (ergoemacs-kbd (nth 0 x)))
                          (error nil)))
                   x
                 (setq found t)
                 `(,(nth 0 x) ,command "")))
             ergoemacs-variable-layout)))
    (unless found
      (add-to-list 'ergoemacs-fixed-layout `(,key ,command "")))))

(defun ergoemacs-global-unset-key (key)
  "Removes a key from the ergoemacs-keymap."
  (interactive)
  (ergoemacs-global-set-key key nil))

(defvar ergoemacs-local-keymap nil
  "Local ergoemacs keymap")
(make-variable-buffer-local 'ergoemacs-local-keymap)

(defun ergoemacs-local-set-key (key command)
  "Set a key in the ergoemacs local map."
  ;; install keymap if not already installed
  (interactive)
  (progn
    (unless ergoemacs-local-keymap
      (ergoemacs-setup-keys-for-keymap ergoemacs-local-keymap)
      (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-local-keymap)))
    ;; add key
    (define-key ergoemacs-local-keymap key command)))

(defun ergoemacs-local-unset-key (key)
  "Unset a key in the ergoemacs local map."
  (ergoemacs-local-set-key key nil))


;; ErgoEmacs advices for local- and global-set-key

(defadvice global-set-key (around ergoemacs-global-set-key-advice (key command))
  "This let you use global-set-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-global-set-key key command)
    ad-do-it))

(defadvice global-unset-key (around ergoemacs-global-unset-key-advice (key))
  "This let you use global-unset-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-global-unset-key key)
    ad-do-it))

(defadvice local-set-key (around ergoemacs-local-set-key-advice (key command))
  "This let you use local-set-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-set-key key command)
    ad-do-it))

(defadvice local-unset-key (around ergoemacs-local-unset-key-advice (key))
  "This let you use local-unset-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-unset-key key)
    ad-do-it))

;;;###autoload
(defun ergoemacs-key (key function &optional desc)
  "Defines KEY in ergoemacs keyboard based on QWERTY and binds to FUNCTION.
Optionally provides DESC for a description of the key.
"
  (let (found)
    (setq ergoemacs-variable-layout
          (mapcar
           (lambda(x)
             (if (not (string= key (nth 0 x)))
                 x
               (setq found t)
               `(,key ,function ,desc)))
           ergoemacs-variable-layout))
    (unless found
      (add-to-list 'ergoemacs-variable-layout
                   `(,key ,function ,desc)))))

;;;###autoload
(defun ergoemacs-minor-key (hook list)
  "Defines keys to add to an ergoemacs keyboard hook.

Adds to the list `ergoemacs-minor-mode-layout' by modifying the
ergoemacs hook applied to HOOK.  The LIST is of the following
format:

 (FUNCTION/KEY FUNCTION-TO-CALL KEYMAP)
"
  (setq ergoemacs-minor-mode-layout
        (mapcar
         (lambda(mode-list)
           (if (not (equal hook (nth 0 mode-list)))
               mode-list
             (let (found lst)
               (setq lst (mapcar
                          (lambda(key-def)
                            (if (and (equal (nth 0 list) (nth 0 key-def))
                                     (equal (nth 2 list) (nth 2 key-def)))
                                (progn
                                  (setq found t)
                                  list)
                              key-def))
                          (nth 1 mode-list)))
               (unless found
                 (add-to-list 'lst list))
               `(,(nth 0 mode-list) ,lst))))
         ergoemacs-minor-mode-layout)))

(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
