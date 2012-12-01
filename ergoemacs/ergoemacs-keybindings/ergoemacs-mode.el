;;; ergoemacs-mode.el --- A minor mode, a keybinding set based on ergonomics. -*- coding: utf-8 -*-

;; Copyright © 2007, 2008, 2009 by Xah Lee
;; Copyright © 2009, 2010 by David Capello

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;;	David Capello <davidcapello@gmail.com>  ( http://www.davidcapello.com.ar/ )
;; Maintainer: Xah Lee
;; Created: August 01 2007
;; Version: 5.5.7
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
;;
;; Install:
;; See the file “_INSTALL.txt”.

;;; Change Log:
;; See the file “_HISTORY.txt”.

;;; Acknowledgment:
;; Thanks to Matthew Fidler for his implementation of the new layout
;; code, and generation of svg images for the supported layouts.
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
(defconst ergoemacs-mode-version "5.6.2"
  "Ergoemacs-keybindings minor mode version number.")

(defgroup ergoemacs-keybindings nil
  "Emulate CUA key bindings including C-x and C-c."
  :group 'editing-basics
  :group 'convenience
  :group 'emulations)

;; Include extra files
(add-to-list 'load-path  (file-name-directory
                          (or
                           load-file-name
                           (buffer-file-name))))

(load "functions")
(load "ergoemacs-unbind")

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

(defcustom ergoemacs-keyboard-layout (getenv "ERGOEMACS_KEYBOARD_LAYOUT")
  (concat "Specifies which keyboard layout to use.
This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT.

After setting this value to apply these settings you will need to type in M-x ergoemacs-setup-keys

Valid values are:

" (ergoemacs-get-layouts-doc))
  :type (ergoemacs-get-layouts-type)
  :group 'ergoemacs-keybindings)

"
 “sv” (Swedish)
 “da” (Danish)
 “pt-nativo” (Ergonomic PT-Nativo URL `http://xahlee.org/kbd/pt-nativo_keyboard_layout.html')"

(setq ergoemacs-needs-translation nil)
(setq ergoemacs-translation-from nil)
(setq ergoemacs-translation-to nil)
(setq ergoemacs-translation-assoc nil)
(setq ergoemacs-translation-regexp nil)

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
        (read-kbd-macro new-key)
      new-key)))

;; Ergoemacs keys

(defcustom ergoemacs-variable-layout
  '(("M-j" ergoemacs-backward-char-key "← char")
    ("M-l" ergoemacs-forward-char-key "→ char")
    ("M-i" ergoemacs-previous-line-key "↑ line")
    ("M-k" ergoemacs-next-line-key "↓ line")
    
    ;; Move by word
    ("M-u" ergoemacs-backward-word-key "← word")
    ("M-o" ergoemacs-forward-word-key "→ word")
    
    ;; Move by paragraph
    ("M-U" ergoemacs-backward-block-key "← ¶")
    ("M-O" ergoemacs-forward-block-key "→ ¶")
    
    ;; Move to beginning/ending of line
    ("M-h" ergoemacs-move-beginning-of-line-key "← line")
    ("M-H" ergoemacs-move-end-of-line-key "→ line")
    
    ;; Move by screen (page up/down)
    ("M-I" ergoemacs-scroll-down-key "↓ page")
    ("M-K" ergoemacs-scroll-up-key "↑ page")
    
    ;; Move to beginning/ending of file
    ("M-J" ergoemacs-backward-open-bracket-key "↑ buffer")
    ("M-L" ergoemacs-forward-close-bracket-key "↓ buffer")
    
    ;; isearch
    ("M-y" ergoemacs-isearch-forward-key "→ isearch")
    ("M-Y" ergoemacs-isearch-backward-key "← isearch")
    
    ("M-p" ergoemacs-recenter-key "recenter")
    
    ;; MAJOR EDITING COMMANDS
    
    ;; Delete previous/next char.
    ("M-d" ergoemacs-delete-backward-char-key "⌫ char")
    ("M-f" ergoemacs-delete-char-key "⌦ char")
    
    ;; Delete previous/next word.
    ("M-e" ergoemacs-backward-kill-word-key "⌫ word")
    ("M-r" ergoemacs-kill-word-key "⌦ word")
    
    ;; Copy Cut Paste, Paste previous
    ("M-x" ergoemacs-kill-region-key "✂ region")
    ("M-c" ergoemacs-kill-ring-save-key "copy")
    ("M-v" ergoemacs-yank-key "paste")
    ("M-V" ergoemacs-yank-pop-key "paste ↑")
    ("M-C" ergoemacs-copy-all-key "copy all")
    ("M-X" ergoemacs-cut-all-key "✂ all")
    
    ;; undo and redo
    ("M-Z" ergoemacs-redo-key "↷ redo")
    ("M-z" ergoemacs-undo-key "↶ undo")
    
    ;; Kill line
    ("M-g" ergoemacs-kill-line-key "⌦ line")
    ("M-G" ergoemacs-kill-line-backward-key "⌫ line")
    
    ;; Textual Transformation
    
    ("M-S-SPC" ergoemacs-mark-paragraph-key "Mark Paragraph")
    ("M-w" ergoemacs-shrink-whitespaces-key "⌧ white")
    ("M-'" ergoemacs-comment-dwim-key "cmt dwim")
    ("M-/" ergoemacs-toggle-letter-case-key "tog. case")
    
    ;; keyword completion, because Alt+Tab is used by OS
    ("M-t" ergoemacs-call-keyword-completion-key "↯ compl")
    
    ;; Hard-wrap/un-hard-wrap paragraph
    ("M-q" ergoemacs-compact-uncompact-block-key "fill/unfill ¶")
    
    ;; EMACS'S SPECIAL COMMANDS
    
    ;; Cancel
    ("<escape>" ergoemacs-keyboard-quit-key)
    
    ;; Mark point.
    ("M-SPC" ergoemacs-set-mark-command-key "Set Mark")
    
    ("M-a" ergoemacs-execute-extended-command-key "M-x")
    ("M-A" ergoemacs-shell-command-key "shell cmd")
    
    ;; WINDOW SPLITING
    ("M-s" ergoemacs-move-cursor-next-pane-key "next pane")
    ("M-S" ergoemacs-move-cursor-previous-pane-key "prev pane")
    
    ;; --------------------------------------------------
    ;; OTHER SHORTCUTS
    
    ("M-~" ergoemacs-switch-to-previous-frame-key "prev frame")
    ("M-`" ergoemacs-switch-to-next-frame-key "next frame")
    
    ("M-5" ergoemacs-query-replace-key "rep")
    ("M-%" ergoemacs-query-replace-regexp-key "rep reg")
    
    ("M-3" ergoemacs-delete-other-windows-key "↯ expand")
    ("M-0" ergoemacs-delete-window-key "close win")
    
    ("M-4" ergoemacs-split-window-vertically-key "split |")
    ("M-$" ergoemacs-split-window-horizontally-key "split —")
    
    ("M-8" ergoemacs-extend-selection-key "←region→")
    ("M-*" ergoemacs-select-text-in-quote-key "←quote→")
    ("M-6" select-current-block "Sel. Block")
    ("M-7" select-current-line "Sel. Line"))
  "Ergoemacs that vary from keyboard types.  By default these keybindings are based on QWERTY."
  :type '(repeat
          (list :tag "Keys"
                (string :tag "Kbd Code")
                (symbol :tag "Ergoemacs Variable")
                (choice (const :tag "No Label" nil)
                        (string :tag "Label"))))
  :group 'ergoemacs-keybindings)

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
    
    ;;tcut to stop compilation/find/grep
    ("C-<pause>" kill-compilation)
    
    
    ("<f1> 1" describe-function)
    ("<f1> 2" describe-variable)
    ("<f1> 3" describe-key)
    ("<f1> 4" describe-char)
    ("<f1> 5" woman)
    
    ("<f1> 6" lookup-all-dictionaries)
    ("<f1> 7" lookup-google)
    ("<f1> 8" lookup-wikipedia)
    ("<f1> 9" lookup-word-definition)
    ("<f1> 0" lookup-answers.com)
    ("<f1> [" lookup-word-dict-org)
    ("<f1> ]" lookup-wiktionary)
    ("<f1> `" elisp-index-search)
    
    ("C-h 1" describe-function)
    ("C-h 2" describe-variable)
    ("C-h 3" describe-key)
    ("C-h 4" describe-char)
    ("C-h 5" woman)
    ("C-h 6" lookup-all-dictionaries)
    ("C-h 7" lookup-google)
    ("C-h 8" lookup-wikipedia)
    ("C-h 9" lookup-word-definition)
    ("C-h 0" lookup-answers.com)
    ("C-h [" lookup-word-dict-org)
    ("C-h ]" lookup-wiktionary)
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
                (string :tag "Kbd code")
                (symbol :tag "Function to Run")
                (choice (const :tag "No Label" nil)
                        (string :tag "Label"))))
  :group 'ergoemacs-keybindings)

(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  ;; Single char cursor movement
  
  (setq ergoemacs-keymap (make-sparse-keymap))
  (mapc
   (lambda(x)
     (define-key ergoemacs-keymap (read-kbd-macro (nth 0 x)) (nth 1 x)))
   ergoemacs-fixed-layout)
  (mapc
   (lambda(x)
     (set (nth 1 x) (ergoemacs-kbd (nth 0 x))))
   ergoemacs-variable-layout)
  ;; Single char cursor movement
  (define-key ergoemacs-keymap ergoemacs-backward-char-key 'backward-char)
  (define-key ergoemacs-keymap ergoemacs-forward-char-key 'forward-char)
  (define-key ergoemacs-keymap ergoemacs-previous-line-key 'previous-line)
  (define-key ergoemacs-keymap ergoemacs-next-line-key 'next-line)
  
  ;; Move by word
  (define-key ergoemacs-keymap ergoemacs-backward-word-key 'backward-word)
  (define-key ergoemacs-keymap ergoemacs-forward-word-key 'forward-word)
  
  ;; Move by paragraph
  (define-key ergoemacs-keymap ergoemacs-backward-block-key 'backward-block)
  (define-key ergoemacs-keymap ergoemacs-forward-block-key 'forward-block)
  
  ;; Move to beginning/ending of line
  (define-key ergoemacs-keymap ergoemacs-move-beginning-of-line-key 'move-beginning-of-line)
  (define-key ergoemacs-keymap ergoemacs-move-end-of-line-key 'move-end-of-line)
  
  ;; Move by screen (page up/down)
  (define-key ergoemacs-keymap ergoemacs-scroll-down-key 'scroll-down)
  (define-key ergoemacs-keymap ergoemacs-scroll-up-key 'scroll-up)
  
  ;; Move to beginning/ending of file
  (define-key ergoemacs-keymap ergoemacs-backward-open-bracket-key 'backward-open-bracket)
  (define-key ergoemacs-keymap ergoemacs-forward-close-bracket-key 'forward-close-bracket)
  
  ;; isearch
  (define-key ergoemacs-keymap ergoemacs-isearch-forward-key 'isearch-forward)
  (define-key ergoemacs-keymap ergoemacs-isearch-backward-key 'isearch-backward)
  
  (define-key ergoemacs-keymap ergoemacs-recenter-key 'recenter-top-bottom)
  
  ;; MAJOR EDITING COMMANDS
  
  ;; Delete previous/next char.
  (define-key ergoemacs-keymap ergoemacs-delete-backward-char-key 'delete-backward-char)
  (define-key ergoemacs-keymap ergoemacs-delete-char-key 'delete-char)
  
  ;; Delete previous/next word.
  (define-key ergoemacs-keymap ergoemacs-backward-kill-word-key 'backward-kill-word)
  (define-key ergoemacs-keymap ergoemacs-kill-word-key 'kill-word)
  
  ;; Copy Cut Paste, Paste previous
  (define-key ergoemacs-keymap ergoemacs-kill-region-key 'cut-line-or-region)
  (define-key ergoemacs-keymap ergoemacs-kill-ring-save-key 'copy-line-or-region)
  (define-key ergoemacs-keymap ergoemacs-yank-key 'yank)
  (define-key ergoemacs-keymap ergoemacs-yank-pop-key 'yank-pop)
  (define-key ergoemacs-keymap ergoemacs-copy-all-key 'copy-all)
  (define-key ergoemacs-keymap ergoemacs-cut-all-key 'cut-all)
  
  ;; undo and redo
  (define-key ergoemacs-keymap ergoemacs-redo-key 'redo)
  (define-key ergoemacs-keymap ergoemacs-undo-key 'undo)
  
  ;; Kill line
  (define-key ergoemacs-keymap ergoemacs-kill-line-key 'kill-line)
  (define-key ergoemacs-keymap ergoemacs-kill-line-backward-key 'kill-line-backward)
  
  ;; Textual Transformation
  
  (define-key ergoemacs-keymap ergoemacs-mark-paragraph-key 'mark-paragraph)
  (define-key ergoemacs-keymap ergoemacs-shrink-whitespaces-key 'shrink-whitespaces)
  (define-key ergoemacs-keymap ergoemacs-comment-dwim-key 'comment-dwim)
  (define-key ergoemacs-keymap ergoemacs-toggle-letter-case-key 'toggle-letter-case)
  
  ;; keyword completion, because Alt+Tab is used by OS
  (define-key ergoemacs-keymap ergoemacs-call-keyword-completion-key 'call-keyword-completion)
  
  ;; Hard-wrap/un-hard-wrap paragraph
  (define-key ergoemacs-keymap ergoemacs-compact-uncompact-block-key 'compact-uncompact-block)
  
  ;; EMACS'S SPECIAL COMMANDS
  
  ;; Cancel
  (define-key ergoemacs-keymap ergoemacs-keyboard-quit-key 'keyboard-quit)
  
  ;; Mark point.
  (define-key ergoemacs-keymap ergoemacs-set-mark-command-key 'set-mark-command)
  
  (define-key ergoemacs-keymap ergoemacs-execute-extended-command-key 'execute-extended-command)
  (define-key ergoemacs-keymap ergoemacs-shell-command-key 'shell-command)
  
  ;; WINDOW SPLITING
  (define-key ergoemacs-keymap ergoemacs-move-cursor-next-pane-key 'move-cursor-next-pane)
  (define-key ergoemacs-keymap ergoemacs-move-cursor-previous-pane-key 'move-cursor-previous-pane)
  
  
  ;; --------------------------------------------------
  ;; OTHER SHORTCUTS
  
  (define-key ergoemacs-keymap ergoemacs-switch-to-previous-frame-key 'switch-to-previous-frame)
  (define-key ergoemacs-keymap ergoemacs-switch-to-next-frame-key 'switch-to-next-frame)
  
  (define-key ergoemacs-keymap ergoemacs-query-replace-key 'query-replace)
  (define-key ergoemacs-keymap ergoemacs-query-replace-regexp-key 'query-replace-regexp)
  
  (define-key ergoemacs-keymap ergoemacs-delete-other-windows-key 'delete-other-windows)
  (define-key ergoemacs-keymap ergoemacs-delete-window-key 'delete-window)
  
  (define-key ergoemacs-keymap ergoemacs-split-window-vertically-key 'split-window-vertically)
  (define-key ergoemacs-keymap ergoemacs-split-window-horizontally-key 'split-window-horizontally)
  
  (define-key ergoemacs-keymap ergoemacs-extend-selection-key 'extend-selection)
  (define-key ergoemacs-keymap ergoemacs-select-text-in-quote-key 'select-text-in-quote)
  
  ;; --------------------------------------------------
  ;; extra keys not dependent on keyboard layout
  
  (require 'lookup-word-on-internet nil "NOERROR"))

;; --------------------------------------------------------------------------------
;; Keyboard Settings

;; Svg heavily modified from
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
  (let ((dir (file-name-directory
              (or
               load-file-name
               (buffer-file-name))))
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
      (setq extra-dir (expand-file-name "extra" dir))
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
  (let ((dir (file-name-directory
              (or
               load-file-name
               (buffer-file-name))))
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
      (setq extra-dir (expand-file-name "extra" dir))
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
  "Generate SVGs for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (mapc
     (lambda(x)
       (message "Generate ahk for %s" x)
       (ergoemacs-gen-ahk x))
     lay)))

(defun ergoemacs-extras ( &optional layouts)
  "Generate extra things (autohotkey scripts, svg diagrams etc.) from keyboard layouts."
  (interactive)
  (ergoemacs-svgs layouts)
  (ergoemacs-ahks layouts)
  (ergoemacs-bashs layouts))

(defun ergoemacs-gen-svg (layout &optional file-name extra)
  "Generates a SVG picture of the layout
FILE-NAME represents the SVG template
EXTRA represents an extra file representation."
  (let ((dir (file-name-directory
              (or
               load-file-name
               (buffer-file-name))))
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
                `(,(if (string-match "-S-\\([a-z]\\)\\>" (nth 0 x))
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
      (setq extra-dir (expand-file-name "extra" dir))
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




;;; --------------------------------------------------
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
       ;; If region is active, region is cancelled if key is unshifted
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

;;----------------------------------------------------------------------
;; ErgoEmacs hooks

(defcustom ergoemacs-minor-mode-layout
  '(;; Key/variable command x-hook
    ;; Minibuffer hook
    (minibuffer-setup-hook
     ((ergoemacs-keyboard-quit-key minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      (ergoemacs-previous-line-key previous-history-element minor-mode-overriding-map-alist)
      (ergoemacs-next-line-key next-history-element minor-mode-overriding-map-alist)
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
      
      (ergoemacs-keyboard-quit-key isearch-abort isearch-mode-map)
      (ergoemacs-isearch-forward-key isearch-repeat-forward isearch-mode-map)
      (ergoemacs-isearch-backward-key isearch-repeat-backward isearch-mode-map)
      (ergoemacs-recenter-key recenter isearch-mode-map)
      (ergoemacs-yank-key isearch-yank-kill isearch-mode-map)
      
      ;; CUA paste key is isearch-yank-kill in isearch mode
      ("C-v" isearch-yank-kill isearch-mode-map)
      
      ;; isearch-other-control-char sends the key to the original buffer and cancels isearch
      (ergoemacs-kill-ring-save-key isearch-other-control-char isearch-mode-map)
      (ergoemacs-kill-word-key isearch-other-control-char isearch-mode-map)
      (ergoemacs-backward-kill-word-key isearch-other-control-char isearch-mode-map)
      
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
     ((ergoemacs-move-beginning-of-line-key eshell-bol minor-mode-overriding-map-alist)
      ("<home>" eshell-bol minor-mode-overriding-map-alist)
      ("<f11>" eshell-previous-matching-input-from-input minor-mode-overriding-map-alist)
      ("<f12>" eshell-next-matching-input-from-input minor-mode-overriding-map-alist)
      ("S-<f11>" eshell-previous-matching-input-from-input minor-mode-overriding-map-alist)
      ("S-<f12>" eshell-next-matching-input-from-input minor-mode-overriding-map-alist)))
    
    ;; Iswitchdb hook
    (iswitchb-minibuffer-setup-hook
     ((ergoemacs-keyboard-quit-key minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      (ergoemacs-isearch-backward-key iswitchb-prev-match minor-mode-overriding-map-alist)
      (ergoemacs-isearch-forward-key iswitchb-next-match minor-mode-overriding-map-alist)
      
      ("<f11>" iswitchb-prev-match minor-mode-overriding-map-alist)
      ("<f12>" iswitchb-next-match minor-mode-overriding-map-alist)
      ("S-<f11>" iswitchb-prev-match minor-mode-overriding-map-alist)
      ("S-<f12>" iswitchb-next-match minor-mode-overriding-map-alist)))
    
    ;; Ido minibuffer setup hook
    (ido-minibuffer-setup-hook
     ((ergoemacs-keyboard-quit-key minibuffer-keyboard-quit minor-mode-overriding-map-alist)
      (ergoemacs-forward-char-key ido-next-match minor-mode-overriding-map-alist)
      (ergoemacs-backward-char-key ido-prev-match minor-mode-overriding-map-alist)
      (ergoemacs-previous-line-key ido-next-match-dir minor-mode-overriding-map-alist)
      (ergoemacs-next-line-key ido-prev-match-dir minor-mode-overriding-map-alist)
      ("<f11>" previous-history-element minor-mode-overriding-map-alist)
      ("<f12>" next-history-element minor-mode-overriding-map-alist)
      ("S-<f11>" previous-matching-history-element minor-mode-overriding-map-alist)
      ("S-<f12>" next-matching-history-element minor-mode-overriding-map-alist)))
    ;; Auto-complete-mode-hook
    ;; When the `auto-complete-mode' is on, and when a word completion
    ;; is in process, Ctrl+s does `ac-isearch'.
    ;; This fixes it.
    (auto-complete-mode-hook
     ((ergoemacs-isearch-forward-key ac-isearch ac-completing-map)
      ("C-s" nil ac-completing-map))))
  "Key bindings that are applied as hooks to specific modes"
  :type '(repeat
          (list :tag "Keys for a particular minor/major mode")
          (symbol :tag "Hook for mode")
          (repeat
           (list :tag "Key"
                 (choice
                  (symbol :tag "Defined Ergoemacs Variable")
                  (string :tag "Kbd Code"))
                 (choice
                  (function :tag "Function to Run")
                  (cost :tag "Unbind Key" nil))
                 
                 (symbol :tag "Keymap to Modify"))))
  :group 'ergoemacs-keybindings)

(defmacro ergoemacs-create-hook-function (hook keys)
  "Creates a hook function based on the HOOK and the list of KEYS defined."
  (let ((is-override (make-symbol "is-override")))
    (setq is-override (eq 'minor-mode-overriding-map-alist (nth 2 (nth 0 keys))))
    `(progn
       ,(if is-override
            `(progn
               (defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")) nil
                 ,(concat "Ergoemacs overriding keymap for `" (symbol-name hook) "'"))
               (setq ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")) (copy-keymap ergoemacs-keymap)))
          nil)
       (defun ,(intern (concat "ergoemacs-" (symbol-name hook))) ()
         ,(concat "Hook for `" (symbol-name hook) "' so ergoemacs keybindings are not lost.
This is an automatically generated function derived from `ergoemacs-minor-mode-layout'.")
         ,@(mapcar
            (lambda(def)
              `(define-key ,(if is-override
                                (intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))
                              (nth 2 def))
                 ,(if (eq 'string (type-of (nth 0 def)))
                      `(kbd ,(nth 0 def))
                    (nth 0 def))
                 ',(nth 1 def)))
            keys)
         ,(if is-override
              `(add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
                            nil (lambda (x y)
                                  (equal (car y) (car x))))
            nil)
         t)
       (ergoemacs-add-hook ',hook ',(intern (concat "ergoemacs-" (symbol-name hook)))))))



(defvar ergoemacs-hook-list (list)
  "List of hook and hook-function pairs.")

(defun ergoemacs-add-hook (hook hook-function)
  "Adds a pair of hook and hook-function to the list
ergoemacs hooks."
  (add-to-list 'ergoemacs-hook-list (cons hook hook-function)))

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

(defun ergoemacs-setup-keys ()
  "Setups keys based on a particular layout. Based on `ergoemacs-keyboard-layout'"
  (interactive)
  (let ((ergoemacs-state ergoemacs-mode)
        (cua-state cua-mode)
        (layout
         (intern-soft
          (concat "ergoemacs-layout-" ergoemacs-keyboard-layout))))
    (when ergoemacs-state
      (ergoemacs-mode -1)
      (when cua-state
        (cua-mode -1)))
    (cond
     (layout
      (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout))
     (t ; US qwerty by default
      (ergoemacs-setup-keys-for-layout "us")))
    (ergoemacs-create-hooks)
    (when ergoemacs-state
      (ergoemacs-mode 1)
      (when cua-state
        (cua-mode 1)))))

(ergoemacs-setup-keys)

(defun ergoemacs-hook-modes ()
  "Installs/Removes ErgoEmacs minor mode hooks from major modes
depending the state of `ergoemacs-mode' variable.  If the mode
is being initialized, some global keybindings in current-global-map
will change."
  
  (let ((modify-hook (if ergoemacs-mode 'add-hook 'remove-hook))
        (modify-advice (if ergoemacs-mode 'ad-enable-advice 'ad-disable-advice)))
    
    ;; Fix CUA
    (if ergoemacs-mode
        (ergoemacs-fix-cua--pre-command-handler-1))
    
    ;; when ergoemacs-mode is on, activate hooks and unset global keys, else do inverse
    (if (and ergoemacs-mode (not (equal ergoemacs-mode 0)))
        (progn
          (ergoemacs-unset-redundant-global-keys)
          
          ;; alt+n is the new "Quit" in query-replace-map
          (ergoemacs-unset-global-key query-replace-map "\e")
          (define-key query-replace-map ergoemacs-keyboard-quit-key 'exit-prefix))
      ;; if ergoemacs was disabled: restore original keys
      (ergoemacs-restore-global-keys))
    
    ;; install the mode-hooks
    (dolist (hook ergoemacs-hook-list)
      (funcall modify-hook (car hook) (cdr hook)))
    
    ;; enable advices
    (funcall modify-advice 'global-set-key 'around 'ergoemacs-global-set-key-advice)
    (funcall modify-advice 'global-unset-key 'around 'ergoemacs-global-unset-key-advice)
    (funcall modify-advice 'local-set-key 'around 'ergoemacs-local-set-key-advice)
    (funcall modify-advice 'local-unset-key 'around 'ergoemacs-local-unset-key-advice)
    
    ;; update advices
    (ad-activate 'global-set-key)
    (ad-activate 'global-unset-key)
    (ad-activate 'local-set-key)
    (ad-activate 'local-unset-key)))

;;----------------------------------------------------------------------
;; ErgoEmacs replacements for local- and global-set-key

(defun ergoemacs-global-set-key (key command)
  "Set a key in the ergoemacs-keymap, thus
making it globally active. This allow to redefine
any key unbound or claimed by ergoemacs."
  (interactive)
  (define-key ergoemacs-keymap key command))

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
      (setq ergoemacs-local-keymap (copy-keymap ergoemacs-keymap))
      (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-local-keymap)))
    ;; add key
    (define-key ergoemacs-local-keymap key command)))

(defun ergoemacs-local-unset-key (key)
  "Unset a key in the ergoemacs local map."
  (ergoemacs-local-set-key key nil))

;;----------------------------------------------------------------------
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

;;----------------------------------------------------------------------
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
  :lighter " ErgoEmacs"	;; TODO this should be nil (it is for testing purposes)
  :global t
  :keymap ergoemacs-keymap
  
  (ergoemacs-hook-modes))

;;; Customizable settings
;; Load the keyboard layout looking the ERGOEMACS_KEYBOARD_LAYOUT
;; enviroment variable (this variable is set by ErgoEmacs runner)


(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
