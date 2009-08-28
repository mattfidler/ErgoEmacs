;-*- coding: utf-8 -*-
;; ergonomic_keybinding_qwerty.el -- A ergonomic keybinding for qwerty keyboard.

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
;; Thanks to marciomazza for spotting several default bindings that should have been be unbound.
;; Thanks to those who have created and improved the version for Colemak layout. They are (by date): “vockets”, “postivan”, Graham Poulter.
;; Thanks to lwarxx for bug report on diff-mode
;; Thanks to many users who send in comments and appreciations on this.


;;; History:

;-*- coding: utf-8 -*-

;; Note: When the context is about keys on the keyboard hardware, then QWERTY is used to indicate the key. For example, “Changed M+y to something”, that “y” means the key that is under the key 7. (which is Dvorak's “f”).

;; version 4.3.12, 2009-08-13 • C-p is now “recenter-top-bottom” for emacs 23 users. In emacs 22, it is “recenter”.

;; version 4.3.11, 2009-08-05 • Added a hook to fix message-mode.

;; version 4.3.10, 2009-06-14 • fixed a previous/next reversal for f11/f12 keys in rcirc-mode-hook. • diff-mode uses some bindings in M-‹key› space. Fixed by adding a diff-mode-hook. (thanks to lwarxx)

;; version 4.3.9, 2009-06-14 • unbind C-x 3 (use Alt+@ for split-window-horizontally) • unbind C-M-% (use Alt+% for query-replace-regexp).  • unbind C-@ (use Alt+SPACE for set-mark-command).  • unbind M-{ (use Alt+U for backward-paragraph).  • unbind M-} (use Alt+O for forward-paragraph). Thanks to marciomazza

;; version 4.3.8, 2009-06-14 • added 【】 to select-text-in-quote.

;; version 4.3.7, 2009-05-27 • ibuffer's M-s changed in emacs 23, such that M-s is now a prefix. For Dvorak users, M-s should be isearch. This is reclaimed to be isearch. For qwerty users, M-s should be other-window. Fixed.

;; version 4.3.6, 2009-05-26 • dired's M-s changed in emacs 23, such that M-s is now a prefix. For Dvorak users, M-s should be isearch. This is reclaimed to be isearch. For qwerty users, M-s should be other-windox. Fixed.

;; version 4.3.5, 2009-05-10 • experimental modification to shrink-whitespaces.

;; version 4.3.4, 2009-04-19 • Added Alt+Shift+PageDown for forward-page (move cursor to the next ascii formfeed char (often displayed as ^L)). Same for PageDown key.

;; version 4.3.3, 2009-03-16 • Added Ctrl+Shift+n for opening a new window (make-frame-command). Removed “C-x 5 2”. Added Ctrl+Shift+w for closing window (delete-frame), removed “C-x 5 0”.

;; version 4.3.2, 2009-03-11 • Removed C-x o for other-window. (use Alt+s and Alt+Shift+s instead.)

;; version 4.3.1, 2009-03-04 • A better implementation for making M-t call M-TAB. So that, describe-key on M-t will show better documentation.

;; version 4.3.0, 2009-03-02 • Added Alt+Shift+x for cut-all and Alt+Shift+c for copy-all.

;; version 4.2.4, 2009-03-01 • Removed Ctrl+x Ctrl+w for write-file. Use Ctrl+Shift+s. • Fixed Ctrl+o for ibuffer to run find-file instead of ibuffer-visit-buffer-other-window-noselect.

;; version 4.2.3, 2009-03-01 • fixed a dired binding so that Ctrl+o runs find-file instead of dired-display-file

;; version 4.2.2, 2009-03-01 • Removed the default keybinding for split-window-vertically (Ctrl+x 2). Use Alt+2 instead. • The key “Ctrl+x Ctrl+b” is now ibuffer, instead of list-buffers. • “Ctrl+h m” now calls describe-major-mode.

;; version 4.2.1, 2009-02-28 Changed the keybinding for previous-user-buffer from Ctrl+← to Ctrl+PageUp. And previous-emacs-buffer from Ctrl+Shift+← to Ctrl+Shift+PageUp. Similar for the “next” versions. Rationale: The Ctrl+Arrow are standard editing shortcuts commond in Windows, Mac, Linux.

;; version 4.2.0, 2009-02-27 Added Alt+t for keyword completion. Rationale: The default M-TAB is used by Windows, Mac, Linux, to switch apps.

;; version 4.1.8, 2009-02-15 Corrected the keybinding for Save As (write-file)

;; version 4.1.7, 2008-12-10 Made compact-uncompact-block to work on text selection if there's one.

;; version 4.1.6, 2008-12-09. Added select-text-in-quote.

;; version 4.1.5, 2008-10-21. Added extend-selection by Nikolaj Schumacher.

;; version 4.1.4, 2008-10-20. Fixed close-current-buffer: sometimes when closing a buffer not associated with a file, it'll prompt whether to kill instead of whether to save.

;; version 4.1.3, 2008-10-18. Fixed minor bug in toggle-letter-case. It now works non-english letters such as éÉ and single letter word “a teapot” or words starting with a number “1st timer”. Thanks to Andreas Politz and Nikolaj Schumacher. • next-frame-command is renamed to switch-to-next-frame. Similar for previous-frame-command.

;; version 4.1.2, 2008-10-16. Removed C-x h for mark-whole-buffer. (use Ctrl+a instead) Improved compact-uncompact-block and shrink-whitespaces.

;; version 4.1.1, 2008-10-07. Removed unfill-paragraph and unfill-region because they are defined in ourcomments-util.el bundled with emacs 22, also because they are not used in this file now (compact-uncompact-block replaced most of their functionality).

;; version 4.1, 2008-10-06. Added keys for previous-user-buffer, next-user-buffer, next-frame-command, previous-frame-command, query-replace and query-replace-regexp, move-cursor-to-next-pane, move-cursor-to-previous-pane, split-window-horizontally, toggle-letter-case. Combined delete-blank-lines and just-one-space to shrink-whitespaces. Moved delete-window to be with delete-other-window.

;; version 4.0.1, 2008-09-23. Fixed C-o in dired mode.

;; version 4, 2008-09-21. Unbind almost all Meta-‹key› and Ctrl-‹key› space bindings. Added about 11 commands, such as next-user-buffer, close-current-buffer etc.

;; version 3.6, 2008-09-18. Reclaimed keybindings in text-mode.

;; version 3.5, 2008-09-16. Reclaimed keybindings in ibuffer.

;; version 3.4, 2008-09-06. Fixed key bindings in the Meta-‹key› space in about 10 modes.

;; version 3.3, 2008-09-05. Fixed cua-mode-hook by setting symbol property 'CUA to nil, so that a bunch of no-select-* functions kludge is no longer needed. Thanks to Lennart Borgman.

;; version 3.2, 2008-09-02. Moved cua fix functions to modern_operations.el. The functinos are: no-select-cua-scroll-down, no-select-cua-scroll-up, no-select-backward-paragraph, no-select-forward-paragraph, no-select-beginning-of-buffer, no-select-end-of-buffer, no-select-move-end-of-line.

;; version 3.1, 2008-09-02. Added just-one-space, delete-blank-lines. Added fill-paragraph, unfill-paragraph. Added comment-dwim.

;; version 3, 2008-08-31. Added isearch. Added redo, kill-line-backward, shell-command. Added bug fix for cua-mode. Now, commands with Shift keys won't go into a selection mode.

;; version 2, 2008-08-29. Somewhat major change. Positions for undo, cut, copy, paste, paste previous, has been moved. Added delete-char, delete-backward-char, kill-word, backward-kill-word. Removed the now redundant binding of kill-word and backward-kill-word using the backspace key. Removed the other-frame binding.

;; version 1.1, 2007-12-18. changed keycode to consistantly use kbd syntax. Fixed a scroll-up and scroll-down mixup.

;; version 1.0, 2007-08-01. first version.


;;; Code:

;; --------------------------------------------------
;;; UNBIND DEFAULT KEYS

;-*- coding: utf-8 -*-
(global-unset-key (kbd "M-1")) ; digit-argument
(global-unset-key (kbd "M-2")) ; digit-argument
(global-unset-key (kbd "M-3")) ; digit-argument
(global-unset-key (kbd "M-4")) ; digit-argument
(global-unset-key (kbd "M-5")) ; digit-argument
(global-unset-key (kbd "M-6")) ; digit-argument
(global-unset-key (kbd "M-7")) ; digit-argument
(global-unset-key (kbd "M-8")) ; digit-argument
(global-unset-key (kbd "M-9")) ; digit-argument
(global-unset-key (kbd "M-0")) ; digit-argument

(global-unset-key (kbd "M-a")) ; backward-sentence
(global-unset-key (kbd "M-b")) ; backward-word
(global-unset-key (kbd "M-c")) ; capitalize-word
(global-unset-key (kbd "M-d")) ; kill-word
(global-unset-key (kbd "M-e")) ; forward-sentence
(global-unset-key (kbd "M-f")) ; forward-word
(global-unset-key (kbd "M-g")) ; (prefix)
(global-unset-key (kbd "M-h")) ; mark-paragraph
(global-unset-key (kbd "M-i")) ; tab-to-tab-stop
(global-unset-key (kbd "M-j")) ; indent-new-comment-line
(global-unset-key (kbd "M-k")) ; kill-sentence
(global-unset-key (kbd "M-l")) ; downcase-word
(global-unset-key (kbd "M-m")) ; back-to-indentation
(global-unset-key (kbd "M-n")) ; nil
(global-unset-key (kbd "M-o")) ; nil
(global-unset-key (kbd "M-p")) ; nil
(global-unset-key (kbd "M-q")) ; fill-paragraph
(global-unset-key (kbd "M-r")) ; move-to-window-line
(global-unset-key (kbd "M-s")) ; nil
(global-unset-key (kbd "M-t")) ; transpose-words
(global-unset-key (kbd "M-u")) ; upcase-word
(global-unset-key (kbd "M-v")) ; scroll-down
(global-unset-key (kbd "M-w")) ; kill-ring-save
(global-unset-key (kbd "M-x")) ; execute-extended-command
(global-unset-key (kbd "M-y")) ; yank-pop
(global-unset-key (kbd "M-z")) ; zap-to-char

(global-unset-key (kbd "M-\\")) ; delete-horizontal-space
(global-unset-key (kbd "M-@")) ; mark-word
(global-unset-key (kbd "M--")) ; negative-argument
(global-unset-key (kbd "M-<")) ; beginning-of-buffer
(global-unset-key (kbd "M->")) ; end-of-buffer
(global-unset-key (kbd "M-{")) ; backward-paragraph
(global-unset-key (kbd "M-}")) ; forward-paragraph

(global-unset-key (kbd "C-1")) ; digit-argument
(global-unset-key (kbd "C-2")) ; digit-argument
(global-unset-key (kbd "C-3")) ; digit-argument
(global-unset-key (kbd "C-4")) ; digit-argument
(global-unset-key (kbd "C-5")) ; digit-argument
(global-unset-key (kbd "C-6")) ; digit-argument
(global-unset-key (kbd "C-7")) ; digit-argument
(global-unset-key (kbd "C-8")) ; digit-argument
(global-unset-key (kbd "C-9")) ; digit-argument
(global-unset-key (kbd "C-0")) ; digit-argument

(global-unset-key (kbd "C-a")) ; move-beginning-of-line
(global-unset-key (kbd "C-b")) ; backward-char
;(global-unset-key (kbd "C-c")) ; (prefix)
(global-unset-key (kbd "C-d")) ; delete-char
(global-unset-key (kbd "C-e")) ; move-end-of-line
(global-unset-key (kbd "C-f")) ; forward-char
;(global-unset-key (kbd "C-g")) ; keyboard-quit
;(global-unset-key (kbd "C-h")) ; (prefix)
;(global-unset-key (kbd "C-i")) ; indent-for-tab-command; this is tab key
(global-unset-key (kbd "C-j")) ; newline-and-indent
(global-unset-key (kbd "C-k")) ; kill-line
(global-unset-key (kbd "C-l")) ; recenter
;(global-unset-key (kbd "C-m")) ; newline-and-indent; This is the Return key
(global-unset-key (kbd "C-n")) ; next-line
(global-unset-key (kbd "C-o")) ; open-line
(global-unset-key (kbd "C-p")) ; previous-line
;(global-unset-key (kbd "C-q")) ; quote-insert
(global-unset-key (kbd "C-r")) ; isearch-backward
(global-unset-key (kbd "C-s")) ; isearch-forward
(global-unset-key (kbd "C-t")) ; transpose-chars
;(global-unset-key (kbd "C-u")) ; universal-argument
(global-unset-key (kbd "C-v")) ; scroll-up
(global-unset-key (kbd "C-w")) ; kill-region
;(global-unset-key (kbd "C-x")) ; (prefix)
(global-unset-key (kbd "C-y")) ; yank
(global-unset-key (kbd "C-z")) ; iconify-or-deiconify-frame

(global-unset-key (kbd "C-/")) ; undo
(global-unset-key (kbd "C-_")) ; undo
(global-unset-key (kbd "C-<backspace>")) ; backward-kill-word

(global-unset-key (kbd "C-@")) ; cua-set-mark set-mark-command

(global-unset-key (kbd "C-<prior>")) ; scroll-right
(global-unset-key (kbd "C-<next>")) ; scroll-left

(global-unset-key (kbd "C-x d")) ; dired
(global-unset-key (kbd "C-x h")) ; mark-whole-buffer

(global-unset-key (kbd "C-x C-d")) ; list-directory
(global-unset-key (kbd "C-x C-f")) ; find-file
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(global-unset-key (kbd "C-x C-w")) ; write-file

(global-unset-key (kbd "C-x 0")) ; delete-window
(global-unset-key (kbd "C-x 1")) ; delete-other-windows
(global-unset-key (kbd "C-x 2")) ; split-window-vertically
(global-unset-key (kbd "C-x 3")) ; split-window-horizontally
(global-unset-key (kbd "C-x o")) ; other-windows

(global-unset-key (kbd "C-x 5 0")) ; delete-frame
(global-unset-key (kbd "C-x 5 2")) ; make-frame-command

(global-unset-key (kbd "C-M-%")) ; query-replace-regexp



;;; --------------------------------------------------
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)

;; Move by word
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word) ; was (prefix)

;; Move by paragraph
(global-set-key (kbd "M-U") 'backward-paragraph)
(global-set-key (kbd "M-O") 'forward-paragraph)

;; Move to beginning/ending of line
(global-set-key (kbd "M-h") 'move-beginning-of-line)
(global-set-key (kbd "M-H") 'move-end-of-line)

;; Move by screen (page up/down)
(global-set-key (kbd "M-I") 'scroll-down)
(global-set-key (kbd "M-K") 'scroll-up)

;; Move to beginning/ending of file
(global-set-key (kbd "M-J") 'beginning-of-buffer)
(global-set-key (kbd "M-L") 'end-of-buffer)

;; isearch
(global-set-key (kbd "M-;") 'isearch-forward)
(global-set-key (kbd "M-:") 'isearch-backward)

(global-set-key (kbd "M-p")
                (if (functionp 'recenter-top-bottom)
                    'recenter-top-bottom
                  'recenter
                  ))

;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "M-f") 'delete-char)

; Delete previous/next word.
(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)

; Copy Cut Paste, Paste previous
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-V") 'yank-pop)
(global-set-key (kbd "M-C") 'copy-all)
(global-set-key (kbd "M-X") 'cut-all)

;; undo and redo
(global-set-key (kbd "M-Z") 'redo)
(global-set-key (kbd "M-z") 'undo)

; Kill line
(global-set-key (kbd "M-g") 'kill-line)
(global-set-key (kbd "M-G") 'kill-line-backward)

;;; Textual Transformation

(global-set-key (kbd "M-S-SPC") 'mark-paragraph)
(global-set-key (kbd "M-w") 'shrink-whitespaces)
(global-set-key (kbd "M-'") 'comment-dwim)
(global-set-key (kbd "M-/") 'toggle-letter-case)

; keyword completion, because Alt+Tab is used by OS
(global-set-key (kbd "M-t") 'call-keyword-completion) 

; Hard-wrap/un-hard-wrap paragraph
(global-set-key (kbd "M-q") 'compact-uncompact-block)

;;; EMACS'S SPECIAL COMMANDS

; Mark point.
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-A") 'shell-command)

;;; WINDOW SPLITING
(global-set-key (kbd "M-s") 'move-cursor-next-pane)
(global-set-key (kbd "M-S") 'move-cursor-previous-pane)

;;; --------------------------------------------------
;;; STANDARD SHORTCUTS

;-*- coding: utf-8 -*-

(setq mac-pass-command-to-system nil) ; so that Cmd+H won't activate Hide Current App and Cmd+Shift+q won't logout user.

(global-set-key (kbd "C-n") 'new-empty-buffer) ; Open New File
(global-set-key (kbd "C-S-n") 'make-frame-command) ; open a new window.
(global-set-key (kbd "C-o") 'find-file) ; Open
(global-set-key (kbd "C-w") 'close-current-buffer) ; Close
(global-set-key (kbd "C-s") 'save-buffer) ; Save
(global-set-key (kbd "C-S-s") 'write-file) ; Save As.
(global-set-key (kbd "C-p") 'print-buffer) ; Print
(global-set-key (kbd "C-a") 'mark-whole-buffer) ; Select All
(global-set-key (kbd "C-S-w") 'delete-frame) ; close Window.

(global-set-key (kbd "<delete>") 'delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.

(global-set-key (kbd "M-~") 'switch-to-previous-frame)
(global-set-key (kbd "M-`") 'switch-to-next-frame)

(global-set-key (kbd "C-<prior>") 'previous-user-buffer)
(global-set-key (kbd "C-<next>") 'next-user-buffer)

(global-set-key (kbd "C-S-<prior>") 'previous-emacs-buffer)
(global-set-key (kbd "C-S-<next>") 'next-emacs-buffer)

(global-set-key (kbd "M-S-<prior>") 'backward-page)
(global-set-key (kbd "M-S-<next>") 'forward-page)

(global-set-key (kbd "M-5") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-!") 'delete-window)

(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-@") 'split-window-horizontally)

(global-set-key (kbd "M-8") 'extend-selection)
(global-set-key (kbd "M-*") 'select-text-in-quote)


(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-h m") 'describe-major-mode)

(add-hook 'w3m-mode-hook
 (lambda ()
  (define-key w3m-mode-map (kbd "<up>") 'previous-line) ; was w3m-previous-anchor. Use Shift+Tab.
  (define-key w3m-mode-map (kbd "<down>") 'next-line) ; was w3m-next-anchor. Use Tab.
  (define-key w3m-mode-map (kbd "<left>") 'backward-char) ; was w3m-view-previous-page. Use B.
  (define-key w3m-mode-map (kbd "<right>") 'forward-char) ; was w3m-view-this-url. Use Enter.
))


(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-o") 'find-file) ; was dired-display-file
 ))


;;; --------------------------------------------------
;;; RECLAIM SOME BINDINGS

;; isearch
(add-hook 'isearch-mode-hook
 (lambda ()
 (define-key isearch-mode-map (kbd "M-;") 'isearch-repeat-forward)
 (define-key isearch-mode-map (kbd "M-:") 'isearch-repeat-backward)

 (define-key isearch-mode-map (kbd "M-p") 'recenter) ; was isearch-ring-retreat
 (define-key isearch-mode-map (kbd "M-n") 'nil) ; was isearch-ring-advance
 (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
 (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)

 (define-key isearch-mode-map (kbd "M-y") 'nil) ; was isearch-yank-kill

 (define-key isearch-mode-map (kbd "M-c") 'kill-ring-save) ; was isearch-toggle-case-fold
 (define-key isearch-mode-map (kbd "M-r") 'kill-word) ; was isearch-toggle-regexp
 (define-key isearch-mode-map (kbd "M-e") 'backward-kill-word) ; was isearch-edit-string
 )
)

;; reclaim some bindings used in minibuffer
(define-key minibuffer-local-map (kbd "M-p") 'recenter) ; was previous-history-element. Use ↑ key or f11.
(define-key minibuffer-local-map (kbd "M-n") 'nil) ; was next-history-element. Use ↓ key or f12.
(define-key minibuffer-local-map (kbd "M-r") 'kill-word) ; was previous-matching-history-element.
(define-key minibuffer-local-map (kbd "M-s") 'other-window) ; was nest-matching-history-element

(define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
(define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
(define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
(define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element)


;; reclaim some binding used by shell mode and shell-command.
;; the shell mode and associated mode and commands use keys in comint-mode-map.
(add-hook 'comint-mode-hook
 (lambda ()
   (define-key comint-mode-map (kbd "M-p") 'recenter) ; was comint-previous-input. Use Ctrl+↑ or f11
   (define-key comint-mode-map (kbd "M-n") 'nil) ; was comint-next-input. Use Ctrl+↓ or f12
   (define-key comint-mode-map (kbd "M-r") 'kill-word) ; was comint-previous-matching-input.
   (define-key comint-mode-map (kbd "M-s") 'other-window) ; was comint-next-matching-input.

   (define-key comint-mode-map (kbd "<f11>") 'comint-previous-input)
   (define-key comint-mode-map (kbd "<f12>") 'comint-next-input)
   (define-key comint-mode-map (kbd "S-<f11>") 'comint-previous-matching-input)
   (define-key comint-mode-map (kbd "S-<f12>") 'comint-next-matching-input)
))

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-n") 'new-empty-buffer) ; was dired-next-line
  (define-key dired-mode-map (kbd "M-o") 'forward-word) ; was dired-omit-mode
  (define-key dired-mode-map (kbd "M-s") 'other-window) ; was prefix in emacs 23.
 ))

(add-hook 'Info-mode-hook
 (lambda ()
 (define-key Info-mode-map (kbd "M-n") 'nil) ; was clone-buffer
 (define-key Info-mode-map (kbd "M-s") 'other-window) ; was Info-search; just press “s” instead for isearch-forward
 )
)

(add-hook 'text-mode-hook
 (lambda ()
 (define-key text-mode-map (kbd "M-s") 'other-window) ; was center-line
 (define-key text-mode-map (kbd "M-S") 'nil) ; was center-paragraph
 )
)

;; prevent cua-mode from going into selection mode when commands with Shift key is used.
(add-hook 'cua-mode-hook
 (lambda ()
    (put 'cua-scroll-down 'CUA nil)
    (put 'cua-scroll-up 'CUA nil)
    (put 'backward-paragraph 'CUA nil)
    (put 'forward-paragraph 'CUA nil)
    (put 'beginning-of-buffer 'CUA nil)
    (put 'end-of-buffer 'CUA nil)
    (put 'move-end-of-line 'CUA nil)
   )
 )


;; reclaim some binding used by ibuffer.el
(add-hook 'ibuffer-mode-hook
 (lambda ()
   (define-key ibuffer-mode-map (kbd "M-s") 'other-window) ; was prefix in emacs 23.
   (define-key ibuffer-mode-map (kbd "M-g") 'kill-line) ; was ibuffer-jump-to-buffer. Use “j” instead.
   (define-key ibuffer-mode-map (kbd "M-p") 'recenter) ; was ibuffer-backward-filter-group Use “←” instead.
   (define-key ibuffer-mode-map (kbd "M-n") 'nil) ; was ibuffer-forward-filter-group. Use “→” instead.
   (define-key ibuffer-mode-map (kbd "M-j") 'backward-char) ; was ibuffer-jump-to-filter-group.
   (define-key ibuffer-mode-map (kbd "M-o") 'forward-word) ; was ibuffer-visit-buffer-1-window
))

(add-hook 'html-mode-hook
 (lambda ()
 (define-key html-mode-map (kbd "M-s") 'other-window)
 )
)

(add-hook 'nxml-mode-hook
 (lambda ()
 (define-key nxml-mode-map (kbd "M-h") 'move-beginning-of-line) ; was nxml-mark-paragraph
 (define-key nxml-mode-map (kbd "C-M-SPC") 'nxml-mark-paragraph)
 )
)

(add-hook 'diff-mode-hook
 (lambda ()
  (define-key diff-mode-map (kbd "M-n") 'nil) ; was diff-hunk-next
  (define-key diff-mode-map (kbd "M-N") 'nil) ; was diff-file-next

  (define-key diff-mode-map (kbd "M-p") 'recenter) ; was diff-hunk-prev
  (define-key diff-mode-map (kbd "M-k") 'next-line) ; was diff-hunk-kill
  (define-key diff-mode-map (kbd "M-K") 'scroll-up) ; was diff-file-kill

 (define-key diff-mode-map (kbd "<f11>") 'diff-hunk-prev)
 (define-key diff-mode-map (kbd "<f12>") 'diff-hunk-next)
 (define-key diff-mode-map (kbd "S-<f11>") 'diff-file-prev)
 (define-key diff-mode-map (kbd "S-<f12>") 'diff-file-next)
 ))

(add-hook 'w3m-mode-hook
 (lambda ()
  (define-key w3m-mode-map (kbd "M-a") 'execute-extended-command) ; was w3m-bookmark-add-this-url
  (define-key w3m-mode-map (kbd "M-g") 'kill-line) ; was goto-line
  (define-key w3m-mode-map (kbd "M-n") 'nil) ; was w3m-copy-buffer
  (define-key w3m-mode-map (kbd "M-l") 'forward-char) ; was w3m-horizontal-recenter

  (define-key w3m-mode-map (kbd "M-i") 'previous-line) ; was w3m-save-image
  (define-key w3m-mode-map (kbd "M-k") 'next-line) ; was w3m-cookie
))

(add-hook 'rcirc-mode-hook
 (lambda ()
  (define-key rcirc-mode-map (kbd "M-p") 'recenter) ; was rcirc-insert-prev-input
  (define-key rcirc-mode-map (kbd "M-n") 'nil) ; was rcirc-insert-next-input
  (define-key rcirc-mode-map (kbd "<f11>") 'rcirc-insert-prev-input)
  (define-key rcirc-mode-map (kbd "<f12>") 'rcirc-insert-next-input)
 ))

(add-hook 'awk-mode-hook
 (lambda ()
  (define-key awk-mode-map (kbd "M-a") 'execute-extended-command) ; was c-beginning-of-statement
  (define-key awk-mode-map (kbd "M-e") 'backward-kill-word) ; was c-end-of-statement
 ))

(add-hook 'message-mode-hook
; M-; comment-region
; M-n message-display-abbrev
 (lambda ()
 (define-key message-mode-map (kbd "M-;") 'isearch-repeat-forward)
 ))

;; nothing to fix: c-mode, c++-mode, java, sh, js, perl, php, python

;;; --------------------------------------------------
;;; FUNCTIONS

;-*- coding: utf-8 -*-

(require 'redo "redo.elc" t) ; for redo shortcut

(delete-selection-mode t) ; turn on text selection highlighting and make typing override selected text (Note: when delete-selection-mode is on, then transient-mark-mode is automatically on too.)

(defun call-keyword-completion ()
  "Call the command that has keyboard shortcut M-TAB."
  (interactive)
  (call-interactively (key-binding (kbd "M-TAB")))
)

(defun describe-major-mode ()
  "Show inline doc for current major-mode."
  ;; code by Kevin Rodgers. 2009-02-25
  (interactive)
  (describe-function major-mode))

(defun copy-all ()
  "Put the whole buffer content into the kill-ring.
If narrow-to-region is in effect, then copy that region only."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer content copied")
  )

(defun cut-all ()
  "Cut the whole buffer content into the kill-ring.
If narrow-to-region is in effect, then cut that region only."
  (interactive)
  (kill-region (point-min) (point-max))
  (message "Buffer content cut")
  )

;;; TEXT SELECTION RELATED

(defun select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters: ()[]<>«»“”‘’「」【】, including \"\"."
 (interactive)
 (let (b1 b2)
   (skip-chars-backward "^<>(“{[「«【\"‘")
   (setq b1 (point))
   (skip-chars-forward "^<>)”}]」】»\"’")
   (setq b2 (point))
   (set-mark b1)
   )
 )

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;;; TEXT TRANSFORMATION RELATED

(defun kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending."
  (interactive)
  (if (looking-back "\n")
      (delete-char -1)
    (kill-line 0)
    )
  )

(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1)
  )

(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1)
  )

(defun compact-uncompact-block ()
  "Remove or add line endings on the current block of text.
This is similar to a toggle for fill-paragraph and unfill-paragraph
When there is a text selection, act on the region.

When in text mode, a paragraph is considerd a block. When in programing
language mode, the block defined by between empty lines.

Todo: The programing language behavior is currently not done.
Right now, the code uses fill* functions, so does not work or work well
in programing lang modes. A proper implementation to compact is replacing
EOL chars by space when the EOL char is not inside string.
"
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”, the
  ;; possible values are t and nil. This property is used to easily
  ;; determine whether to compact or uncompact, when this command is
  ;; called again

  (let (bds currentLineCharCount currentStateIsCompact
            (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; currentLineCharCount is used to determine whether current state
      ;; is compact or not, when the command is run for the first time
      (setq currentLineCharCount
            (progn
              (setq bds (bounds-of-thing-at-point 'line))
              (length (buffer-substring-no-properties (car bds) (cdr bds)))    
              ;; Note: 'line includes eol if it is not buffer's last line
              )
            )

      ;; Determine whether the text is currently compact.  when the last
      ;; command is this, then symbol property easily tells, but when
      ;; this command is used fresh, right now we use num of chars of
      ;; the cursor line as a way to define current compatness state
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> currentLineCharCount fill-column) t nil)
              )
            )

      (if (and transient-mark-mode mark-active)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end)))
            )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil))
          )
        )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact
                                              nil t)) ) ) )


(defun shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line contains non-white space chars, then shrink any whitespace char surrounding cursor to just one space.
If current line does not contain non-white space chars, then remove blank lines to just one."
  (interactive)
  (let (
        cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))

      (setq spaceTabNeighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)) t nil) )
      (goto-char cursor-point)

      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char cursor-point)      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point))
      )


    (if line-has-meat-p
        (progn 
          (when spaceTabNeighbor-p
            (delete-region space-or-tab-begin space-or-tab-end)
            (insert " "))
          )

      (progn
;;         (delete-region whitespace-begin whitespace-end)
;;         (insert "\n")
        (delete-blank-lines)
        )
      ;; todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )
    )
  )

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
(interactive)
(let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
  (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning)
            pos2 (region-end))
    (setq pos1 (car (bounds-of-thing-at-point 'word))
          pos2 (cdr (bounds-of-thing-at-point 'word))))

  (when (not (eq last-command this-command))
    (save-excursion
      (goto-char pos1)
      (cond
       ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
       ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
       ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
       (t (put this-command 'state "all lower") )
       )
      )
    )

  (cond
   ((string= "all lower" (get this-command 'state))
    (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
   ((string= "init caps" (get this-command 'state))
    (upcase-region pos1 pos2) (put this-command 'state "all caps"))
   ((string= "all caps" (get this-command 'state))
    (downcase-region pos1 pos2) (put this-command 'state "all lower"))
   )
)
)

;;; FRAME

(defun switch-to-next-frame ()
  "Select the next frame on current display, and raise it."
  (interactive)
  (other-frame 1)
  )

(defun switch-to-previous-frame ()
  "Select the previous frame on current display, and raise it."
  (interactive)
  (other-frame -1)
  )

;;; BUFFER RELATED

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.


(defvar recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable recently-closed-buffers-max.")
(defvar recently-closed-buffers-max 10 "The maximum length for recently-closed-buffers.")

(defun close-current-buffer ()
"Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• make sure the buffer shown after closing is a user buffer.
• if the buffer is a file, add the path to the list recently-closed-buffers.

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
 (interactive)
 (let (emacsBuff-p isEmacsBufferAfter)
   (if (string-match "^*" (buffer-name))
       (setq emacsBuff-p t)
     (setq emacsBuff-p nil))

   ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
   (when (and (buffer-modified-p)
              (not emacsBuff-p)
              (not (string-equal major-mode "dired-mode"))
              (if (equal (buffer-file-name) nil) 
                  (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                t
                )
              )
     (if (y-or-n-p
            (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
       (save-buffer)
       (set-buffer-modified-p nil)))

   ;; save to a list of closed buffer
   (when (not (equal buffer-file-name nil))
     (setq recently-closed-buffers
           (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
     (when (> (length recently-closed-buffers) recently-closed-buffers-max)
           (setq recently-closed-buffers (butlast recently-closed-buffers 1))
           )
     )

   ;; close
   (kill-buffer (current-buffer))

   ;; if emacs buffer, switch to a user buffer
   (if (string-match "^*" (buffer-name))
       (setq isEmacsBufferAfter t)
     (setq isEmacsBufferAfter nil))
   (when isEmacsBufferAfter
     (previous-user-buffer)
     )
   )
 )
