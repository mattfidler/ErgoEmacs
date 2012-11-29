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
;; ergoemacs-layout-sp.el Contributor: David Capello
;; ergoemacs-layout-sv.el Contributor: Kristian Hellquist
;; ergoemacs-layout-us.el Contributor: David Capello, Xah Lee
;; ergoemacs-layout-colemak.el Contributor: Ivan Haralamov ( postivan gmail.com ), “vockets”, Graham Poulter.
;; ergoemacs-layout-pt-nativo.el Contributor: Xavier Pinho

;;; --------------------------------------------------

;;; Code:

;; Add this same directory to load elisp files
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Ergoemacs-keybindings version
(defconst ergoemacs-mode-version "5.6.2"
  "Ergoemacs-keybindings minor mode version number.")

;; Include extra files
(load "functions")
(load "ergoemacs-unbind")


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

(defun ergoemacs-kbd (key)
  "Translates kbd code for layout `ergoemacs-translation-from' to kbd code for `ergoemacs-translation-to'."
  (let ((new-key key))
    (when (and ergoemacs-needs-translation
               (string-match ergoemacs-translation-regexp new-key))
      (setq new-key (replace-match (concat "-" (cdr (assoc (match-string 1 new-key) ergoemacs-translation-assoc))) t t new-key)))
    ;;(message "%s -> %s" key new-key)
    (read-kbd-macro new-key)))

(setq ergoemacs-layout
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
        
        ("M-S-SPC" ergoemacs-mark-paragraph-key )
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
        ("M-SPC" ergoemacs-set-mark-command-key)
        
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
        ))

(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  (mapc
   (lambda(x)
     (set (nth 1 x) (ergoemacs-kbd (nth 0 x))))
   ergoemacs-layout)
  ;; Single char cursor movement
  
  (setq ergoemacs-keymap (make-sparse-keymap))
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
  
;;; MAJOR EDITING COMMANDS
  
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
  ;; STANDARD SHORTCUTS
  
  (define-key ergoemacs-keymap (kbd "C-n") 'new-empty-buffer)
  (define-key ergoemacs-keymap (kbd "C-S-n") 'make-frame-command)
  (define-key ergoemacs-keymap (kbd "C-o") 'find-file)
  (define-key ergoemacs-keymap (kbd "C-S-o") 'open-in-external-app)
  (define-key ergoemacs-keymap (kbd "C-S-t") 'open-last-closed)
  (define-key ergoemacs-keymap (kbd "C-w") 'close-current-buffer)
  (define-key ergoemacs-keymap (kbd "C-s") 'save-buffer)
  (define-key ergoemacs-keymap (kbd "C-S-s") 'write-file)
  (define-key ergoemacs-keymap (kbd "C-p") 'print-buffer-confirm)
  (define-key ergoemacs-keymap (kbd "C-a") 'mark-whole-buffer)
  
  (define-key ergoemacs-keymap (kbd "C-f") 'isearch-forward)
  
  (define-key ergoemacs-keymap (kbd "<delete>") 'delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.
  
  (define-key ergoemacs-keymap (kbd "C-<prior>") 'previous-user-buffer)
  (define-key ergoemacs-keymap (kbd "C-<next>") 'next-user-buffer)
  
  (define-key ergoemacs-keymap (kbd "C-S-<prior>") 'previous-emacs-buffer)
  (define-key ergoemacs-keymap (kbd "C-S-<next>") 'next-emacs-buffer)
  
  (define-key ergoemacs-keymap (kbd "M-S-<prior>") 'backward-page)
  (define-key ergoemacs-keymap (kbd "M-S-<next>") 'forward-page)
  
  (define-key ergoemacs-keymap (kbd "C-x C-b") 'ibuffer)
  (define-key ergoemacs-keymap (kbd "C-h m") 'describe-major-mode)
  (define-key ergoemacs-keymap (kbd "<f1> m") 'describe-major-mode)
  (define-key ergoemacs-keymap (kbd "C-h o") 'where-is-old-binding)
  (define-key ergoemacs-keymap (kbd "<f1> o") 'where-is-old-binding)
  
  ;; Ctrl+Break is a common IDE shortcut to stop compilation/find/grep
  (define-key ergoemacs-keymap (kbd "C-<pause>") 'kill-compilation)
  
  (define-key ergoemacs-keymap (kbd "M-6") 'select-current-block)
  (define-key ergoemacs-keymap (kbd "M-7") 'select-current-line)
  
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
  
  (require 'lookup-word-on-internet nil "NOERROR")
  
  (define-key ergoemacs-keymap (kbd "<f1> 1") 'describe-function)
  (define-key ergoemacs-keymap (kbd "<f1> 2") 'describe-variable)
  (define-key ergoemacs-keymap (kbd "<f1> 3") 'describe-key)
  (define-key ergoemacs-keymap (kbd "<f1> 4") 'describe-char)
  (define-key ergoemacs-keymap (kbd "<f1> 5") 'woman)
  
  (define-key ergoemacs-keymap (kbd "<f1> 6") 'lookup-all-dictionaries)
  (define-key ergoemacs-keymap (kbd "<f1> 7") 'lookup-google)
  (define-key ergoemacs-keymap (kbd "<f1> 8") 'lookup-wikipedia)
  (define-key ergoemacs-keymap (kbd "<f1> 9") 'lookup-word-definition)
  (define-key ergoemacs-keymap (kbd "<f1> 0") 'lookup-answers.com)
  (define-key ergoemacs-keymap (kbd "<f1> [") 'lookup-word-dict-org)
  (define-key ergoemacs-keymap (kbd "<f1> ]") 'lookup-wiktionary)
  (define-key ergoemacs-keymap (kbd "<f1> `") 'elisp-index-search)
  
  (define-key ergoemacs-keymap (kbd "<f2>") 'cut-line-or-region) ;cut
  (define-key ergoemacs-keymap (kbd "<C-f2>") 'cut-all)
  (define-key ergoemacs-keymap (kbd "<f3>") 'copy-line-or-region) ;copy
  (define-key ergoemacs-keymap (kbd "<C-f3>") 'copy-all)
  (define-key ergoemacs-keymap (kbd "<f4>") 'yank)
  (define-key ergoemacs-keymap (kbd "<C-f4>") 'yank-pop)
  
  (define-key ergoemacs-keymap (kbd "<f5>") 'undo)
  (define-key ergoemacs-keymap (kbd "<C-f5>") 'redo)
  
  (define-key ergoemacs-keymap (kbd "<f8>") ctl-x-map)
  
  ;; Set the menu/apps key to do emacs's M-x, if on Windows
  (cond
   ((string-equal system-type "windows-nt")
    (define-key ergoemacs-keymap (kbd "<apps>") 'execute-extended-command)
    )
   ((string-equal system-type "darwin")
    t )
   ((string-equal system-type "gnu/linux")
    t ) )
  
  (define-key ergoemacs-keymap (kbd "<M-delete>") 'kill-word)
  
  ;; arrow keys to traverse brackets
  (define-key ergoemacs-keymap (kbd "<M-left>") 'backward-open-bracket) ; Alt+←
  (define-key ergoemacs-keymap (kbd "<M-right>") 'forward-close-bracket) ; Alt+→
  
  (define-key ergoemacs-keymap (kbd "<M-up>") 'backward-block) ; Alt+↑
  (define-key ergoemacs-keymap (kbd "<M-down>") 'forward-block) ; Alt+↓
  )

;; Svg from http://en.wikipedia.org/wiki/File:KB_United_Kingdom.svg
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

(defun ergoemacs-gen-svg (layout &optional file-name extra)
  "Generates a SVG picture of the layout
FILE-NAME represents the SVG template
EXTRA represents an extra file representation.

This layout file opens will in inkscape, but not all applications."
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
          (setq txt (assoc (format "M-%s" (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) ergoemacs-layout))
          (if (not txt)
              (setq txt "")
            (if (>= (length txt) 3)
                (setq txt (nth 2 txt))
              (setq txt "")))
          (when (search-forward (format ">M%s<" i) nil t)
            (replace-match  (format ">%s<" txt) t t))
          (setq i (+ i 1))))
      (message "Layout generated to %s" file))))

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




(defun ergoemacs-setup-keys ()
  "Setups keys based on a particular layout. Based on `ergoemacs-keyboard-layout'"
  (interactive)
  (let ((layout
         (intern-soft
          (concat "ergoemacs-layout-" ergoemacs-keyboard-layout))))
    (cond
     (layout
      (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout))
     (t ; US qwerty by default
      (ergoemacs-setup-keys-for-layout "us")))))

;;; --------------------------------------------------
;;; ergoemacs-keymap

(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.")

(ergoemacs-setup-keys)


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
  (if cuaModeState (progn
                     (cua-mode -1)
                     (cua-mode 1))
    (cua-mode -1)))

;;----------------------------------------------------------------------
;; ErgoEmacs hooks

(defun ergoemacs-minibuffer-setup-hook ()
  "Hook for minibuffer to move through history with previous-line and next-line keys."
  
  (defvar ergoemacs-minibuffer-keymap (copy-keymap ergoemacs-keymap))
  
  (define-key ergoemacs-minibuffer-keymap ergoemacs-keyboard-quit-key 'minibuffer-keyboard-quit)
  (define-key ergoemacs-minibuffer-keymap ergoemacs-previous-line-key 'previous-history-element)
  (define-key ergoemacs-minibuffer-keymap ergoemacs-next-line-key 'next-history-element)
  
  (define-key ergoemacs-minibuffer-keymap (kbd "<f11>") 'previous-history-element)
  (define-key ergoemacs-minibuffer-keymap (kbd "<f12>") 'next-history-element)
  (define-key ergoemacs-minibuffer-keymap (kbd "S-<f11>") 'previous-matching-history-element)
  (define-key ergoemacs-minibuffer-keymap (kbd "S-<f12>") 'next-matching-history-element)
  
  ;; The ergoemacs-mode keymap could already be in the minor-mode-overriding map
  ;; (e.g. iswitchb or ido hooks were executed)
  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-minibuffer-keymap)
               nil (lambda (x y)
                     (equal (car y) (car x))))
  )

(defun ergoemacs-isearch-hook ()
  "Hook for `isearch-mode-hook' so ergoemacs keybindings are not lost."
  
  ;; TODO restore these keys! (it is not necessary, when the
  ;; ergoemacs-isearch-hook is removed from isearch-mode-hook)
  
  (define-key isearch-mode-map (kbd "M-p") 'nil) ; was isearch-ring-retreat
  (define-key isearch-mode-map (kbd "M-n") 'nil) ; was isearch-ring-advance
  (define-key isearch-mode-map (kbd "M-y") 'nil) ; was isearch-yank-kill
  (define-key isearch-mode-map (kbd "M-c") 'nil) ; was isearch-toggle-case-fold
  (define-key isearch-mode-map (kbd "M-r") 'nil) ; was isearch-toggle-regexp
  (define-key isearch-mode-map (kbd "M-e") 'nil) ; was isearch-edit-string
  
  (define-key isearch-mode-map ergoemacs-keyboard-quit-key 'isearch-abort)
  (define-key isearch-mode-map ergoemacs-isearch-forward-key 'isearch-repeat-forward)
  (define-key isearch-mode-map ergoemacs-isearch-backward-key 'isearch-repeat-backward)
  (define-key isearch-mode-map ergoemacs-recenter-key 'recenter)
  (define-key isearch-mode-map ergoemacs-yank-key 'isearch-yank-kill)
  
  ;; CUA paste key is isearch-yank-kill in isearch mode
  (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
  
  ;; isearch-other-control-char sends the key to the original buffer and cancels isearch
  (define-key isearch-mode-map ergoemacs-kill-ring-save-key 'isearch-other-control-char)
  (define-key isearch-mode-map ergoemacs-kill-word-key 'isearch-other-control-char)
  (define-key isearch-mode-map ergoemacs-backward-kill-word-key 'isearch-other-control-char)
  
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
  )

;; Hook for interpreters
(defun ergoemacs-comint-hook ()
  "Hook for `comint-mode-hook'."
  
  (define-key comint-mode-map (kbd "<f11>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<f12>") 'comint-next-input)
  (define-key comint-mode-map (kbd "S-<f11>") 'comint-previous-matching-input)
  (define-key comint-mode-map (kbd "S-<f12>") 'comint-next-matching-input)
  )


;; Log edit mode
(defun ergoemacs-log-edit-hook ()
  "Hook for `log-edit-mode-hook'."
  (define-key log-edit-mode-map (kbd "<f11>") 'log-edit-previous-comment)
  (define-key log-edit-mode-map (kbd "<f12>") 'log-edit-next-comment)
  (define-key log-edit-mode-map (kbd "S-<f11>") 'log-edit-previous-comment)
  (define-key log-edit-mode-map (kbd "S-<f12>") 'log-edit-next-comment)
  )

(defun ergoemacs-eshell-hook ()
  "Hook for `eshell-mode-hook', to redefine some ErgoEmacs keys so they are more useful."
  
  ;; Redefining ergoemacs-move-beginning-of-line-key to eshell-bol in eshell-mode-map
  ;; does not work, we have to use minor-mode-overriding-map-alist in this case
  (defvar ergoemacs-eshell-keymap (copy-keymap ergoemacs-keymap))
  
  (define-key ergoemacs-eshell-keymap ergoemacs-move-beginning-of-line-key 'eshell-bol)
  (define-key ergoemacs-eshell-keymap (kbd "<home>") 'eshell-bol)
  (define-key ergoemacs-eshell-keymap (kbd "<f11>") 'eshell-previous-matching-input-from-input)
  (define-key ergoemacs-eshell-keymap (kbd "<f12>") 'eshell-next-matching-input-from-input)
  (define-key ergoemacs-eshell-keymap (kbd "S-<f11>") 'eshell-previous-matching-input-from-input)
  (define-key ergoemacs-eshell-keymap (kbd "S-<f12>") 'eshell-next-matching-input-from-input)
  
  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-eshell-keymap))
  )

(defun ergoemacs-iswitchb-hook ()
  "Hooks for `iswitchb-minibuffer-setup-hook'."
  
  (defvar ergoemacs-iswitchb-keymap (copy-keymap ergoemacs-keymap))
  
  (define-key ergoemacs-iswitchb-keymap ergoemacs-keyboard-quit-key 'minibuffer-keyboard-quit)
  (define-key ergoemacs-iswitchb-keymap ergoemacs-isearch-backward-key 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap ergoemacs-isearch-forward-key 'iswitchb-next-match)
  
  (define-key ergoemacs-iswitchb-keymap (kbd "<f11>") 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "<f12>") 'iswitchb-next-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "S-<f11>") 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "S-<f12>") 'iswitchb-next-match)
  
  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-iswitchb-keymap))
  )

(defun ergoemacs-ido-minibuffer-setup-hook ()
  "Hook for `ido-minibuffer-setup-hook'."
  
  (defvar ergoemacs-ido-keymap (copy-keymap ergoemacs-keymap))
  
  (define-key ergoemacs-ido-keymap ergoemacs-keyboard-quit-key 'minibuffer-keyboard-quit)
  (define-key ergoemacs-ido-keymap ergoemacs-forward-char-key 'ido-next-match)
  (define-key ergoemacs-ido-keymap ergoemacs-backward-char-key 'ido-prev-match)
  (define-key ergoemacs-ido-keymap ergoemacs-previous-line-key 'ido-next-match-dir)
  (define-key ergoemacs-ido-keymap ergoemacs-next-line-key 'ido-prev-match-dir)
  
  (define-key ergoemacs-ido-keymap (kbd "<f11>") 'previous-history-element)
  (define-key ergoemacs-ido-keymap (kbd "<f12>") 'next-history-element)
  (define-key ergoemacs-ido-keymap (kbd "S-<f11>") 'previous-matching-history-element)
  (define-key ergoemacs-ido-keymap (kbd "S-<f12>") 'next-matching-history-element)
  
  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-ido-keymap))
  )

(defun ergoemacs-auto-complete-mode-hook ()
  "Hook for `auto-complete-mode-hook'.

When the `auto-complete-mode' is on, and when a word completion
is in process, Ctrl+s does `ac-isearch'.
This fixes it."
  
  (define-key ac-completing-map ergoemacs-isearch-forward-key 'ac-isearch)
  (define-key ac-completing-map (kbd "C-s") nil))

(defvar ergoemacs-hook-list (list)
  "List of hook and hook-function pairs.")

(defun ergoemacs-add-hook (hook hook-function)
  "Adds a pair of hook and hook-function to the list
ergoemacs hooks."
  (add-to-list 'ergoemacs-hook-list (cons hook hook-function)))

(ergoemacs-add-hook 'isearch-mode-hook 'ergoemacs-isearch-hook)
(ergoemacs-add-hook 'comint-mode-hook 'ergoemacs-comint-hook)
(ergoemacs-add-hook 'log-edit-mode-hook 'ergoemacs-log-edit-hook)
(ergoemacs-add-hook 'eshell-mode-hook 'ergoemacs-eshell-hook)
(ergoemacs-add-hook 'minibuffer-setup-hook 'ergoemacs-minibuffer-setup-hook)
(ergoemacs-add-hook 'iswitchb-minibuffer-setup-hook 'ergoemacs-iswitchb-hook)
(ergoemacs-add-hook 'ido-minibuffer-setup-hook 'ergoemacs-ido-minibuffer-setup-hook)
(ergoemacs-add-hook 'auto-complete-mode-hook 'ergoemacs-auto-complete-mode-hook)

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
(defcustom ergoemacs-keyboard-layout (getenv "ERGOEMACS_KEYBOARD_LAYOUT")
  (concat "Specifies which keyboard layout to use.
This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT.

After setting this value to apply these settings you will need to type in M-x ergoemacs-setup-keys

Valid values are:

" (ergoemacs-get-layouts-doc))
  :type (ergoemacs-get-layouts-type)
  :group 'ergoemacs-keybindings)

" 
 “fr” (French)
 “sv” (Swedish)
 “da” (Danish)
 “pt-nativo” (Ergonomic PT-Nativo URL `http://xahlee.org/kbd/pt-nativo_keyboard_layout.html')"


(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
