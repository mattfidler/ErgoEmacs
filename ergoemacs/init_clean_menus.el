;-*- coding: utf-8 -*-

;; the purpose of this file is to create a more clean menu.
;; Rationale:
;; • Emacs's Menu Usability Problem
;;   http://xahlee.org/emacs/modernization_menu.html

;; Create new File menu
(setq menu-bar-file-menu (make-sparse-keymap "File"))
(define-key global-map [menu-bar file] (cons "File" menu-bar-file-menu))

(define-key menu-bar-file-menu [exit-emacs] '("Quit" . save-buffers-kill-emacs))
(define-key menu-bar-file-menu [separator5] '("--"))
(define-key menu-bar-file-menu [one-window] '("Unsplit Window" . delete-other-windows))
(define-key menu-bar-file-menu [split-window-leftright] '("Split Window left/right" . split-window-horizontally))
(define-key menu-bar-file-menu [split-window] '("Split Window" . split-window-vertically))
(define-key menu-bar-file-menu [separator4] '("--"))
(define-key menu-bar-file-menu [ps-print-buffer-faces] '("Print (font+color)" . ps-print-buffer-faces))
(define-key menu-bar-file-menu [print-buffer] '("Print" . print-buffer))
(define-key menu-bar-file-menu [separator3] '("--"))

;; add a language modes menu
(define-key menu-bar-file-menu [lang-modes] (cons "Language Modes" (make-sparse-keymap "major modes")))

(define-key menu-bar-file-menu [lang-modes bash] '("Bash" . sh-mode))
(define-key menu-bar-file-menu [lang-modes tcl] '("TCL" . tcl-mode))
(define-key menu-bar-file-menu [lang-modes ruby] '("Ruby" . ruby-mode))
(define-key menu-bar-file-menu [lang-modes python] '("Python" . python-mode))
(define-key menu-bar-file-menu [lang-modes php] '("PHP" . php-mode))
(define-key menu-bar-file-menu [lang-modes perl] '("Perl" . cperl-mode))
(define-key menu-bar-file-menu [lang-modes separator1] '("--"))
(define-key menu-bar-file-menu [lang-modes haskell] '("Haskell" . haskell-mode))
(define-key menu-bar-file-menu [lang-modes ocaml] '("OCaml" . tuareg-mode))
(define-key menu-bar-file-menu [lang-modes elisp] '("Emacs Lisp" . emacs-lisp-mode))
(define-key menu-bar-file-menu [lang-modes separator2] '("--"))
(define-key menu-bar-file-menu [lang-modes latex] '("LaTeX" . latex-mode))
(define-key menu-bar-file-menu [lang-modes js] '("Javascript" . js2-mode))
(define-key menu-bar-file-menu [lang-modes xml] '("XML (xml-mode)" . xml-mode))
(define-key menu-bar-file-menu [lang-modes nxml] '("XML (nxml-mode)" . nxml-mode))
(define-key menu-bar-file-menu [lang-modes html] '("HTML" . html-mode))
(define-key menu-bar-file-menu [lang-modes css] '("CSS" . css-mode))
(define-key menu-bar-file-menu [lang-modes separator3] '("--"))
(define-key menu-bar-file-menu [lang-modes java] '("Java" . java-mode))
(define-key menu-bar-file-menu [lang-modes c++] '("C++" . c++-mode))
(define-key menu-bar-file-menu [lang-modes c] '("C" . c-mode))

(define-key menu-bar-file-menu [separator2] '("--"))
(define-key menu-bar-file-menu [revert-buffer] '("Revert" . revert-buffer))
(define-key menu-bar-file-menu [write-file] '("Save As…" . write-file))
(define-key menu-bar-file-menu [save-buffer] '("Save" . save-buffer))
(define-key menu-bar-file-menu [separator1] '("--"))
(define-key menu-bar-file-menu [kill-buffer] '("Close" . close-current-buffer))
(define-key menu-bar-file-menu [open-file] '("Open…" . find-file))
(define-key menu-bar-file-menu [make-frame] '("New Frame" . make-frame-command))
(define-key menu-bar-file-menu [new-file] '("New" . new-empty-buffer))

;; edit menu
(define-key menu-bar-edit-menu [search search-forward] nil)
(define-key menu-bar-edit-menu [search search-backward] nil)
(define-key menu-bar-edit-menu [search re-search-forward] nil)
(define-key menu-bar-edit-menu [search re-search-backward] nil)
(define-key menu-bar-edit-menu [search repeat-search-fwd] nil)
(define-key menu-bar-edit-menu [search repeat-search-back] nil)

(define-key menu-bar-edit-menu [goto go-to-pos] nil)
(define-key menu-bar-edit-menu [goto beg-of-buf] nil)
(define-key menu-bar-edit-menu [goto end-of-buf] nil)

(define-key-after menu-bar-edit-menu [redo] '("Redo" . redo) 'undo)
(define-key-after menu-bar-edit-menu [redo-sep] '("--") 'redo)

;; options menu

; remove stuff
(define-key menu-bar-options-menu [cua-mode] nil)
(define-key menu-bar-options-menu [transient-mark-mode] nil)

(define-key menu-bar-options-menu [blink-cursor-mode] nil)
(define-key menu-bar-options-menu [debug-on-error] nil)
(define-key menu-bar-options-menu [debug-on-quit] nil)

(define-key menu-bar-options-menu [showhide showhide-tool-bar] nil)
(define-key menu-bar-options-menu [showhide showhide-scroll-bar] nil)
(define-key menu-bar-options-menu [showhide showhide-fringe] nil)

(define-key menu-bar-options-menu [showhide mac-font-panel-mode] nil)
(define-key menu-bar-options-menu [showhide showhide-battery] nil)
(define-key menu-bar-options-menu [showhide showhide-date-time] nil)
(define-key menu-bar-options-menu [showhide size-indication-mode] nil)

(define-key global-map (kbd "<S-down-mouse-1>") nil)

;; add a command to toggle by cursor move by visual line.
;; todo: need to make the menu reflect current state
(define-key-after menu-bar-options-menu [line-move-visual]
  '(menu-item "Move Through Wrapped Lines" toggle-line-move-visual 
    :button (:toggle . line-move-visual)) 'line-wrapping)

;; add font scale change
(define-key-after menu-bar-options-menu [menu-font-size] 
  (cons "Font Size" (make-sparse-keymap "font sizes")) 'menu-set-font)

(define-key menu-bar-options-menu [menu-font-size zoom-in] '("Zoom In" . text-scale-increase))
(define-key menu-bar-options-menu [menu-font-size zoom-out] '("Zoom Out" . text-scale-decrease))
(define-key menu-bar-options-menu [menu-font-size zoom-reset] '("Zoom Reset" . text-scale-normal-size))

;; buffers menu
(define-key global-map [menu-bar buffer next-buffer] '("Next User Buffer" . next-user-buffer))
(define-key global-map [menu-bar buffer previous-buffer] '("Previous User Buffer" . previous-user-buffer))
(define-key global-map [menu-bar buffer next-emacs-buffer] '("Next Emacs Buffer" . next-emacs-buffer))
(define-key global-map [menu-bar buffer previous-emacs-buffer] '("Previous Emacs Buffer" . previous-emacs-buffer))

(define-key global-map [menu-bar buffer list-all-buffers] '("List All Buffers" . ibuffer))

;; tools menu
(define-key global-map [menu-bar tools gnus] nil)
(define-key global-map [menu-bar tools rmail] nil)
(define-key global-map [menu-bar tools compose-mail] nil)
(define-key global-map [menu-bar tools games] nil)

;; obsolete, outdated contents. Much replaced by web.
(define-key global-map [menu-bar help-menu getting-new-versions] nil)
(define-key global-map [menu-bar help-menu describe-distribution] nil)

(define-key global-map [menu-bar help-menu external-packages] nil) (define-key global-map [menu-bar help-menu more] nil)

(define-key global-map [menu-bar help-menu emacs-known-problems] nil)
(define-key global-map [menu-bar help-menu emacs-problems] nil)
(define-key global-map [menu-bar help-menu find-emacs-packages] nil)

;; outdated humor and tech
(define-key global-map [menu-bar help-menu eliza] nil) (define-key global-map [menu-bar help-menu emacs-psychotherapist] nil)

;; antiquated tutorial. If it needs a tutorial, something is wrong with UI.
(define-key global-map [menu-bar help-menu emacs-tutorial] nil)
(define-key global-map [menu-bar help-menu emacs-tutorial-language-specific] nil)
(define-key global-map [menu-bar help-menu emacs-faq] nil)
(define-key global-map [menu-bar help-menu search-documentation emacs-terminology] nil)

;; remove FSF propaganda. (already linked in About Emacs)
(define-key global-map [menu-bar help-menu about-gnu-project] nil)
(define-key global-map [menu-bar help-menu describe-copying] nil)
(define-key global-map [menu-bar help-menu describe-no-warranty] nil)
(define-key global-map [menu-bar help-menu more-manuals order-emacs-manuals] nil)
(define-key global-map [menu-bar help-menu manuals order-emacs-manuals] nil)
(define-key global-map [menu-bar help-menu about-gnu-project] nil)

;; TODO: 

;; • possibly add menu that shows the current minor modes in effect. (To implement, probably just call describe-mode. ) We might need this since we made C-h m to be describe-major-mode instead of describe-mode. But again maybe not since minor-modes is rather very technical specific to emacs, a concept and feature not in most editors.

;; • re-create menus from scratch, instead of piggy back to remove add. (done for the File menu) Because piggy back is difficult to do and manage and subject to emacs changes. 

;; • move incremental search menus one level up.
;; • remove redundant dividers
;; • reorg the help menu and submenu.

;; • when recentf-mode minor mode is on, it adds a Open Recent menu item under File at bottom after Quit. Needs to be after Open. Looking at the code, it uses easymenu.el to do things. Need fix.

;; • the code can be improved. Right now it uses define-key repeatedly. It can be just a key map.