;-*- coding: utf-8 -*-

;; 2009-02-02, 2009-08-21

;; Clean out emacs menus. Add additional ones.
;; Rationale:
;; • Emacs's Menu Usability Problem
;;   http://xahlee.org/emacs/modernization_menu.html

;; and also some other criticisms that effects the menu. e.g.

;; • Replace list-buffers with ibuffer
;; • Ctrl+N for new-empty-buffer.
;; • Support standard shortcuts for open, close, save, save as.
;; • next user buffer and previous user buffer.
;; etc.
;; Most of are discussed here:
;; 
;; • Why Emacs's Keyboard Shortcuts Are Painful
;;   http://xahlee.org/emacs/emacs_kb_shortcuts_pain.html
;; 
;; • Suggestions on Emacs's Scratch Buffer
;;   http://xahlee.org/emacs/modernization_scratch_buffer.html
;; 
;; • Emacs's M-‹key› Notation vs Alt+‹key› Notation
;;   http://xahlee.org/emacs/modernization_meta_key.html


;; file menu

(define-key global-map [menu-bar file open-file] '("Open file or folder" . find-file))
(define-key global-map [menu-bar file kill-buffer] '("Close" . close-current-buffer))
(define-key global-map [menu-bar file new-file] '("New" . new-empty-buffer))

(define-key global-map [menu-bar file dired] nil)
(define-key global-map [menu-bar file insert-file] nil)
(define-key global-map [menu-bar file make-frame-on-display] nil)

;; edit menu
(define-key global-map [menu-bar edit search search-forward] nil)
(define-key global-map [menu-bar edit search search-backward] nil)
(define-key global-map [menu-bar edit search re-search-forward] nil)
(define-key global-map [menu-bar edit search re-search-backward] nil)
(define-key global-map [menu-bar edit search repeat-search-fwd] nil)
(define-key global-map [menu-bar edit search repeat-search-back] nil)

(define-key global-map [menu-bar edit goto go-to-pos] nil)
(define-key global-map [menu-bar edit goto beg-of-buf] nil)
(define-key global-map [menu-bar edit goto end-of-buf] nil)

;; options menu

; remove stuff
(define-key global-map [menu-bar options cua-mode] nil)
(define-key global-map [menu-bar options transient-mark-mode] nil)

(define-key global-map [menu-bar options blink-cursor-mode] nil)
(define-key global-map [menu-bar options debug-on-error] nil)
(define-key global-map [menu-bar options debug-on-quit] nil)

(define-key global-map [menu-bar options showhide showhide-tool-bar] nil)
(define-key global-map [menu-bar options showhide showhide-scroll-bar] nil)
(define-key global-map [menu-bar options showhide showhide-fringe] nil)

(define-key global-map [menu-bar options showhide mac-font-panel-mode] nil)
(define-key global-map [menu-bar options showhide showhide-battery] nil)
(define-key global-map [menu-bar options showhide showhide-date-time] nil)
(define-key global-map [menu-bar options showhide size-indication-mode] nil)

(define-key global-map (kbd "<S-down-mouse-1>") nil)

;; add a command to toggle by cursor move by visual line.
;; todo: need to make the menu reflect current state
(define-key global-map [menu-bar options line-move-visual] '("Toggle ↑↓ Move by Visual Line" . toggle-line-move-visual))

;; add font scale change
(define-key global-map [menu-bar options zoom-in] '("Zoom In" . 'text-scale-increase))
(define-key global-map [menu-bar options zoom-out] '("Zoom Out" . 'text-scale-decrease))
(define-key global-map [menu-bar options zoom-reset] '("Zoom Reset" . 'text-scale-normal-size))

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


;; Create language modes menu
(define-key-after global-map [menu-bar file lang-modes] (cons "Language Modes" (make-sparse-keymap "major modes")) 'kill-buffer )

(define-key global-map [menu-bar file lang-modes bash] '("Bash" . sh-mode))
(define-key global-map [menu-bar file lang-modes tcl] '("TCL" . tcl-mode))
(define-key global-map [menu-bar file lang-modes ruby] '("Ruby" . ruby-mode))
(define-key global-map [menu-bar file lang-modes python] '("Python" . python-mode))
(define-key global-map [menu-bar file lang-modes php] '("PHP" . php-mode))
(define-key global-map [menu-bar file lang-modes perl] '("Perl" . cperl-mode))
(define-key global-map [menu-bar file lang-modes separator1] '("--"))
(define-key global-map [menu-bar file lang-modes haskell] '("Haskell" . haskell-mode))
(define-key global-map [menu-bar file lang-modes ocaml] '("OCaml" . tuareg-mode))
(define-key global-map [menu-bar file lang-modes elisp] '("Emacs Lisp" . emacs-lisp-mode))
(define-key global-map [menu-bar file lang-modes separator2] '("--"))
(define-key global-map [menu-bar file lang-modes latex] '("LaTeX" . latex-mode))
(define-key global-map [menu-bar file lang-modes js] '("Javascript" . js2-mode))
(define-key global-map [menu-bar file lang-modes xml] '("XML (xml-mode)" . xml-mode))
(define-key global-map [menu-bar file lang-modes nxml] '("XML (nxml-mode)" . nxml-mode))
(define-key global-map [menu-bar file lang-modes html] '("HTML" . html-mode))
(define-key global-map [menu-bar file lang-modes css] '("CSS" . css-mode))
(define-key global-map [menu-bar file lang-modes separator3] '("--"))
(define-key global-map [menu-bar file lang-modes java] '("Java" . java-mode))
(define-key global-map [menu-bar file lang-modes c++] '("C++" . c++-mode))
(define-key global-map [menu-bar file lang-modes c] '("C" . c-mode))


;; TO DO: 
;; • remove dividers
;; • move incremental search menus one level up.
;; • reorg the help menu and submenu.