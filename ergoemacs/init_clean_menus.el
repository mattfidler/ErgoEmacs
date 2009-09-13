;-*- coding: utf-8 -*-

;; the purpose of this file is to create a more clean menu.
;; Rationale:
;; • Emacs's Menu Usability Problem
;;   http://xahlee.org/emacs/modernization_menu.html

;; File menu
(setq menu-bar-file-menu 
      '(keymap
	(new-file menu-item "New" new-empty-buffer)
	(make-frame menu-item "New Frame" make-frame-command)
	(open-file menu-item "Open..." find-file)
	(kill-buffer menu-item "Close" close-current-buffer)
	(separator1 menu-item "--")
	(save-buffer menu-item "Save" save-buffer)
	(write-file menu-item "Save As..." write-file)
	(revert-buffer menu-item "Revert" revert-buffer)
	(separator2 menu-item "--")
	(lang-modes menu-item "Language Modes"
		    (keymap (c "C" . c-mode)
			    (c++ "C++" . c++-mode)
			    (java "Java" . java-mode)
			    (separator3 "--")
			    (css "CSS" . css-mode)
			    (html "HTML" . html-mode)
			    (nxml "XML (nxml-mode)" . nxml-mode)
			    (xml "XML (xml-mode)" . xml-mode)
			    (js "Javascript" . js2-mode)
			    (latex "LaTeX" . latex-mode)
			    (separator2 "--")
			    (elisp "Emacs Lisp" . emacs-lisp-mode)
			    (ocaml "OCaml" . tuareg-mode)
			    (haskell "Haskell" . haskell-mode)
			    (separator1 "--")
			    (perl "Perl" . cperl-mode)
			    (php "PHP" . php-mode)
			    (python "Python" . python-mode)
			    (ruby "Ruby" . ruby-mode)
			    (tcl "TCL" . tcl-mode)
			    (bash "Bash" . sh-mode)
			    "major modes"))
	(separator3 menu-item "--")
	(print-buffer menu-item "Print" print-buffer)
	(ps-print-buffer-faces menu-item "Print (font+color)" ps-print-buffer-faces)
	(separator4 menu-item "--")
	(split-window menu-item "Split Window" 
		      split-window-vertically)
	(split-window-leftright menu-item "Split Window left/right" 
				split-window-horizontally)
	(one-window menu-item "Unsplit Window" 
		    delete-other-windows)
	(separator5 menu-item "--")
	(exit-emacs menu-item "Quit" save-buffers-kill-emacs)
	"File"))

(define-key global-map [menu-bar file] (cons "File" menu-bar-file-menu))

;; Edit menu
(define-key menu-bar-edit-menu [search search-forward] '("Text..." . search-forward))
(define-key menu-bar-edit-menu [search search-backward] nil)
(define-key menu-bar-edit-menu [search re-search-forward] nil)
(define-key menu-bar-edit-menu [search re-search-backward] nil)
(define-key menu-bar-edit-menu [search repeat-search-fwd] nil)
(define-key menu-bar-edit-menu [search repeat-search-back] nil)
(define-key menu-bar-edit-menu [search separator-tag-search] nil)

;; Move 'i-search after 'search
(define-key-after menu-bar-edit-menu [i-search]
  (list 'menu-item "Incremental Search" (lookup-key menu-bar-edit-menu [search i-search]))
  'search)
(define-key menu-bar-edit-menu [search separator-tag-isearch] nil)
(define-key menu-bar-edit-menu [search i-search] nil)

(define-key menu-bar-edit-menu [goto go-to-pos] nil)
(define-key menu-bar-edit-menu [goto beg-of-buf] nil)
(define-key menu-bar-edit-menu [goto end-of-buf] nil)

(define-key-after menu-bar-edit-menu [redo] '("Redo" . redo) 'undo)
(define-key-after menu-bar-edit-menu [redo-sep] '("--") 'redo)

;; Options menu

; remove stuff
(define-key menu-bar-options-menu [cua-mode] nil)
(define-key menu-bar-options-menu [transient-mark-mode] nil)

(define-key menu-bar-options-menu [cursor-separator] nil)
(define-key menu-bar-options-menu [blink-cursor-mode] nil)
(define-key menu-bar-options-menu [debugger-separator] nil)
(define-key menu-bar-options-menu [debug-on-error] nil)
(define-key menu-bar-options-menu [debug-on-quit] nil)

(define-key menu-bar-options-menu [showhide showhide-tool-bar] nil)
(define-key menu-bar-options-menu [showhide showhide-scroll-bar] nil)
(define-key menu-bar-options-menu [showhide showhide-fringe] nil)

(define-key menu-bar-options-menu [showhide mac-font-panel-mode] nil)
(define-key menu-bar-options-menu [showhide showhide-battery] nil)
(define-key menu-bar-options-menu [showhide datetime-separator] nil)
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

;; Buffer menu
(defun ergoemacs-update-buffers (&optional force)
  (define-key global-buffers-menu-map [next-buffer] '("Next User Buffer" . next-user-buffer))
  (define-key global-buffers-menu-map [previous-buffer] '("Previous User Buffer" . previous-user-buffer))
  (define-key-after global-buffers-menu-map [next-emacs-buffer] '("Next Emacs Buffer" . next-emacs-buffer)
    'previous-buffer)
  (define-key-after global-buffers-menu-map [previous-emacs-buffer] '("Previous Emacs Buffer" . previous-emacs-buffer)
    'next-emacs-buffer)
  (define-key global-buffers-menu-map [select-named-buffer] nil)
  (define-key global-buffers-menu-map [list-all-buffers] '("List All Buffers" . ibuffer))
  )

(add-hook 'menu-bar-update-hook 'ergoemacs-update-buffers t)

;; Tools menu

(define-key menu-bar-tools-menu [shell] 
  '("Shell" keymap
    (shell menu-item "Run Command..." shell-command 
	   :help "Invoke a shell command and catch its output")
    (shell-on-region menu-item "Run Command on Region..." shell-command-on-region 
		     :enable mark-active 
		     :help "Pass marked region to a shell command")
    (shell-sep1 menu-item "--")
    (cmd menu-item "Cmd" cmd-shell :help "Start Windows Shell (cmd.exe / command.com)")
    (eshell menu-item "Eshell" eshell :help "Start Emacs Shell")
    (msys menu-item "MSYS Shell" msys-shell :help "Start MSYS shell (sh.exe)")
    (shell-sep2 menu-item "--")
    (powershell menu-item "Powershell" powershell :help "Start PowerShell")
    "shells"))

(define-key menu-bar-tools-menu [shell-on-region] nil)

(define-key menu-bar-tools-menu [gnus] nil)
(define-key menu-bar-tools-menu [rmail] nil)
(define-key menu-bar-tools-menu [compose-mail] nil)
(define-key menu-bar-tools-menu [separator-games] nil)
(define-key menu-bar-tools-menu [games] nil)

;; obsolete, outdated contents. Much replaced by web.
(define-key menu-bar-help-menu [getting-new-versions] nil)
(define-key menu-bar-help-menu [describe-distribution] nil)

(define-key menu-bar-help-menu [external-packages] nil)
(define-key menu-bar-help-menu [more] nil)

(define-key menu-bar-help-menu [emacs-known-problems] nil)
(define-key menu-bar-help-menu [emacs-problems] nil)
(define-key menu-bar-help-menu [find-emacs-packages] nil)

;; outdated humor and tech
(define-key menu-bar-help-menu [eliza] nil)
(define-key menu-bar-help-menu [emacs-psychotherapist] nil)

;; antiquated tutorial. If it needs a tutorial, something is wrong with UI.
(define-key menu-bar-help-menu [emacs-tutorial] nil)
(define-key menu-bar-help-menu [emacs-tutorial-language-specific] nil)
(define-key menu-bar-help-menu [emacs-faq] nil)
(define-key menu-bar-help-menu [search-documentation emacs-terminology] nil)

;; remove FSF propaganda. (already linked in About Emacs)
(define-key menu-bar-help-menu [about-gnu-project] nil)
(define-key menu-bar-help-menu [describe-copying] nil)
(define-key menu-bar-help-menu [describe-no-warranty] nil)
(define-key menu-bar-help-menu [more-manuals order-emacs-manuals] nil)
(define-key menu-bar-help-menu [manuals order-emacs-manuals] nil)
(define-key menu-bar-help-menu [sep2] nil)
(define-key menu-bar-help-menu [about-gnu-project] nil)

;; TODO: 

;; • possibly add menu that shows the current minor modes in effect. (To implement, probably just call describe-mode. ) We might need this since we made C-h m to be describe-major-mode instead of describe-mode. But again maybe not since minor-modes is rather very technical specific to emacs, a concept and feature not in most editors.

;; • re-create menus from scratch, instead of piggy back to remove add. (done for the File menu) Because piggy back is difficult to do and manage and subject to emacs changes. 

;; • remove redundant dividers
;; • reorg the help menu and submenu.

;; • when recentf-mode minor mode is on, it adds a Open Recent menu item under File at bottom after Quit. Needs to be after Open. Looking at the code, it uses easymenu.el to do things. Need fix.

;; • the code can be improved. Right now it uses define-key repeatedly. It can be just a key map.