;-*- coding: utf-8 -*-

;; the purpose of this file is to create a more clean menu.
;; Rationale:
;; • Emacs's Menu Usability Problem
;;   http://xahlee.org/emacs/modernization_menu.html

;;;;§----------------------------------------
;; File menu
(setq menu-bar-file-menu
      '(keymap
	(new-file menu-item "New" new-empty-buffer)
	(make-frame menu-item "New Frame" make-frame-command)
	(open-file menu-item "Open..." find-file)
	(open-last-closed menu-item "Open last closed" open-last-closed)
	(kill-buffer menu-item "Close" close-current-buffer)
	(separator1 menu-item "--")
	(save-buffer menu-item "Save" save-buffer)
	(write-file menu-item "Save As..." write-file)
	(revert-buffer menu-item "Revert to Saved" revert-buffer)
	(separator2 menu-item "--")
	(lang-modes menu-item "Language Modes"
		    (keymap (c "C" . c-mode)
			    (c++ "C++" . c++-mode)
			    (csharp "C#" . csharp-mode)
			    (java "Java" . java-mode)
			    (separator3 "--")
			    (css "CSS" . css-mode)
			    (html "HTML" . html-mode)
			    (nxml "XML (nxml-mode)" . nxml-mode)
			    (xml "XML (xml-mode)" . xml-mode)
			    (js "Javascript (js-mode)" . js-mode)
			    (js2 "Javascript (js2-mode)" . js2-mode)
			    (latex "LaTeX" . latex-mode)
			    (separator2 "--")
			    (elisp "Emacs Lisp" . emacs-lisp-mode)
			    (clojure "Clojure" . clojure-mode)
			    (ocaml "OCaml" . tuareg-mode)
			    (haskell "Haskell" . haskell-mode)
			    (scala-mode "Scala" . scala-mode)
			    (haskell "Erlang" . erlang-mode)
			    (separator1 "--")
			    (perl "Perl" . cperl-mode)
			    (php "PHP" . php-mode)
			    (python "Python" . python-mode)
			    (ruby "Ruby" . ruby-mode)
			    (tcl "TCL" . tcl-mode)
			    (bash "Bash" . sh-mode)
			    (vb "Visual Basic" . visual-basic-mode)
			    (cmd "cmd.exe" . dos-mode)
			    (powershell "PowerShell" . powershell-mode)
			    (list-text-editing-modes "List Text Editing Modes..." . list-text-editing-modes)
			    "major modes"))
	(separator3 menu-item "--")
	(print-buffer menu-item "Print" print-buffer)
	(ps-print-buffer-faces menu-item "Print (font+color)" ps-print-buffer-faces)
	(separator4 menu-item "--")
	(split-window menu-item "Split Window"
		      split-window-vertically)
	(split-window-leftright menu-item "Split Window left/right"
				split-window-horizontally
				:keys "Alt+Shift+2")
	(one-window menu-item "Unsplit Window"
		    delete-other-windows)
	(separator5 menu-item "--")
	(exit-emacs menu-item "Quit" save-buffers-kill-emacs)
	"File"))

(define-key global-map [menu-bar file] (cons "File" menu-bar-file-menu))

;;;;§----------------------------------------
;; Edit menu
(setq menu-bar-edit-menu
      '(keymap
	(undo menu-item "Undo" undo
	      :enable (and
		       (not buffer-read-only)
		       (not
			(eq t buffer-undo-list))
		       (if
			   (eq last-command 'undo)
			   (listp pending-undo-list)
			 (consp buffer-undo-list)))
	      :help "Undo last operation"
	      :keys "Ctrl+Z")
	(redo menu-item "Redo" redo
	      :keys "Ctrl+Shift+Z")
	(redo-sep menu-item "--")
	(cut menu-item "Cut" clipboard-kill-region
	     :help "Delete text in region and copy it to the clipboard"
	     :keys "Ctrl+X")
	(copy menu-item "Copy" clipboard-kill-ring-save
	      :help "Copy text in region to the clipboard"
	      :keys "Ctrl+C")
	(paste menu-item "Paste" clipboard-yank
	       :help "Paste text from clipboard"
	       :keys "Ctrl+V")
	(paste-from-menu menu-item "Paste from Kill Menu" yank-menu
			 :enable (and
				  (cdr yank-menu)
				  (not buffer-read-only))
			 :help "Choose a string from the kill ring and paste it")
	(clear menu-item "Clear" delete-region 
	       :enable (and mark-active (not buffer-read-only))
	       :help "Delete the text in region between mark and current position"
	       :keys "Del")
	(mark-whole-buffer menu-item "Select All" mark-whole-buffer
			   :help "Mark the whole buffer for a subsequent cut/copy")
	(separator-search menu-item "--")
	(search menu-item "Search"
		(keymap
		 (search-forward menu-item "Text…" search-forward)
		 (separator-repeat-search menu-item "--")
		 (tags-srch menu-item "Search Tagged Files..." tags-search
			    :help "Search for a regexp in all tagged files")
		 (tags-continue menu-item "Continue Tags Search" tags-loop-continue
				:help "Continue last tags search operation")
		 "Search"))
	(i-search menu-item "Incremental Search"
		  (keymap
		   (isearch-forward menu-item "Forward String..." isearch-forward
				    :help "Search forward for a string as you type it")
		   (isearch-backward menu-item "Backward String..." isearch-backward
				     :help "Search backwards for a string as you type it")
		   (isearch-forward-regexp menu-item "Forward Regexp..." isearch-forward-regexp
					   :help "Search forward for a regular expression as you type it")
		   (isearch-backward-regexp menu-item "Backward Regexp..." isearch-backward-regexp
					    :help "Search backwards for a regular expression as you type it")
		   "Incremental Search"))
	(replace menu-item "Replace"
		 (keymap
		  (query-replace menu-item "Replace String..." query-replace 
				 :enable (not buffer-read-only)
				 :help "Replace string interactively, ask about each occurrence")
		  (query-replace-regexp menu-item "Replace Regexp..." query-replace-regexp 
					:enable (not buffer-read-only)
					:help "Replace regular expression interactively, ask about each occurrence")
		  (separator-replace-tags menu-item "--")
		  (tags-repl menu-item "Replace in Tagged Files..." tags-query-replace
			     :help "Interactively replace a regexp in all tagged files")
		  (tags-repl-continue menu-item "Continue Replace" tags-loop-continue
				      :help "Continue last tags replace operation")
		  "Replace"))
	(goto menu-item "Go To"
	      (keymap
	       (go-to-line menu-item "Goto Line..." goto-line
			   :help "Read a line number and go to that line")
	       (separator-tags menu-item "--")
	       (find-tag menu-item "Find Tag..." find-tag
			 :help "Find definition of function or variable")
	       (find-tag-otherw menu-item "Find Tag in Other Window..." find-tag-other-window
				:help "Find function/variable definition in another window")
	       (next-tag menu-item "Find Next Tag" menu-bar-next-tag
			 :enable (and
				  (boundp 'tags-location-ring)
				  (not
				   (ring-empty-p tags-location-ring)))
			 :help "Find next function/variable matching last tag name")
	       (next-tag-otherw menu-item "Next Tag in Other Window" menu-bar-next-tag-other-window 
				:enable (and
					 (boundp 'tags-location-ring)
					 (not
					  (ring-empty-p tags-location-ring)))
				:help "Find next function/variable matching last tag name in another window")
	       (apropos-tags menu-item "Tags Apropos..." tags-apropos
			     :help "Find function/variables whose names match regexp")
	       (separator-tag-file menu-item "--")
	       (set-tags-name menu-item "Set Tags File Name..." visit-tags-table
			      :help "Tell Tags commands which tag table file to use")
	       "Go To"))
	(bookmark menu-item "Bookmarks" menu-bar-bookmark-map)
	(separator-bookmark menu-item "--")
	(fill menu-item "Fill" fill-region
	      :enable (and mark-active
			   (not buffer-read-only))
	      :help "Fill text in region to fit between left and right margin")
	(props menu-item "Text Properties" facemenu-menu)
	"Edit"))

(define-key global-map [menu-bar edit] (cons "Edit" menu-bar-edit-menu))

;;;;§----------------------------------------
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

;; (define-key menu-bar-options-menu [customize] nil)
;; (define-key menu-bar-options-menu [save] nil)
(define-key global-map (kbd "<S-down-mouse-1>") nil) ; remove Shift+“Mouse Left Button” for setting font

;; add a menu for showing line numbers on margin
(define-key-after menu-bar-options-menu [showhide global-linum-mode]
  '(menu-item "Show/Hide line numbers in margin" global-linum-mode
    :button (:toggle . global-linum-mode))  'line-number-mode )

;; add a menu for toggling the visibility of spaces and tabs
(define-key-after menu-bar-options-menu [whitespace-mode]
  '(menu-item "Show/Hide Space, Tab" whitespace-mode
    :button (:toggle . whitespace-mode)) 'line-move-visual )

;; add a menu to toggle whether down arrow key move cursor by visual line.
(define-key-after menu-bar-options-menu [line-move-visual]
  '(menu-item "Move through wrapped lines" toggle-line-move-visual
    :button (:toggle . line-move-visual)) 'line-wrapping)

;; add a menu to toggle whether left/right cursor movement will move into camelCaseWords 
(define-key-after menu-bar-options-menu [global-subword-mode]
  '(menu-item "Move through camelCaseWord" global-subword-mode
    :button (:toggle . global-subword-mode)) 'line-move-visual)

;; add font scale change
(define-key-after menu-bar-options-menu [menu-font-size]
  '(menu-item "Font Size"
	      (keymap 
	       (zoom-in menu-item "Zoom In" text-scale-increase)
	       (zoom-out menu-item "Zoom Out" text-scale-decrease)
	       (zoom-reset menu-item "Zoom Reset" text-scale-normal-size)))
  'menu-set-font)

;;;;§----------------------------------------
;; Buffer menu
(defun ergoemacs-update-buffers (&optional force)
  (define-key global-buffers-menu-map [next-buffer]
    '(menu-item "Next User Buffer" next-user-buffer		:keys "Ctrl+PgDn"))
  (define-key global-buffers-menu-map [previous-buffer]
    '(menu-item "Previous User Buffer" previous-user-buffer	:keys "Ctrl+PgUp"))
  (define-key-after global-buffers-menu-map [next-emacs-buffer]
    '(menu-item "Next Emacs Buffer" next-emacs-buffer		:keys "Ctrl+Shift+PgDn")
    'previous-buffer)
  (define-key-after global-buffers-menu-map [previous-emacs-buffer]
    '(menu-item "Previous Emacs Buffer" previous-emacs-buffer	:keys "Ctrl+Shift+PgUp")
    'next-emacs-buffer)
  (define-key global-buffers-menu-map [select-named-buffer] nil)
  (define-key global-buffers-menu-map [list-all-buffers]
    '(menu-item "List All Buffers" ibuffer			:keys "Ctrl+X Ctrl+B"))
  )

(add-hook 'menu-bar-update-hook 'ergoemacs-update-buffers t)

;;;;§----------------------------------------
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


;;;;§----------------------------------------
;; Process menu keyboard bindings

(defun ergoemacs-shortcut-for-command (cmd)
  (let ((key (key-description (where-is-internal cmd nil t nil t))))
    ;(message "KEY \"%s\"" key)
    (let ((case-fold-search nil))
      (replace-regexp-in-string " " "  "
      (replace-regexp-in-string "<" ""
      (replace-regexp-in-string ">" ""
      (replace-regexp-in-string "\\bRET\\b" "ENTER"
      (replace-regexp-in-string "\\bprior\\b" "PgUp"
      (replace-regexp-in-string "\\bnext\\b" "PgDn"
      (replace-regexp-in-string "<f\\([0-9]+\\)>" "F\\1"
      (replace-regexp-in-string "\\b-\\b" "+"
      (replace-regexp-in-string "\\b[[:lower:]]\\b" 'upcase
      (replace-regexp-in-string "\\b\\([[:upper:]]\\)\\b" "Shift+\\1"
      (replace-regexp-in-string "\\bC-" "Ctrl+"
      (replace-regexp-in-string "\\bS-" "Shift+"
      (replace-regexp-in-string "\\bM-" "Alt+"
				key t) t) t) t) t) t) t) t) t) t) t) t) t)
      )
    )
  )

(defun ergoemacs-shortcut-for-menu-item (item)
  (if (and (>= (safe-length item) 4)
	   (symbolp (car item))
	   (eq (cadr item) 'menu-item)
	   (stringp (caddr item))
	   (symbolp (cadddr item))
	   (not (keymapp (cadddr item))))
      ;; Look if this item already has a :keys property
      (if (position :keys item)
	  nil
	(ergoemacs-shortcut-for-command (cadddr item))
	)
    nil
    )
  )

(defun ergoemacs-preprocess-menu-keybindings (menu)
  (unless (keymapp menu)
    (error "Invalid menu in ergoemacs-preprocess-menu-keybindings %s" menu))

  (when (symbolp menu)
    (setq menu (symbol-value menu)))

  ;; For each element in the menu
  (setcdr menu
	  (mapcar (lambda (item)
		    (let ((key (ergoemacs-shortcut-for-menu-item item)))
		      (if key
			  (append item (cons :keys (cons key nil)))
			item))
		    )
		  (cdr menu)
		  )
	  )

  ;; Recurse sub menu items
  (mapc (lambda (x)
	  (when (and (consp x)
		     (consp (cdr x))
		     (consp (cdr (cdr x)))
		     (consp (cdr (cdr (cdr x))))
		     (eq (car (cdr x)) 'menu-item)
		     (keymapp (car (cdr (cdr (cdr x))))))
	    ;(message "Submenu: %s" (car (cdr (cdr x))))
	    (ergoemacs-preprocess-menu-keybindings (car (cdr (cdr (cdr x)))))
	    )
	  )
	(cdr menu)
	)
  )

(ergoemacs-preprocess-menu-keybindings menu-bar-file-menu)
(ergoemacs-preprocess-menu-keybindings menu-bar-edit-menu)
(ergoemacs-preprocess-menu-keybindings menu-bar-bookmark-map)
(ergoemacs-preprocess-menu-keybindings menu-bar-options-menu)
(ergoemacs-preprocess-menu-keybindings menu-bar-tools-menu)
(ergoemacs-preprocess-menu-keybindings (lookup-key menu-bar-tools-menu [shell]))
(ergoemacs-preprocess-menu-keybindings menu-bar-help-menu)
(ergoemacs-preprocess-menu-keybindings edit-popup-menu)

(ergoemacs-preprocess-menu-keybindings
 (lookup-key lisp-interaction-mode-map [menu-bar lisp-interaction]))

;;;;§----------------------------------------
;; TODO:

;; • call ergoemacs-preprocess-menu-keybindings for global-map (all menus of all modes)

;; • possibly add menu that shows the current minor modes in effect. (To implement, probably just call describe-mode. ) We might need this since we made C-h m to be describe-major-mode instead of describe-mode. But again maybe not since minor-modes is rather very technical specific to emacs, a concept and feature not in most editors.

;; • re-create menus from scratch, instead of piggy back to remove add. (done for the File menu) Because piggy back is difficult to do and manage and subject to emacs changes.

;; • reorg the help menu and submenu.
