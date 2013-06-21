;;; ergoemacs-menus.el --- Turn on and off ergoemacs-style menus
;; 
;; Filename: ergoemacs-menus.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Jun 21 01:05:18 2013 (-0500)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defvar ergoemacs-menu-bar-old-file-menu (lookup-key global-map [menu-bar file]))
(defvar ergoemacs-menu-bar-file-menu nil)

(defun ergoemacs-menu-bar-file-menu ()
  "Creates Ergoemacs File Menu"
  (setq ergoemacs-menu-bar-file-menu
        '(keymap
          (new-file menu-item "New" ergoemacs-new-empty-buffer)
          (make-frame menu-item "New Frame" make-frame-command)
          (open-file menu-item "Open..." find-file)
          (open-last-closed menu-item "Open last closed" ergoemacs-open-last-closed)
          (kill-buffer menu-item "Close" ergoemacs-close-current-buffer)
          (separator1 menu-item "--")
          (save-buffer menu-item "Save" save-buffer)
          (write-file menu-item "Save As..." write-file)
          (revert-buffer menu-item "Revert to Saved" revert-buffer)
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
                              (js "Javascript (js-mode)" . js-mode)
                              (latex "LaTeX" . latex-mode)
                              (separator2 "--")
                              (elisp "Emacs Lisp" . emacs-lisp-mode)
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
                                  split-window-horizontally)
          (one-window menu-item "Unsplit Window"
                      delete-other-windows)
          (separator5 menu-item "--")
          (execute-command menu-item "Execute Command" execute-extended-command)
          (repeat-earlier-command "Repeat Earlier Command" repeat-complex-command)
          (separator6 menu-item "--")
          (exit-emacs menu-item "Quit" save-buffers-kill-emacs)
          "File"))
  (ergoemacs-preprocess-menu-keybindings ergoemacs-menu-bar-file-menu))

(defvar ergoemacs-menu-bar-old-edit-menu (lookup-key global-map [menu-bar edit]))

(setq ergoemacs-menu-bar-edit-menu
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
              :keys "Ctrl+Y")
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
                           :help "Mark the whole buffer for a subsequent cut/copy"
                           :keys "Ctrl+A")
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

(ergoemacs-preprocess-menu-keybindings ergoemacs-menu-bar-edit-menu)

(defun ergoemacs-kbd-to-key (key)
  "Converts key Emacs key code to ergoemacs-key-code."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     " " "  "
     (replace-regexp-in-string
      "<" ""
      (replace-regexp-in-string
       ">" ""
       (replace-regexp-in-string
        "\\bRET\\b" "ENTER"
        (replace-regexp-in-string
         "\\bprior\\b" "PgUp"
         (replace-regexp-in-string
          "\\bnext\\b" "PgDn"
          (replace-regexp-in-string
           "<f\\([0-9]+\\)>" "F\\1"
           (replace-regexp-in-string
            "\\b-\\b" "+"
            (replace-regexp-in-string
             "\\b[[:lower:]]\\b" 'upcase
             (replace-regexp-in-string
              "\\b\\([[:upper:]]\\)\\b" "Shift+\\1"
              (replace-regexp-in-string
               "\\bC-" "Ctrl+"
               (replace-regexp-in-string
                "\\bS-" "Shift+"
                (replace-regexp-in-string
                 "\\bM-" "Alt+"
                 key t) t) t) t) t) t) t) t) t) t) t) t) t)))

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
        (ergoemacs-shortcut-for-command (cadddr item)))
    nil))

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
                        item)))
                  (cdr menu)))

  ;; Recurse sub menu items
  (mapc (lambda (x)
          (when (and (consp x)
                     (consp (cdr x))
                     (consp (cdr (cdr x)))
                     (consp (cdr (cdr (cdr x))))
                     (eq (car (cdr x)) 'menu-item)
                     (keymapp (car (cdr (cdr (cdr x))))))
                                        ;(message "Submenu: %s" (car (cdr (cdr x))))
            (ergoemacs-preprocess-menu-keybindings (car (cdr (cdr (cdr x)))))))
	(cdr menu)))

(defun ergoemacs-shortcut-for-command (cmd)
  (let ((key (key-description (where-is-internal cmd nil t nil t))))
    (when ergoemacs-debug (message "Menu KEY Shortcut \"%s\"" key))
    (ergoemacs-kbd-to-key key)))

;; Preprocess menu keybindings...

(defun ergoemacs-menus-on ()
  "Turn on ergoemacs menus instead of emacs menus."
  (interactive)
  (ergoemacs-menu-bar-file-menu)
  (define-key global-map [menu-bar file] (cons "File" ergoemacs-menu-bar-file-menu))
  (define-key global-map [menu-bar edit] (cons "Edit" ergoemacs-menu-bar-edit-menu)))

(defun ergoemacs-menus-off ()
  "Turn off ergoemacs menus instead of emacs menus"
  (interactive)
  (define-key global-map [menu-bar file] (cons "File" ergoemacs-menu-bar-old-file-menu))
  (define-key global-map [menu-bar edit] (cons "Edit" ergoemacs-menu-bar-old-edit-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-menus.el ends here
