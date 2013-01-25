;;; ergoemacs-extras.el --- Generate Ergoemacs Extras  -*- coding: utf-8 -*-
;;; Code:

(defgroup ergoemacs-extras nil
  "Documentation and script generation"
  :group 'ergoemacs-mode)

(defcustom ergoemacs-ahk-script-snippets
  '((previous-line "\n  SendInput {Up}\n  return")
    (next-line "\n  SendInput {Down}\n  return")
    (backward-char "\n SendInput {Left}\n return")
    (forward-char "\n SendInput {Right}\n return")
    (backward-word "\n SendInput {Ctrl down}{Left}{Ctrl up}\n  return")
    (forward-word "\n  SendInput {Ctrl down}{Right}{Ctrl up}\n  return")
    (move-beginning-of-line "\n  SendInput {Home}\n  return")
    (move-end-of-line "\n SendInput {End}\n return")
    (delete-backward-char "\n SendInput {Backspace}\n  return")
    (delete-char "\n SendInput {Delete}\n return")
    (scroll-down "\n SendInput {PgUp}\n return")
    (scroll-up "\n SendInput {PgDn}\n return")
    (isearch-forward "\n  SendInput {Ctrl down}{f}{Ctrl Up}\n return")
    (query-replace "\n  SendInput {Ctrl down}{h}{Ctrl Up}\n return")
    (backward-kill-word " SendInput {Shift down}{Home}{Shift up}{Ctrl down}{x}{Ctrl up}
  return")
    (kill-word "\n  SendInput {Ctrl down}{Shift down}{Right}{Ctrl up}{Shift up}{Ctrl down}{x}{Ctrl up}
  return")
    (kill-line "\n SendInput {Shift down}{End}{Shift up}{Ctrl down}{x}{Ctrl up}
  return")
    (ergoemacs-kill-line-backward "\n  SendInput {Shift down}{Home}{Shift up}{Ctrl down}{x}{Ctrl up}
  return")
    (ergoemacs-cut-line-or-region "\n SendInput {Ctrl down}{x}{Ctrl up}
  return")
    (ergoemacs-copy-line-or-region "\n SendInput {Ctrl down}{c}{Ctrl up}
  return")
    (yank "\n SendInput {Ctrl down}{v}{Ctrl up}
  return")
    (undo "\n  SendInput {Ctrl down}{z}{Ctrl up}
  return")
    (redo "\n SendInput {Ctrl down}{y}{Ctrl up}
  return"))
  "Autohotkey script snippets that define equivalent actions to emacs functions"
  :type '(repeat
          (list
           (symbol :tag "Emacs Function")
           (string :tag "Autohotkey Equivalent Snippet")))
  :group 'ergoemacs-extras)

;;; Keyboard Settings

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
         (symbol-value (ergoemacs-get-variable-layout)))
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
         (symbol-value (ergoemacs-get-variable-layout)))
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
      (setq ret (replace-match
                 (concat "-S-"
                         (downcase (match-string 1 ret)) (match-string 2 ret))
                 t t ret)))
    
    (while (string-match "M-" ret)
      (setq ret (replace-match "!" t t ret)))
    
    (while (string-match "S-" ret)
      (setq ret (replace-match "+" t t ret)))
    
    (while (string-match "C-" ret)
      (setq ret (replace-match "^" t t ret)))
    
    (symbol-value 'ret)))

(defun ergoemacs-get-layouts-ahk-ini ()
  "Gets the easymenu entry for ergoemacs-layouts."
  (with-temp-buffer
    (insert "[Layouts]\n")
    (mapcar
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
         (insert lay)
         (insert "=")
         (insert doc)
         (insert "\n")))
     (ergoemacs-get-layouts))
    (buffer-string)))

(defun ergoemacs-get-variants-ahk-ini ()
  "Gets the list of all known variants and the documentation associated with the variants."
  (with-temp-buffer
    (insert "[Variants]\n")
    (insert "Standard=Standard Variant\n")
    (let ((lays (sort (ergoemacs-get-variants) 'string<)))
      (mapcar
       (lambda(lay)
         (let* ((variable (intern (concat "ergoemacs-" lay "-variant")))
                (alias (condition-case nil
                           (indirect-variable variable)
                         (error variable)))
                (is-alias nil)
                (doc nil))
           (setq doc (or (documentation-property variable 'group-documentation)
                         (progn
                           (setq is-alias t)
                           (documentation-property alias 'group-documentation))))
           (insert lay)
           (insert "=")
           (insert doc)
           (insert "\n")))
       lays))
    (buffer-string)))

(defun ergoemacs-get-ahk-keys-ini ()
  "Get ahk keys for all variants/ahk combinations and put into INI file."
  (with-temp-buffer
    (let ((old-lay ergoemacs-variant))
      (ergoemacs-set-default 'ergoemacs-variant nil)
      (mapc
       (lambda(x)
         (ergoemacs-setup-keys-for-layout x)
         (insert (concat "[" x "-Standard]\n"))
         (mapc
          (lambda(y)
            (message "Generating AHK ini for %s Standard" x)
            (when (assoc (nth 1 y) ergoemacs-ahk-script-snippets)
              (insert (symbol-name (nth 1 y)))
              (insert "=")
              (insert (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y))))
              (insert "\n")))
          (symbol-value (ergoemacs-get-variable-layout))))
       (ergoemacs-get-layouts))
      (mapc
       (lambda(z)
         (ergoemacs-set-default 'ergoemacs-variant z)
         (mapc
          (lambda(x)
            (ergoemacs-setup-keys-for-layout x)
            (insert (concat "[" x "-" z "]\n"))
            (mapc
             (lambda(y)
               (message "Generating AHK ini for %s %s" x z)
               (when (assoc (nth 1 y) ergoemacs-ahk-script-snippets)
                 (insert (symbol-name (nth 1 y)))
                 (insert "=")
                 (insert (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y))))
                 (insert "\n")))
             (symbol-value (ergoemacs-get-variable-layout))))
          (ergoemacs-get-layouts)))
       (ergoemacs-get-variants))
      (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)
      (ergoemacs-set-default 'ergoemacs-variant old-lay))
    (buffer-string)))

(defun ergoemacs-gen-ahk (&optional extra)
  "Generates autohotkey for all layouts and variants"
  (interactive)
  (let ((xtra (or extra "ahk"))
        not-first
        (extra-dir))
    (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
    (if (not (file-exists-p extra-dir))
        (make-directory extra-dir t))
    (setq extra-dir (expand-file-name xtra extra-dir))
    (if (not (file-exists-p extra-dir))
        (make-directory extra-dir t))
    (setq file (expand-file-name "ergoemacs.ini" extra-dir))
    (with-temp-file file
      (insert (ergoemacs-get-layouts-ahk-ini))
      (insert (ergoemacs-get-variants-ahk-ini))
      (insert (ergoemacs-get-ahk-keys-ini)))
    (setq file (expand-file-name "ergoemacs.ahk" extra-dir))
    (with-temp-file file
      (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir))
      (goto-char (point-max))
      (insert "\n;; Autogenerated functions below\n\n")
      (mapc
       (lambda(x)
         (insert "\n")
         (insert (symbol-name (nth 0 x)))
         (insert ":\n")
         (insert (nth 1 x))
         (insert "\n\n"))
       ergoemacs-ahk-script-snippets))
    (message "Generated ergoemacs.ahk")
    (when (executable-find "ahk2exe")
      (shell-command (format "ahk2exe /in %s" file))
      (message "Generated ergoemacs.exe"))))


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
              `(,@(symbol-value (ergoemacs-get-fixed-layout))
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
          (setq txt (assoc (format "M-%s" (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) (symbol-value (ergoemacs-get-variable-layout))))
          (if (not txt)
              (setq txt "")
            (if (>= (length txt) 3)
                (setq txt (nth 2 txt))
              (setq txt "")))
          
          (when (string= txt "")
            (setq txt (all-completions (format "M-%s " (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) (symbol-value (ergoemacs-get-variable-layout))))
            (if (= 0 (length txt))
                (setq txt "")
              (setq txt "prefix")))
          
          (unless (string= "" txt)
            (when (search-forward (format ">M%s<" i) nil t)
              (replace-match  (format ">%s<" txt) t t)))
          
          (goto-char (point-min))
          (setq txt (assoc (format "C-%s" (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) (symbol-value (ergoemacs-get-variable-layout))))
          (if (not txt)
              (setq txt "")
            (if (>= (length txt) 3)
                (setq txt (nth 2 txt))
              (setq txt "")))
          
          (when (string= txt "")
            (setq txt (all-completions (format "C-%s " (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))) (symbol-value (ergoemacs-get-variable-layout))))
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
               (setq txt (assoc x (symbol-value (ergoemacs-get-variable-layout))))
               (if (not txt)
                   (setq txt "")
                 (if (>= (length txt) 3)
                     (setq txt (nth 2 txt))
                   (setq txt "")))
               
               (when (string= txt "")
                 (setq txt (all-completions (format "%s " x) (symbol-value (ergoemacs-get-variable-layout))))
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
  "Generate SVGs for all the defined layouts and variants."
  (interactive)
  (let* ((lay (or layouts (ergoemacs-get-layouts)))
         (saved-variant ergoemacs-variant))
    (mapc
     (lambda(x)
       (message "Generate SVG for %s" x)
       (ergoemacs-gen-svg x)
       (ergoemacs-set-default 'ergoemacs-variant nil)
       (ergoemacs-gen-svg x "kbd-ergo.svg" "ergo-layouts")
       (mapc
        (lambda(y)
          (ergoemacs-set-default 'ergoemacs-variant y)
          (ergoemacs-gen-svg x "kbd-ergo.svg" (concat y "/ergo-layouts")))
        (sort (ergoemacs-get-variants) 'string<))
       (ergoemacs-set-default 'ergoemacs-variant saved-variant))
     lay)))
(provide 'ergoemacs-extras)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-extras.el ends here
