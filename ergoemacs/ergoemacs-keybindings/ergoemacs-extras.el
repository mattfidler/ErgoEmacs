;;; ergoemacs-extras.el --- Generate Ergoemacs Extras  -*- coding: utf-8 -*-
;;; Code:

(defgroup ergoemacs-extras nil
  "Documentation and script generation"
  :group 'ergoemacs-mode)

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

(defun ergoemacs-trans-ahk (key &optional number)
  "Translates Emacs kbd code KEY to ahk kbd code. "
  (let ((ret key)
        (mod-code 0)
        (case-fold-search nil))
    
    (while (string-match "-\\([A-Z]\\)\\($\\| \\)" ret)
      (setq ret (replace-match
                 (concat "-S-"
                         (downcase (match-string 1 ret)) (match-string 2 ret))
                 t t ret)))
    
    (while (string-match "M-" ret)
      (setq mod-code (+ mod-code 2))
      (setq ret (replace-match (if number "" "!") t t ret)))
    
    (while (string-match "S-" ret)
      (setq mod-code (+ mod-code 8))
      (setq ret (replace-match (if number "" "+") t t ret)))
    
    (while (string-match "C-" ret)
      (setq mod-code (+ mod-code 4))
      (setq ret (replace-match (if number "" "^") t t ret)))
    (if (and number (= 1 (length ret)))
        (setq ret (format "%s%s" (string-to-char ret) (number-to-string mod-code))))
    (symbol-value 'ret)))

(defun ergoemacs-get-layouts-ahk-ini ()
  "Gets the easymenu entry for ergoemacs-layouts."
  (let ((lay-ini "")
        (trans-keys '())
        (i 0))
    (with-temp-buffer
      (insert "[Layouts]\n")
      (mapc
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
           (insert "\n")
           (setq i 1)
           (setq lay-ini (format "%s\n[%s]" lay-ini lay))
           (mapc
            (lambda(x)
              (let ((key (format "%s" (string-to-char x))))
                (add-to-list 'trans-keys `(,x ,key))
                (setq lay-ini (format "%s\n%s=%s" lay-ini i key)))
              (setq i (+ i 1)))
            (symbol-value variable))))
       (ergoemacs-get-layouts))
      (goto-char (point-max))
      (insert lay-ini)
      (insert "\n")
      (buffer-string))))

(defun ergoemacs-get-variants-ahk-ini ()
  "Gets the list of all known variants and the documentation associated with the variants."
  (with-temp-buffer
    (insert "[Variants]\n")
    (insert "Standard=Standard Variant\n")
    (let ((lays (sort (ergoemacs-get-variants) 'string<)))
      (mapc
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
  (flet ((ergoemacs-unset-redundant-global-keys ()))
    (let ((re "")
          lst)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir))
        (goto-char (point-min))
        (while (re-search-forward "^\\([^ \n]*\\):" nil t)
          (add-to-list 'lst (match-string 1))))
      (setq re (format "^%s$" (regexp-opt lst 't)))
      (with-temp-buffer
        (let ((old-lay ergoemacs-variant))
          (ergoemacs-set-default 'ergoemacs-variant nil)
          (mapc
           (lambda(x)
             (ergoemacs-setup-keys-for-layout x)
             ;; (insert (concat "[" x "]\n"))
             ;; (mapc 
             ;;  (lambda(y)
             ;;    (let ((key (car y))
             ;;          (val (cdr y)))
             ;;      (cond
             ;;       ((string= val "=")
             ;;        (setq val "eq"))
             ;;       ((string= val "[")
             ;;        (setq val "ob"))
             ;;       ((string= val "\\")
             ;;        (setq val "bs"))
             ;;       ((string= val "#")
             ;;        (setq val "pn"))
             ;;       ((string= val ";")
             ;;        (setq val "sc"))
             ;;       ((string= val "]")
             ;;        (setq val "cb")))
             ;;      (cond
             ;;       ((string= key "=")
             ;;        (setq key "eq"))
             ;;       ((string= key "[")
             ;;        (setq key "ob"))
             ;;       ((string= key "\\")
             ;;        (setq key "bs"))
             ;;       ((string= key ";")
             ;;        (setq key "sc"))
             ;;       ((string= key "#")
             ;;        (setq key "pn"))
             ;;       ((string= key "]")
             ;;        (setq key "cb")))
             ;;      (insert (format "%s=%s\n" key val))))
             ;;  ergoemacs-translation-assoc)
             (insert (concat "[" x "-Standard]\n"))
             (mapc
              (lambda(y)
                (message "Generating AHK ini for %s Standard" x)
                (when (string-match re (format "%s"(nth 1 y)))
                  (insert (symbol-name (nth 1 y)))
                  (insert "=")
                  (insert (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y)) t))
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
                   (when (string-match re (format "%s" (nth 1 y)))
                     (insert (symbol-name (nth 1 y)))
                     (insert "=")
                     (insert (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y))))
                     (insert "\n")))
                 (symbol-value (ergoemacs-get-variable-layout))))
              (ergoemacs-get-layouts)))
           (ergoemacs-get-variants))
          (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)
          (ergoemacs-set-default 'ergoemacs-variant old-lay))
        (buffer-string)))))

(defun ergoemacs-gen-ahk (&optional extra)
  "Generates autohotkey for all layouts and variants"
  (interactive)
  (let ((xtra (or extra "ahk")) 
        not-first
        (extra-dir)
        file-temp)
    (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
    (if (not (file-exists-p extra-dir))
        (make-directory extra-dir t))
    (setq extra-dir (expand-file-name xtra extra-dir))
    (if (not (file-exists-p extra-dir))
        (make-directory extra-dir t))
    (setq file-temp (expand-file-name "ergoemacs.ini" extra-dir))
    (with-temp-file file-temp
      (insert (ergoemacs-get-layouts-ahk-ini))
      (insert (ergoemacs-get-variants-ahk-ini))
      (insert (ergoemacs-get-ahk-keys-ini)))
    (setq file-temp (expand-file-name "ergoemacs.ahk" extra-dir))
    (with-temp-file file-temp
      (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir)))
    (message "Generated ergoemacs.ahk")
    (when (executable-find "ahk2exe")
      (shell-command (format "ahk2exe /in %s" file-temp))
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
  (ergoemacs-gen-ahk)
  (ergoemacs-bashs layouts)
  (ergoemacs-mac-osx-dicts layouts)
  (find-file (expand-file-name "ergoemacs-extras" user-emacs-directory)))



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


(defun ergoemacs-curr-svg ()
  "Generates the current ergoemacs layout, unless it already exists."
  (interactive)
  (let ((var ergoemacs-variant)
        (layout ergoemacs-keyboard-layout)
        (extra "ergo-layouts")
        (dir "")
        (file ""))
    (when var
      (setq extra (concat var "/ergo-layouts")))
    (setq dir (expand-file-name extra
                                (expand-file-name "ergoemacs-extras" user-emacs-directory)))
    (setq file (expand-file-name (concat "ergoemacs-layout-" layout ".svg") dir))
    (unless (file-exists-p file)
      (message "Generating SVG file...")
      (unless (featurep 'ergoemacs-extras)
        (require 'ergoemacs-extras))
      (ergoemacs-gen-svg layout "kbd-ergo.svg" extra)
      (message "Generated!"))
    (when (interactive-p)
      (browse-url (concat "file://" file)))
    (symbol-value 'file)))

;;;###autoload
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
