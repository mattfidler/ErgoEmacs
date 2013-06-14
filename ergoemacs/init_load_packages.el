; -*- coding: utf-8 -*-

;; makes it possible to have some modes not have linum-mode on when global-linum-mode is on
;; 2013-04-19, as of GNU Emacs 24.3.1, linum-mode will freeze emacs for about 10 minutes when opening a jpg file of 10 megabytes size
(require 'linum-off)

;; ; redo mode
(add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/") )
(require 'undo-tree)
(defalias 'redo 'undo-tree-redo)
(global-undo-tree-mode 1)


;;;; language modes

;;; php mode
(autoload 'php-mode "php-mode" "php mode by Aaron S Hawley." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Major moder for editing Visual Basic code." t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode)) ;VBscript
(add-to-list 'auto-mode-alist '("\\.vb\\'" . visual-basic-mode))  ;visual basic .NET file
(add-to-list 'auto-mode-alist '("\\.bas\\'" . visual-basic-mode)) ;visual basic form
(add-to-list 'auto-mode-alist '("\\.frm\\'" . visual-basic-mode)) ;basic language source
(add-to-list 'auto-mode-alist '("\\.cls\\'" . visual-basic-mode)) ;C++ class definition file

;;; csharp mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;; for editing Windows's cmd.exe's script; batch, “.bat” file mode.
(autoload 'dos-mode "dos" "A mode for editing Windows cmd.exe batch scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . dos-mode))

;;; powershell-mode. http://en.wikipedia.org/wiki/PowerShell
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell scripts." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;;; powershell interactive shell
(when (string-equal system-type "windows-nt")
  (autoload 'powershell "powershell" "Interactive shell for Microsoft PowerShell." t)
)

;;; xlsl-mode. http://en.wikipedia.org/wiki/Linden_Scripting_Language
(autoload 'xlsl-mode "xlsl-mode" "Load xlsl-mode for editing Linden Scripting Lang." t)
(add-to-list 'auto-mode-alist '("\\.lsl\\'" . xlsl-mode))

;;; AutoHotKey (ahk) mode (a keyboard macro for Windows)
(autoload 'xahk-mode "xahk-mode" "AutoHotKey mode" t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;;; tuareg mode for ML/Caml/OCaml lang
;; (add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/tuareg-2.0.4/"))
;; (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; xbbcode-mode for editing BBCode
(add-to-list 'auto-mode-alist '("\\.bbcode\\'" . xbbcode-mode))
(autoload 'xbbcode-mode "xbbcode-mode" "Load xbbcode-mode for editing BBCode." t)


;;;; productivity, enhancement, or minor modes

;;; yasnippet template system
(add-to-list 'load-path
 (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/yasnippet-0.6.1c/")
 )
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory
 (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/yasnippet-0.6.1c/snippets/")
 )
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

;;; enhanced “directory viewer”/“file manager” (diredplus mode)
(require 'dired+)

;;; enhanced execute-extended-command
;; make the menu key call smex's M-x. However, for some reason this doesn't work. Works only when this code is manually executed after emacs start.
(require 'smex)
(smex-initialize)
(when (member 'smex features)
  (cond
   ((string-equal system-type "windows-nt")
    (progn
      (global-set-key (kbd "<apps>") 'smex)
      (global-set-key (kbd "<S-apps>") 'smex-major-mode-commands)
      )
    )
   ((string-equal system-type "darwin")
    nil)
   ((string-equal system-type "gnu/linux")
    (progn
      (global-set-key (kbd "<menu>") 'smex)
      (global-set-key (kbd "<S-menu>") 'smex-major-mode-commands)
      )
    )
   )
  )

;; a user interface library used by auto-complete
(add-to-list 'load-path
 (concat (file-name-directory (or load-file-name buffer-file-name)) 
         "../packages/popup/" ) )

;; auto-complete 1.4.0 (a enhanced word completion for computer languages)
(add-to-list 'load-path
 (concat (file-name-directory (or load-file-name buffer-file-name))
                     "../packages/auto-complete/" ) )
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
 (concat (file-name-directory (or load-file-name buffer-file-name))
                     "../packages/auto-complete/dict/" ))
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode 1)

;; enhanced text selection, expand-region
;; 2013-04-19 turned it off. Because, it's slow, and often incorrect. e.g. in js-mode, itone case took it few seconds …
;(add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/expand-region/"))
;(require 'expand-region)
;(when
;    (member 'expand-region features)
;  (if
;      (fboundp 'ergoemacs-key )
;      (progn (ergoemacs-key "M-8" 'er/expand-region "←region→")
;             (ergoemacs-key "M-9" 'er/contract-region "→region←")
;             )
;    (progn
;      (global-set-key (kbd "M-8") 'er/expand-region)
;      (global-set-key (kbd "M-9") 'er/contract-region)
;      )
;    )
;  )

;;; color CSS color code
(autoload 'rainbow-mode "rainbow-mode" "Colorize strings that represent colors, e.g. #aabbcc." t nil)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'xah-css-mode-hook 'rainbow-mode)

;;; make the formfeed char (^L) display as a line
(require 'page-break-lines)
(global-page-break-lines-mode 1)

;;; xub-mode for browsing Unicode characters
(autoload 'xub-mode "xub-mode" "Load Unicode browsing mode." t)

;;; xmsi-mode 〔xmsi-math-symbols-input.el〕 for inputting math (Unicode) symbols.
(autoload 'xmsi-mode "xmsi-math-symbols-input" "Load xmsi minor mode for inputting math (Unicode) symbols." t)

;; ;; Hunspell only for Windows until we fix http://code.google.com/p/ergoemacs/issues/detail?id=51
;; (when (and (string-equal system-type "windows-nt")
;; 	   (executable-find "hunspell"))
;;   (progn
;;     (add-to-list 'load-path
;; 		 (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/rw-hunspell/"))
;;     (require 'rw-hunspell)
;;     (rw-hunspell-setup)
;;     )
;;   )

;; ;; Hunspell
;; (when (string-equal system-type "windows-nt")
;;   (when (or (file-exists-p "../hunspell")
;;             (file-exists-p "C:\\Program Files (x86)\\ErgoEmacs\\hunspell")
;;             )
;;     (progn
;;       (add-to-list 'load-path
;;        (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/rw-hunspell/") )
;;       (require 'rw-hunspell)
;;       (rw-hunspell-setup)
;;       ) ) )

;; ;; ;; Hunspell. TODO: See http://code.google.com/p/ergoemacs/issues/detail?id=51
;; ;; (when (or (executable-find "hunspell") (executable-find "aspell") (executable-find "ispell"))
;; ;; (progn
;; ;;       (add-to-list 'load-path
;; ;;        (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/rw-hunspell/") )
;; ;;       (require 'rw-hunspell)
;; ;;       (rw-hunspell-setup)
;; ;;       )
;; ;; )

;; ;; speck-mode. To use, call speck-mode.
;; ;; TODO: http://code.google.com/p/ergoemacs/issues/detail?id=56
;; ;; http://code.google.com/p/ergoemacs/issues/detail?id=74
;; (autoload 'speck-mode "speck" "Background spell checking mode, alternative to flyspell-mode." t)
;; (setq speck-engine (quote Hunspell))
;; (setq speck-hunspell-language-options
;;       (quote (("da" utf-8 nil t nil)
;;               ("de" iso-8859-1 nil t nil)
;;               ("en" utf-8 nil nil nil)
;;               ("fr" iso-8859-1 nil nil nil)
;;               ("it" iso-8859-1 nil nil nil)
;;               ("ru" koi8-r nil nil nil))))
;; (setq speck-hunspell-program (concat (file-name-directory (or load-file-name buffer-file-name)) "../hunspell/hunspell.exe"))
;; (setq speck-hunspell-library-directory (concat (file-name-directory (or load-file-name buffer-file-name)) "../hunspell/"))
;; (setq speck-hunspell-default-dictionary-name "en_US")

;; record command call statistics
(require 'keyfreq)
(setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
(setq keyfreq-autosave-timeout 600)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; add a tab bar widget
;; (require 'tabbar)
;; (tabbar-mode 1)
;; (require 'tabbar-ruler)
;; 2010-09-30. tabbar problem: sometimes after a while, tabbar mode will slow you down. For example, in your OS, set key repeat rate to highest, so that holding down a key will keep repeating the letter. Then, when using emacs for while (maybe few hours or few days), holding down a key, you'll see jumpy behavior.
;; 2013-06-01 tabbar and tabbar-ruler together makes emacs take some 20% cpu even when idle. without, it's 1% cpu.

;; display horizontal line for the Form Feed char (ASCII 12, ^L)
;; The Form Feed char is often used in elisp source code for marking sections. The command forward-page (and backward-page) moves to the next form feed char.
;; (require 'pp-c-l)

;; some convenient commands to lookup reference sites on web
(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)
(autoload 'lookup-google "lookup-word-on-internet" "Lookup in browser" t)
(autoload 'lookup-wikipedia "lookup-word-on-internet" "Lookup in browser" t)
(autoload 'lookup-word-dict-org "lookup-word-on-internet" "Lookup in browser" t)
(autoload 'lookup-word-definition "lookup-word-on-internet" "Lookup in browser" t)
(autoload 'lookup-wiktionary "lookup-word-on-internet" "Lookup word in browser" t)
(autoload 'lookup-php-ref "lookup-word-on-internet" "Lookup word in browser" t)

;; dictionary client for dict.org . To use, call dictionary-lookup-definition to lookup def of word under cursor.
(add-to-list 'load-path
 (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/dictionary-1.8.7")
)
(autoload 'dictionary-search "dictionary" "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary" "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary" "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary" "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary" "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary" "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary" "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary" "Enable/disable dictionary-tooltip-mode for all buffers" t)

;; loads tramp. This is to fix a dired recursive load bug, see: http://ergoemacs.org/emacs/emacs_on_ubuntu_linux.html
(require 'tramp)
