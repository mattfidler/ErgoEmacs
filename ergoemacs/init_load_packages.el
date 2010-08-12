; -*- coding: utf-8 -*-

;;;; language modes

;;; php mode
(autoload 'php-mode "php-mode" "php mode by Aaron S Hawley." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;;; javascript. (IDE-like by Steve Yegge. Features a js syntax parser)
(autoload 'js2-mode "js2-20090723b" "IDE-like Javascript mode; features a on-the-fly syntax parser." t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

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
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;;; powershell interactive shell
(autoload 'powershell "powershell" "Interactive shell for PowerShell." t)

;;; mode for lua language
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)) ; lua-mode

;;; xlsl-mode. http://en.wikipedia.org/wiki/Linden_Scripting_Language
(autoload 'xlsl-mode "xlsl-mode" "Load xlsl-mode for editing Linden Scripting Lang." t)
(add-to-list 'auto-mode-alist '("\\.lsl\\'" . xlsl-mode))

;;; AutoHotKey (ahk) mode (a keyboard macro for Windows)
(autoload 'xahk-mode "xahk-mode" "AutoHotKey mode" t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;;; POV-Ray (3D rendering engine) http://en.wikipedia.org/wiki/POV-Ray
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/pov-mode-3.2/"))
(autoload 'pov-mode "pov-mode" "Major mode for working with POV-Ray code." t)
(add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))

;;; mode for Haskell lang. http://en.wikipedia.org/wiki/Haskell_(programming_language)
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/haskell-mode-2.7.0/"))
(load "haskell-mode-2.7.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;; tuareg mode for ML/Caml/OCaml lang
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/tuareg-mode-1.45.7/")
             )
(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing ML/Caml/OCaml code." t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; xbbcode-mode for editing BBCode
(add-to-list 'auto-mode-alist '("\\.bbcode\\'" . xbbcode-mode))
(autoload 'xbbcode-mode "xbbcode-mode" "Load xbbcode-mode for editing BBCode." t)

;;; xub-unicode-browser-mode for browsing Unicode characters
(autoload 'xub-unicode-browser-mode "xub-unicode-browser-mode" "Load Unicode browsing mode." t)


;;;; productivity, enhancement, or minor modes

; redo mode
(require 'redo)

;; Hunspell
(when (string-equal system-type "windows-nt")
  (when (or (file-exists-p "../hunspell")
            (file-exists-p "C:\\Program Files (x86)\\ErgoEmacs\\hunspell")
            )
    (progn
      (add-to-list 'load-path
                   (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/rw-hunspell/") )
      (require 'rw-hunspell)
      (rw-hunspell-setup)
      ) ) ) 

;; ;; Hunspell. TODO: See http://code.google.com/p/ergoemacs/issues/detail?id=51
;; (when (or (executable-find "hunspell") (executable-find "aspell") (executable-find "ispell"))
;; (progn
;;       (add-to-list 'load-path
;;                    (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/rw-hunspell/") )
;;       (require 'rw-hunspell)
;;       (rw-hunspell-setup)
;;       )
;; )

;; speck-mode. To use, call speck-mode.
;; TODO: http://code.google.com/p/ergoemacs/issues/detail?id=56
;; http://code.google.com/p/ergoemacs/issues/detail?id=74
(autoload 'speck-mode "speck" "Background spell checking mode, alternative to flyspell-mode." t)
(setq speck-engine (quote Hunspell))
(setq speck-hunspell-language-options
      (quote (("da" utf-8 nil t nil)
              ("de" iso-8859-1 nil t nil)
              ("en" utf-8 nil nil nil) 
              ("fr" iso-8859-1 nil nil nil)
              ("it" iso-8859-1 nil nil nil)
              ("ru" koi8-r nil nil nil))))
(setq speck-hunspell-program (concat (file-name-directory (or load-file-name buffer-file-name)) "../hunspell/hunspell.exe"))
(setq speck-hunspell-library-directory (concat (file-name-directory (or load-file-name buffer-file-name)) "../hunspell/"))
(setq speck-hunspell-default-dictionary-name "en_US")

;; yasnippet template system
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/yasnippet-0.6.1c/")
             )
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory
 (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/yasnippet-0.6.1c/snippets/")
 )
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))


;; Highlight occurrence of current word, and move cursor to next/prev occurrence
;; see http://xahlee.org/emacs/modernization_isearch.html
(require 'highlight-symbol)
;; temp hotkeys
(global-set-key (kbd "<f10>") 'highlight-symbol-at-point) ; this is a toggle
(global-set-key (kbd "<f11>") 'highlight-symbol-next)
(global-set-key (kbd "<f12>") 'highlight-symbol-prev)

;; command-frequency minor mode. To use, call command-frequency.
(autoload 'command-frequency "command-frequency" "minor mode for logging emacs command calls for usage insights by statistics." t)

;; turn command-frequency on
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;; display horizontal line for the Form Feed char (ASCII 12, ^L)
;; The Form Feed char is often used in elisp source code for marking sections. The command forward-page (and backward-page) moves to the next form feed char.
(require 'pp-c-l)
(setq pp^L-^L-string "                                                           ")
(pretty-control-l-mode 1)

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
