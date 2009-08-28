; redo mode
(require 'redo)

;; php mode
(autoload 'php-mode "php-mode" "php mode by Aaron S Hawley" t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; javascript mode
(autoload 'js2-mode "js2-20080616a" "Steve Yegge's javascript mode" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode)) ;VBscript
(add-to-list 'auto-mode-alist '("\\.vb\\'" . visual-basic-mode))  ;visual basic .NET file
(add-to-list 'auto-mode-alist '("\\.bas\\'" . visual-basic-mode)) ;visual basic form
(add-to-list 'auto-mode-alist '("\\.frm\\'" . visual-basic-mode)) ;basic language source
(add-to-list 'auto-mode-alist '("\\.cls\\'" . visual-basic-mode)) ;C++ class definition file

;; powershell-mode
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;; xlsl-mode
(autoload 'xlsl-mode "xlsl-mode" "Load xlsl-mode for editing Linden Scripting Lang." t)
(add-to-list 'auto-mode-alist '("\\.lsl\\'" . xlsl-mode))

;; powershell interactive shell
(autoload 'powershell "powershell" "Start a interactive shell of PowerShell." t)

;; speck-mode
(autoload 'speck-mode "speck" "Improved batch spell-checking mode." t)

;; AutoHotKey mode (a keyboard macro for Windows)
(setq ahk-syntax-directory "c:/Program Files (x86)/AutoHotkey/Extras/Editors/Syntax/")
(autoload 'ahk-mode "ahk-mode.el" "AutoHotKey mode" t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . ahk-mode))

;; yasnippet template system
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "yasnippet-0.6.0b/")
             )
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory
 (concat (file-name-directory (or load-file-name buffer-file-name)) "yasnippet-0.6.0b/snippets/")
 )

;; POV-Ray (3D rendering engine)
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "pov-mode-3.2/"))
(autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
(add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))


; tuareg mode for ML/Caml/OCaml lang
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "tuareg-mode-1.45.6/")
             )
(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)


;; dictionary client for dict.org 
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "dictionary-1.8.7")
)
(autoload 'dictionary-search "dictionary" "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary" "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary" "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary" "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary" "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary" "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary" "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary" "Enable/disable dictionary-tooltip-mode for all buffers" t)


;; display horizontal line for page break char ^L
(require 'pp-c-l)
(setq pp^L-^L-string "                                                           ")
(pretty-control-l-mode 1)
