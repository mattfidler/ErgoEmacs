;; -*- coding: utf-8 -*-
;; 2008-09-21, 2009-08-13

;; this script is a “make file” that creates ergonomic keybinding elisp source.

;; It just joins several files together
;; then creates 2 files: ergonomic_keybinding_dvorak.el and ergonomic_keybinding_qwerty.el
;; in a specified dir,
;; then byte compile them.

;; you may get some warning messages such as

;; ergonomic_keybinding_dvorak.el:386:15:Warning: reference to free variable
;;     `w3m-mode-map'

;; That's ok, because it just means those modes are not installed or loaded atm.

(setq output-dir "../")

(defvar fname nil "temp var")


(find-file "kbd_dvorak.el")
(goto-char (point-min))
(while (search-forward-regexp "«\\([^»]+\\)»" nil t)
  (setq fname (match-string 1))
  (replace-match "")
  (goto-char (+ (point) (cadr (insert-file-contents fname) )) ) ; move to end
  )
(write-file (concat output-dir "ergonomic_keybinding_dvorak.el" ) )
(kill-this-buffer)

(find-file "kbd_qwerty.el")
(goto-char (point-min))
(while (search-forward-regexp "«\\([^»]+\\)»" nil t)
  (setq fname (match-string 1))
  (replace-match "")
  (goto-char (+ (point) (cadr (insert-file-contents fname) )) ) ; move to end
  )
(write-file (concat output-dir "ergonomic_keybinding_qwerty.el" ) )
(kill-this-buffer)

(byte-compile-file (concat output-dir "ergonomic_keybinding_dvorak.el" ))
(byte-compile-file (concat output-dir "ergonomic_keybinding_qwerty.el" ))

(message "“Ergoemacs keybinding” build process completed.")
