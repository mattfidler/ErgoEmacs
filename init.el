; -*- coding: utf-8 -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; (load "ergonomic_keybinding_dvorak")
;; (load "ergonomic_keybinding_qwerty")

(load "ergoemacs_keybindings_minor_mode/ergoemacs_minor_mode.el")
(ergoemacs-mode 1)


(load "init_functions.el")
(load "init_keybinding.el")
(load "init_load_packages.el")
(load "init_settings.el")
(load "init_clean_menus.el")
