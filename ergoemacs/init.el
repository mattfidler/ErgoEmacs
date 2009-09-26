; -*- coding: utf-8 -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path 
	     (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages"))

(load "init_version")
(load "init_functions")
(load "init_keybinding")
(load "init_load_packages")
(load "init_settings")
(load "init_clean_menus")

;; The ergoemacs mode is the last thing to be initialized (so we can
;; hook all packages, like iswitchb)
(load "ergoemacs-keybindings/ergoemacs-mode")
(ergoemacs-mode 1)

;; (server-start) ; this keeps emacs running just one instance. For example, a user double clicks a file, it'll just switch to a existing instance. Not sure this is best approach.
