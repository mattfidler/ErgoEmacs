; 2009-08-13

;; nxml mode. in emacs 23 already
;; (if (string= (substring-no-properties emacs-version 0 2) "23" )
;;     nil
;;   (progn
;;     (load "nxml-mode-20041004/rng-auto")
;;     (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
;;     (add-to-list 'auto-mode-alist '("\\.kml\\'" . nxml-mode))
;;     )
;;   )

;; css mode. In emacs 23 already.
;; (if (string= (substring-no-properties emacs-version 0 2) "23" )
;;     nil
;;   (autoload 'css-mode "css-mode" "css mode" t)
;;   )
