;;; xah-html6-mode.el --- Major mode for editing html6. -*- coding: utf-8 -*-

;; http://xahlee.info/comp/html6.html

;;; HISTORY
;; version 0.2, 2011-05-27
;; version 0.1, 2010-12-17

(defvar html6-font-lock-defaults nil "Value for font-lock-defaults.")

(setq html6-font-lock-defaults
      '(("〔\\([^ ]+?\\) " . (1 font-lock-function-name-face))
        ("“\\([^ ]+?\\)”" . (1 font-lock-string-face))
        ("「\\([^」]+\\)」" . (1 font-lock-variable-name-face))
        ) )

(define-derived-mode xah-html6-mode fundamental-mode
  (setq font-lock-defaults '(html6-font-lock-defaults))
  (setq mode-name "HTML6")
  )

(provide 'html6)
