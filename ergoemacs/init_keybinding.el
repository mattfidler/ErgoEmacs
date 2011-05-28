;-*- coding: utf-8 -*-

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-normal-size)

(global-set-key (kbd "C-S-z") 'redo)

(defun browser-nav-keys ()
  "Add some browser styled nav keys for Info-mode.
  The following keys and mouse buttons are added:
 【Backspace】 for `Info-history-back'
 【Shift+Backspace】 for `Info-history-forward'."
  (progn
    (local-set-key (kbd "<backspace>") 'Info-history-back)
    (local-set-key (kbd "<S-backspace>") 'Info-history-forward)
    )
  ;; note: Backspace key for previous page isn't support by Firefox on Linux. That key isn't support in Mac's Safari neither until i think 2008 or so.
  )

(add-hook 'Info-mode-hook 'browser-nav-keys)

;; In Windows, Alt+F4 closes the frame (or kill emacs if it is the last frame)
(if (and (boundp 'w32-initialized) w32-initialized)
    (global-set-key (kbd "M-<f4>") 'close-frame))
