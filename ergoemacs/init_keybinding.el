;-*- coding: utf-8 -*-

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-normal-size)

(global-set-key (kbd "C-S-z") 'redo)

;; In Windows, Alt+F4 closes the frame (or kill emacs if it is the last frame)
(if (and (boundp 'w32-initialized) w32-initialized)
    (global-set-key (kbd "M-<f4>") 'close-frame))
