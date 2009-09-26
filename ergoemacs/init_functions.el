;-*- coding: utf-8 -*-

(defun text-scale-normal-size ()
  "Set the height of the default face in the current buffer to its default value."
  (interactive)
  (text-scale-increase 0))

(defun toggle-line-move-visual ()
  "Toggle behavior of up/down arrow key, by visual line vs logical line."
  (interactive)
  (if line-move-visual
      (setq line-move-visual nil)
    (setq line-move-visual t))
  )

(defun cmd-shell (&optional arg)
  "Run cmd.exe (WinNT) or command.com shell. A numeric prefix
arg switches to the specified session, creating it if necessary."
  (interactive "P")
  (let ((buf-name (cond ((numberp arg)
			 (format "*cmd<%s>*" arg))
			(arg 
			 (generate-new-buffer-name "*cmd*"))
			(t
			 "*cmd*")))
	(explicit-shell-file-name (or (and (w32-using-nt) "cmd.exe")
				      "command.com")))
    (shell buf-name)))

(defun msys-shell (&optional arg)
  "Run MSYS shell (sh.exe).  It's like a Unix Shell in Windows.
A numeric prefix arg switches to the specified session, creating
it if necessary."
  (interactive "P")
  (let ((buf-name (cond ((numberp arg)
			 (format "*msys<%d>*" arg))
			(arg
			 (generate-new-buffer-name "*msys*"))
			(t
			 "*msys*")))
	(explicit-shell-file-name "sh.exe"))
    (shell buf-name)))

(defun soft-wrap-lines ()
  "Make lines wrap at window edge and on word boundary,
in current buffer."
  (interactive)
  (setq truncate-lines nil)
  (setq word-wrap t)
  )