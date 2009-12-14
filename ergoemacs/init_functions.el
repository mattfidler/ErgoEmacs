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

(defun close-frame ()
  "Closes the current frame or kill emacs if there are just one
frame. It simulates the same functionality of the Close button in
the frame title bar."
  (interactive)
  (if multiple-frames
      (delete-frame)
    (save-buffers-kill-terminal)))

(defun list-text-editing-modes ()
  "Display a list of all text editing related major modes.

The list includes all major modes for editing programing language
files or such things as BBCode, but does not include major modes
for dired, irc, mail, shell, etc."
  (interactive)
(with-output-to-temp-buffer "*Major Modes for Text Editing*"
  (princ 
         "###############################################
# This is a list of text-editing related major modes that comes with ErgoEmacs.
# The lines are formatted this way:
# ‹purpose/language name› • ‹command name›

# The list is not complete.
# please help by adding modes you use at
# http://code.google.com/p/ergoemacs/issues/detail?id=64

AutoHotKey • ahk-mode
BBCode • xbbcode-mode
Bash • sh-mode
C • c-mode
C++ • c++-mode
CSS • css-mode
Emacs Lisp • emacs-lisp-mode
HTML • html-mode
Haskell • haskell-mode
Java • java-mode
Javascript • espresso-mode
Javascript • js2-mode
LaTeX • latex-mode
Linden Scripting Language • xlsl-mode
OCaml • tuareg-mode
PHP • php-mode
Perl • cperl-mode
PowerShell • powershell-mode
Python • python-mode
Ruby • ruby-mode
TCL • tcl-mode
Visual Basic • visual-basic-mode
XML • nxml-mode
XML • xml-mode
cmd.exe • dos-mode"
         )
  )
  )