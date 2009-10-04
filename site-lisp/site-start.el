; -*- coding: utf-8 -*-

(defun fullpath-relative-to-call-location (file-path)
  "Returns the full path of FILE-PATH, relative to file location where this function is called.

Example: If the file that calls fullpath-relative-to-call-location is at:
/Users/xah/web/emacs/emacs_init.el then,
 (fullpath-relative-to-call-location \"xyz.el\")
returns
 /Users/xah/web/emacs/xyz.el

This function solves 2 problems.
 (1) if you have file A that contains the line “(load \"B\")”,
and file B calls load with a relative path “(load \"../C\")”,
then Emacs will complain about unable to find C. Because, emacs
does not switch current directory with “load”. To solve this, you
call “(load (fullpath-relative-to-call-location ‹your path of C
relative to B›))” in B.  A common solution is add all dirs to
load path, but that is not always desirable.

 (2) To know the current file's full path, emacs has 2 ways:
load-file-name and buffer-file-name. If the file is called by
“load”, then load-file-name contains the file's path, but not
buffer-file-name. But if the file is called by eval-buffer, then
load-file-name is nil. So, to know the running file's path
regardless how it is called, use “(or load-file-name
buffer-file-name)”. This is part of this function."
  (concat (file-name-directory (or load-file-name buffer-file-name)) file-path)
)

(defalias 'fullpath 'fullpath-relative-to-call-location)

(load (fullpath "../ergoemacs/init") )
