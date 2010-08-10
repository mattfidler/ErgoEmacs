(setq xx '(
"add-hook"
"append"
"apply"
"assq"
"beginning-of-line"
"condition-case"
"consp"
"defalias"
"defsubst"
"delete-region"
"dolist"
"expand-file-name"
"fboundp"
"file-name-nondirectory"
"forward-line"
"function"
"get"
"mapcar"
"match-string"
"push"
"repeat"
"require"
"set"
"set-buffer"
"string"
"string="
"widget-get"
"with-current-buffer"
) )

(defun ff (arg)
  "DOCSTRING"
  (interactive)
  (let (fname)
    (setq fname (concat arg ".yasnippet"))

 (find-file fname)
  (insert "#contributor: Xah Lee (XahLee.org)\n#name: " arg "\n# --\n")
  (insert "(" arg " $0 )" )
  ;; (save-buffer)
  ;; (kill-buffer (current-buffer))
    ))

(mapc 'ff xx)
