; -*- coding: utf-8 -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "../ergoemacs/init")

;; Note: if you have a file such as ~.emacs that tells emacs to load another file such as this file, and in the second file, you call “load” with a relative path, such as (load "../some/init"), emacs complains about not able to find the file. The error message is “File error: Cannot open load file, ../some/init”. It appears, when emacs load files, it does not change its current dir.
;; this is extremely annoying.
;; One way to solve it is to add the file dir into the load-path, eg. (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
;; but that is not ideal because you don't really want the dir to be perm part of the load-path.
;; So, i tried the following:
;; (load-file
;;  (expand-file-name (concat "../"
;;                            (file-relative-name
;;                             (file-name-directory
;;                              (or load-file-name buffer-file-name)))
;;                            "ergoemacs/init.elc"
;;                            )) )
;; after some 30 min, so far doesn't work. I abondoned on dealing with this.
;; Xah Lee, 2009-10-01