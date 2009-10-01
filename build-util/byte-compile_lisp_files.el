; -*- coding: utf-8 -*-

;; Create byte compiled files for all .el files, that are under the parent dir. 

(require 'find-lisp)
(mapc
 (lambda (x) (byte-compile-file x))
 (find-lisp-find-files
  (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "../"))
  "\\.el$"))
