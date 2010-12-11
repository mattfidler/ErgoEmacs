; -*- coding: utf-8 -*-

;; 2009-10-01, 2010-11-15
;; This elisp script builds a ErgoEmacs elisp package.
;; Effectively, it creates a new zip file, nothing else.

;; This script is experimental. Best to use the make util at
;; ergoemacs/Makefile
;; for now.

;; What does it do:
;; copy the whole “ergoemacs” dir into some dest dir. The “ergoemacs” is the dir from root checked out from svn.
;; remove all .svn dirs.
;; remove other files and dir such as Makefile and win32-setup etc.

;; HOW TO RUN IT
;; First, change the version number in variable zipDirName”.
;; then, just eval-buffer.
;; The result will be a new zip file (and a unzipped dir) at the root of your svn checkout.
;; For example, if your svn checkout path is
;;   c:/Users/xah/ErgoEmacs_Source
;; then the following are created
;;   c:/Users/xah/ErgoEmacs_Source/ergoemacs_1.9.1
;;   c:/Users/xah/ErgoEmacs_Source/ergoemacs_1.9.1.zip

;; This script requires unix “find”, “rm”, “cp”, etc.

(defvar zipDirName nil "the zip file/dir name")
(setq zipDirName "ergoemacs_1.9.1.1")



(defvar sourceDir nil "The ergoemacs source code dir in repository. By default, this is parent dir of the dir this file is in.")
(setq sourceDir (expand-file-name  (concat (file-name-directory buffer-file-name) "../")) ) ; e.g. "c:/Users/xah/ErgoEmacs_Source/ergoemacs/"

(defvar destDirRoot nil "The output dir. Will be created if doesn't exit. By default, this is 2 dir above this file.")
(setq destDirRoot (expand-file-name  (concat (file-name-directory buffer-file-name) "../../"))) ;

(setq destDirWithZipPath (concat destDirRoot zipDirName "/"))

;; set to absolute path if not already
(setq sourceDir (expand-file-name sourceDir ) ) 
(setq destDirRoot (expand-file-name destDirRoot ) )
(setq destDirWithZipPath (expand-file-name destDirWithZipPath ) )

;; main

;; if previous build dir and zip file exist, remove them.
(if (file-exists-p destDirWithZipPath) (delete-directory destDirWithZipPath t))
(if (file-exists-p (concat destDirWithZipPath ".zip" )) (delete-file (concat destDirWithZipPath ".zip" )) )

;; create the new dest dir
(make-directory destDirWithZipPath t)

;; copy stuff over to dest dir
;; (shell-command (concat "cp -R " sourceDir " " destDirRoot) )
(copy-directory sourceDir destDirWithZipPath )

;; delete “.svn” dir and other files we don't want
(shell-command (concat "find " destDirWithZipPath " -depth -name \".svn\" -type d -exec rm -R {} ';'" ) )

;; (require 'find-lisp)
;; (mapc 'my-process-file
;;  (find-lisp-find-files destDirWithZipPath "\\.svn$")
;;  (find-lisp-find-files "c:/Users/xah/xx2/ergoemacs_1.9.1.1/build-util/" "")
;;  (find-lisp-find-dired-subdirectories "c:/Users/xah/xx2/ergoemacs_1.9.1.1/build-util/")
;; )

;; delete emacs backup files
;; (shell-command (concat "find " destDirWithZipPath " -name \"*~\" -exec rm {} ';'" ) )
(require 'find-lisp)
(mapc 'delete-file (find-lisp-find-files destDirWithZipPath "~$"))

;; delete Windows specific setup dir
;; (shell-command (concat " rm -R " destDirWithZipPath "win32-setup"))
(delete-directory (concat destDirWithZipPath "win32-setup") t)

;; delete misc files we dont need
(delete-file (concat destDirWithZipPath "Makefile"))
(delete-file (concat destDirWithZipPath "build-util/build_ergoemacs_package.el"))

;; byte compile elc files
(load-file (concat destDirWithZipPath "build-util/byte-compile_lisp_files.el"))

;; zip it
(cd destDirRoot)
(shell-command (concat "zip -r " zipDirName ".zip " zipDirName ) )

;; change current dir back
(cd (expand-file-name (file-name-directory buffer-file-name)))


;; TODO
;; ideally, change all shell calls to elisp functions so it's not dependent on shell.
;; using elisp for build is just experimental. We can revert to unix shell in the future.

;; currently, the version number is hard coded. We probably want to make use svn's tag feature for version stapm, for building both Windows release and elisp package release.
