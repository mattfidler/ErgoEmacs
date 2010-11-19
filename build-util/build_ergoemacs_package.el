; -*- coding: utf-8 -*-

;; 2009-10-01, 2010-11-15
;; This elisp script builds a ErgoEmacs elisp package.
;; Effectively, it creates a new zip file, nothing else.

;; What does it do:
;; copy the whole “ergoemacs” dir into some dest dir. The “ergoemacs” is the dir from root checked out from svn.
;; remove all .svn dirs.
;; remove other files and dir such as Makefile and win32-setup etc.

;; HOW TO RUN IT
;; First, change the version number in variable “destDir”.
;; then, just eval-buffer.
;; The result will be a new zip file (and a unzipped dir) at the root of your svn checkout.
;; For example, if your svn checkout path is
;;   c:/Users/xah/ErgoEmacs_Source
;; then the following are created
;;   c:/Users/xah/ErgoEmacs_Source/ergoemacs_1.9.1
;;   c:/Users/xah/ErgoEmacs_Source/ergoemacs_1.9.1.zip

;; This script requires unix “find”, “rm”, “cp”, etc.

(defvar sourceDir nil "The ergoemacs source code dir in repository.")
(setq sourceDir "../")

(defvar destDir nil "The output dir.")
(setq destDir "../../ergoemacs_1.9.1/")


;; main


;; if previous build dir and zip file exist, remove them.
(let (destDirNoSlash) 
  (setq destDirNoSlash (substring destDir 0 -1))
  (if (file-exists-p destDirNoSlash) (shell-command (concat "rm -R " destDirNoSlash) ))
  (if (file-exists-p (concat destDirNoSlash ".zip" )) 
      (delete-file (concat destDirNoSlash ".zip" ))
    )
  )

(make-directory destDir t)
 (shell-command (concat "cp -R " sourceDir " " destDir) )

(shell-command (concat "find " destDir " -depth -name \".svn\" -type d -exec rm -R {} ';'" ) )

(shell-command (concat " rm -R " destDir "win32-setup"))
(delete-file (concat destDir "Makefile"))
(delete-file (concat destDir "build-util/build_ergoemacs_package.el"))

;; byte compile elc files
(load-file (concat destDir "build-util/byte-compile_lisp_files.el"))

;; zip it
(let ((destDirSansSlash (substring destDir 0 -1)))
  (shell-command (concat "zip -r " destDirSansSlash ".zip " destDirSansSlash ) )
)



;; TODO
;; ideally, change all shell calls to elisp functions so it's not dependent on shell.
;; using elisp for build is just experimental. We can revert to unix shell in the future.

;; currently, the version number is hard coded. We probably want to make use svn's tag feature for version stapm, for building both Windows release and elisp package release.
