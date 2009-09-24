; -*- coding: utf-8 -*-

(defconst ergoemacs-version "1.6")
(defconst ergoemacs-url "http://code.google.com/p/ergoemacs/")
(defconst ergoemacs-url-authors "http://code.google.com/p/ergoemacs/wiki/AuthorsAndAcknowledgement")
(defconst ergoemacs-url-contrib "http://code.google.com/p/ergoemacs/wiki/HowToContribute")

;; ErgoEmacs version
(defun emacs-version (&optional here) "\
Return string describing the version of Emacs that is running.
If optional argument HERE is non-nil, insert string at point.
Don't use this function in programs to choose actions according
to the system configuration; look at `system-configuration' instead."
  (interactive "P")
  (let ((version-string
         (format "GNU Emacs %s (%s%s%s) of %s on %s\nErgoEmacs distribution %s"
		 emacs-version
		 system-configuration
		 (cond ((featurep 'motif)
			(concat ", " (substring motif-version-string 4)))
		       ((featurep 'gtk)
			(concat ", GTK+ Version " gtk-version-string))
		       ((featurep 'x-toolkit) ", X toolkit")
		       ((featurep 'ns)
			(format ", NS %s" ns-version-string))
		       (t ""))
		 (if (and (boundp 'x-toolkit-scroll-bars)
			  (memq x-toolkit-scroll-bars '(xaw xaw3d)))
		     (format ", %s scroll bars"
			     (capitalize (symbol-name x-toolkit-scroll-bars)))
		   "")
		 (format-time-string "%Y-%m-%d" emacs-build-time)
		 emacs-build-system 
		 ergoemacs-version)))
    (if here
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))

;; About text when logo can be displayed
(setq fancy-about-text
  '((:face (variable-pitch (:foreground "red"))
     "This is "
     :link ("ErgoEmacs"
	    (lambda (button) (browse-url ergoemacs-url))
	    "ErgoEmacs home page")
     " based on "
     :link ("GNU Emacs"
	    (lambda (button) (browse-url "http://www.gnu.org/software/emacs/"))
	    "GNU Emacs home page")
     ".\n\n"
     (lambda () (emacs-version))
     "\n\n"
     :face variable-pitch
     :link ("ErgoEmacs Authors"
	    (lambda (button) (browse-url ergoemacs-url-authors)))
     "\tErgoEmacs contributors\n"
     :link ("GNU/Emacs Authors"
	    (lambda (button)
	      (view-file (expand-file-name "AUTHORS" data-directory))
	      (goto-char (point-min))))
     "\tMany people have contributed code included in GNU Emacs\n"
     :link ("Contributing"
	    (lambda (button) (browse-url ergoemacs-url-contrib)))
     "\tHow to contribute improvements to ErgoEmacs\n"
     "\n"
     :link ("Absence of Warranty" (lambda (button) (describe-no-warranty)))
     "\tErgoEmacs comes with "
     :face (variable-pitch (:slant oblique))
     "ABSOLUTELY NO WARRANTY\n"
     :face variable-pitch
     "\n"
     )))

;; About text in non-graphics screen
(defun normal-about-screen ()
  (insert "\n" (emacs-version) "\n\n")

  (insert "To follow a link, click Mouse-1 on it, or move to it and type RET.\n\n")

  (insert-button "ErgoEmacs Authors"
		 'action
		 (lambda (button) (browse-url ergoemacs-url-authors))
		 'follow-link t)
  (insert "\tErgoEmacs contributors\n")

  (insert-button "GNU/Emacs Authors"
		 'action
		 (lambda (button)
		   (view-file (expand-file-name "AUTHORS" data-directory))
		   (goto-char (point-min)))
		 'follow-link t)
  (insert "\tMany people have contributed code included in GNU Emacs\n")

  (insert-button "Contributing"
		 'action
		 (lambda (button) (browse-url ergoemacs-url-contrib))
		 'follow-link t)
  (insert "\t\tHow to contribute improvements to ErgoEmacs\n\n")

  (insert-button "Absence of Warranty"
		 'action (lambda (button) (describe-no-warranty))
		 'follow-link t)
  (insert "\tErgoEmacs comes with ABSOLUTELY NO WARRANTY\n")
  )
