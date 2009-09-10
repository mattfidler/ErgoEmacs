;;; pov-mode.el --- major mode for Povray scene files

;; Copyright (C) 1997 Peter W. Boettcher

;;
;; Author: Peter Boettcher <pwb@andrew.cmu.edu>
;; Maintainer: Marco Pessotto <marco.erika@gmail.com>
;; Created: 04 March 1994
;; Modified: 26 May 2008
;; Version: 3.2
;; Keywords: pov, povray
;;

;; This file is not (yet) part of GNU Emacs


;; LCD Archive Entry:
;; povray|Peter Toneby|woormie@acc.umu.se|
;; Major mode for Povray scene files|
;; 08-Sep-2003|2.10|~/lib/emacs/pov-mode.el|
;;
;;
;;
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; This major mode for GNU Emacs provides support for editing Povray
;; scene files, rendering and viewing them.  It automatically indents
;; blocks, both {} and #if #end.  It also provides context-sensitive
;; keyword completion and font-lock highlighting, as well as the
;; ability to look up those keywords in the povray docu.
;;
;;
;; INSTALLATION
;;
;; Add the following code to your emacs init file.
;;
;; (add-to-list 'load-path "~/john/pov-mode-3.x")
;; (autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
;; (add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))
;;
;; The "~/john/pov-mode-3.x" should be the path to the file pov-mode.el .
;;
;; Once installed, you may need to set pov-include-dir and
;; pov-documentation-directory. You can set these by using M-x
;; set-variable or M-x customize-group RET pov RET.
;;
;; Byte compile the pov-mode.el to make it load faster. 
;; Type M-x byte-compile-file.
;;
;; To read pov-mode documentation, type M-x pov-mode then C-h m.
;; To access the pov-mode info file type C-u C-h i RET. This will
;; prompt you for  a file: give the pov-mode.info file that you can
;; find in the pov-mode.el directory. Or install somewhere in your
;; INFOPATH and run install-info pov-mode.info dir. 
;;
;; Download and install somewhere the InsertMenu directory, if you
;; want this nice feature. I'd recommend you to unpack it in the same
;; directory of pov-mode.el and check via M-x customize-group that the
;; variable pov-insertmenu-location has the correct value. It is
;; possible that <http://www.imagico.de/> has a fresher version of
;; this package.
;; 
;;; Change Log:
;;
;;Modified by: Peter Boettcher <pwb@andrew.cmu.edu>
;;  5/8/97:
;;    Added font-lock support for Emacs/XEmacs 19
;;    Indent under `#declare Object=' lines
;;    Corrected comment syntax
;;    Got rid of more remnants from postscript mode
;;    General cleanup
;;    Arbitrarily chose version 1.2
;; 5/8/97:  Version 1.21
;;    fontify-insanely was ignored.  fixed.
;;
;; 9/24/97: Version 1.3
;;    Added indentation for Pov 3 syntax (#if #else, etc)
;;    Preliminary context-sensitive keyword completion
;;
;; 1/13/98 by Peter Boettcher <pwb@andrew.cmu.edu>
;;    Explicitly placed package under GPL
;;    Reorganized comment sections and change log to follow GNU standards
;;    Added simple code for jumping to pov documentation (Thanks to
;;         Benjamin Strautin <bis@acpub.duke.edu> for this code)
;;
;; Modified by: Peter Toneby <woormie@acc.umu.se>
;;  22/3/99: Version 1.99beata1
;;    Added support for Pov3.1s new keywords. (not all, I think...)
;;    Removed atmosphere (and atmosphere_*) (stupid me...)
;;
;; Modified by: Peter Toneby <woormie@acc.umu.se>
;;  23/4/99: Version 1.99beata2
;;    Added support for all new keyword, BUT
;;    Added atmosphere (and atmosphere_*) again
;;    Got Pete Boettchers blessing to continue (but with a note
;;      that said that I should have talked to him first, I'm sorry
;;      for not doing that). Pete also said he was willing to let
;;      me continue the maintainance of this file.
;;    I can't get the pov-keyword-help to work, anyone with knowledge
;;      about elisp can send me a fix for it.
;;    The keyword expansion doesn't work for all keywords,
;;      I need to add lots of stuff and read through the docs
;;      to get everything correct.
;;
;; Modified by: Alexander Schmolck <aschmolck@gmx.de>
;; 2000-01-31: Version 2beataXXX
;;    Added working keyword lookup in povuser.txt
;;    Added rendering and viewing from within Emacs and with an external viewer
;;    Added customization and made installation simpler
;;    Added a few other minor details
;;
;; Modified by: Peter Toneby <woormie@acc.umu.se>
;; 2000-05-24: Version 2
;;    Changed the keyword lookup a little, povuser.txt didn't open as
;;       expected when having set the pov-home-dir and pov-help-file
;;       manually.
;;
;; Modified by: Peter Toneby <woormie@acc.umu.se>
;; 2000-08-10: Version 2.5b1
;;    Added povray-font-lock-faces.
;;    Made sure font-lock works properly on:
;;        XEmacs 19.15p7
;;        XEmacs 20.0
;;        XEmacs 21.1p10
;;        Emacs 19.29.1
;;        Emacs 20.7.2
;;    Added all 3.1 keywords except track, since I don't know what it
;;        is, I have also dropped the 3.0 specific keywords that
;;        shouldn't be used anymore.
;;    Fixed some completion stuff, I think I have added all keywords to
;;        the completions.
;;    Added configureation for all faces. To bad I can't get the defaults
;;        to work properly on dark backgrounds, I don't know why that is.
;;    Added a toolbar, it replaces to standard XEmacs toolbar, I think
;;        that is the best thing to do, but I retain the standard useful
;;        functionality.
;;    Fixed an error in the external viewer, it used variables that were
;;        not available in the same scope as the sentinel.
;;    Added basic imenu support, currently only #local and #declare,
;;        but I will try to add objects, cameras and lightsources later.
;; 2000-09-12 Version 2.5.b2
;;    Added basic support for megapov
;;    Bob Pepin fixed a bug in test to select external/internal viewer.
;;    fortsätt på kapitel 5.6
;; 2001-04-05 Version 2.6
;;    Added the capability to open standard include files by pressing
;;        C-c i. It opens the file entered ro.
;;    Fixed leeking color in emacs (Robert Kleemann)
;;    Changed the rendertoolbarbutton to show a popup dialog with buttons
;;        for the different qualities.
;; 2001-12-07 Version 2.7
;;    Fixed font-locks for Emacs 21
;; 2002-06-19 Version 2.8
;;    Fixed loading for Emacs 21-2, missed a test for customizations
;;    Added most parts of a patch from  Christoph Hormann that cleaned
;;        up the Regexp mess, and added all (or at least most) of the
;;        keywords for 3.5 Parts of that patch are still missing, I'll
;;        get around to those sometime...
;; 2002-08-07 Version 2.9
;;    Added Insert menu, it uses the directory structure of the winpov
;;        insert menu.
;;    Moved the toolbar icons out to separate xpm-files.
;;    Added some missing keyword expansions
;;    Fixed so that box, cylinder... has their own face, object-face,
;;        removed unused faces
;;    Cleaned up the code, removed unused stuff.
;;    It works in versions 21, can't test in 20, font-lock is borked for me.
;; 2002-08-10 Version 2.9.1
;;    Fixed so that tollbar icons are searched for, not hard coded.
;; 2003-08-29 Version 2.10
;;    Fixed references to povray.el.
;;    Fixed cut-n-pasted comment for font-pov-operator-face.
;;    Fixed the insert menus, they missed the last items, thanks to Hartwig
;;        Bosse for the heads up, and a fix.
;; 2003-08-29 Version 2.11 (This is a future version)
;;    Fix a bug with rendering so that the active buffer is changed to the
;;        correct buffer, found by Hartwig Bosse.
;;
;; Modified by: Marco Pessotto <marco.erika@gmail.com>
;; 2008-01-05
;;    Workaround for Emacs 22 (on which pov-mode was not working)
;; 2008-02-10
;;    Really fix the defcustom menu for faces
;;    Upgraded to GPL 3
;;    Provides a setup.sh script that fixes some vars
;; 2008-02-11
;;    Added internal view for GNU Emacs 22
;; 2008-02-13
;;    Fixed faces to be consistent with other languages
;;     (as much as possible). If you don't like, customize it via
;;     M-x customize-group pov. Beware! It has not been tested on Xemacs 
;;     nor earlier versions of GNU emacs
;; 2008-02-16 Version 2.14 
;;    Rewrited from scratch the pov-keyword-help function
;;     and commented out the old one, because the povuser.txt no more
;;     exists. Now the pov-mode.el open an external browser to find
;;     the documentation.  Try this and let me know! Browser can be
;;     choosen in the M-x customize-group pov menu. However, don't
;;     modify the name of directory and file names if this feature
;;     works. You'll mess up things. This has been tested on the
;;     documentation for pov-ray 3.6 (official binaries for linux).
;;     Your documentation (for Mac or for previous or later version
;;     of PoV-ray) may be somewhere else. Please let me know.
;; 2008-02-17 Version 2.15
;;    Created the tool-bar for GNU Emacs 22.1 (and maybe for previous 
;;     releases. If you are running GNU Emacs < 22 and the toolbar 
;;     works (with 2 icons, for view and for rendering, let me know)
;; 2008-02-17 Version 2.16
;;    Fixed a portability bug. Uses browse-url instead of 
;;     browse-url-generic. Use the customize-group browse-url to choose
;;     the browser.
;;     Thanks to Xah Lee for point this out.
;;    Cleaned code for compilation. However something still remains for 
;;     suspect compatibily with Xemacs. Please let me if Xemacs works 
;;     with this package.
;;    Cleaned code and dropped prehistorical releases of (X)emacs that 
;;     don't support customization. The old package still exists. If 
;;     you stuck with older versions, use the older version of this 
;;     package too.   ;-)   
;; 2008-02-20 Version 2.17
;;    Added a menu for rendering and one for the preview. I didn't use 
;;     the lisp libraries, just modified the keymap. Needs testing with 
;;     xemacs.
;;    Upgraded to POV-Ray 3.6
;; 2008-02-23 Version 2.18 
;;    Cleaned the InsertMenu directory and added a pair of templates.
;;    If you like this mode, please *donate* a spare scene to be inserted
;;     in the InserMenu. 
;;    Fixed the imenu critical bug that messed up the menubar.
;;     Not tested with xemacs
;;    I consider this release quite stable. If you find a bug feel 
;;     free to contact me. I'll try to fix it.
;; 2008-02-24 Version 2.19
;;    Fixed a minor bug that prevented the viewer to get the file name.
;;     This happens when the path directory is too long and the output of
;;     povray wrap words. I didn't discard the old code. If the buffer 
;;     search fails, it uses the basename of current buffer appending 
;;     the default extension (png for unix, bmp for windoze).
;;    Added a lot of keybindings for rendering and viewers.
;; 2008-03-01 Version 2.20
;;    Fixed a bug in the killing of processes. Now it use
;;     delete-process, no more kill-process, that just send a kill -9 and
;;     doesn't run the sentinel.
;;    Got rid of the shell script setup.sh. Now pov-mode can find the
;;     insert-menu without problems. This remains available for user's
;;     customization. The path to pov-ray must be set by the user. However
;;     it uses the default installation path of the official povray release.
;;    Rebound the keystrokes, so they are more comfortable and consistent.
;;    Added a pair of missing keywords.
;;    Added some documentation in the commentary
;; 2008-03-09 Version 3.0
;;    Dropped xemacs compatibility (I can't maintain this). Cleaned code.
;;    The InsertMenu becomes just an add-on. Added the code to turn on and 
;;     off the support to this feature.
;;    Added the control to the Errors with exit code 0 (!)
;;    Added the var "pov-isosurface-keyword" that was declared but void
;; 2008-03-19 
;;    Added documentation (thanks to Xah Lee)
;;    Solved the InsertMenu question with the POV-Ray team. Thanks to
;;     Christoph Hormann <http://www.imagico.de>, one of the authors of
;;     the InsertMenu. This package is covered by the POV-Ray license.
;;    There is a commented function pov-online-search for keyword
;;     lookup in the on-line documentation, but still needs a lot of
;;     development. 
;; 2008-04-18 Version 3.1 just a minor change
;;     Resized the icons to suite the 24x24 pixel standard
;;     Added a new Misc menu for comment, uncomment and doc lookup
;;     Fixed the pov-open-include-file function to prompt to existing files
;; 2008-04-28 
;;     Added the autoloads for elpa and package.el
;;     Fixed a minor #macro indenting bug
;; 2008-05-24 Version 3.2 
;;     Written the texinfo manual. No code improvement (sorry)
;; 2008-05-26 
;;     Minor changes to the info manual. Thanks to Xah Lee.
;;     
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Original Author:     Kevin O. Grover <grover@isri.unlv.edu>
;;        Cre Date:     04 March 1994
;; This file derived from postscript mode by Chris Maio
;;
;;  Please send bug reports/comments/suggestions and of course patches to 
;;           Marco Pessotto
;;        <marco.erika@gmail.com>
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TODO
;;
;;    Fix the standard include file command
;;    Write the texinfo documentation
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

;; Better safe than sorry, lets fail if you are using a (very?) old
;; version of (X)Emacs.



(if (if (save-match-data (string-match "Lucid\\|XEmacs" (emacs-version)))
	(and (= emacs-major-version 19) (< emacs-minor-version 14))
      (and (= emacs-major-version 19) (< emacs-minor-version 29)))
    (error "`pov-mode' was written for Emacs 19.29/XEmacs 19.14 or later"))

(defvar font-pov-is-XEmacs19
  (and (not (null (save-match-data
                    (string-match "XEmacs\\|Lucid" emacs-version))))
       (= 19 emacs-major-version)))
(defvar font-pov-is-XEmacs20
  (and (not (null (save-match-data
                    (string-match "XEmacs\\|Lucid" emacs-version))))
       (<= 20 emacs-major-version)))
(defvar font-pov-is-XEmacs21
  (and (not (null (save-match-data
                    (string-match "XEmacs\\|Lucid" emacs-version))))
       (<= 21 emacs-major-version)))

(defvar font-pov-is-XEmacs20-2
  (or (and font-pov-is-XEmacs20 (<= 2 emacs-minor-version))
      font-pov-is-XEmacs21))

(defvar font-pov-is-Emacs19
  (and (not font-pov-is-XEmacs19)
       (not font-pov-is-XEmacs20)
       (= 19 emacs-major-version)))
(defvar font-pov-is-Emacs20
  (and (not font-pov-is-XEmacs19)
       (not font-pov-is-XEmacs20)
       (= 20 emacs-major-version)))
(defvar font-pov-is-Emacs21
  (and (not font-pov-is-XEmacs19)
       (not font-pov-is-XEmacs20)
       (not font-pov-is-XEmacs21)
       (= 21 emacs-major-version)))
(defvar font-pov-is-Emacs22
  (and (not font-pov-is-XEmacs19)
       (not font-pov-is-XEmacs20)
       (not font-pov-is-XEmacs21)
       (<= 22 emacs-major-version))) ; this time let emacs grow

(defvar font-pov-is-Emacs
  (or font-pov-is-Emacs19 
      font-pov-is-Emacs20 
      font-pov-is-Emacs21 
      font-pov-is-Emacs22))
;;; font-pov-is-Emacs returns t, font-pov-is-Emacs22 t, anything else nil

(unless	(or font-pov-is-Emacs20
	    font-pov-is-Emacs21 font-pov-is-Emacs22)
  (error "PoV-mode requires GNU Emacs >= 20"))
    

;; initialize the variables to avoid compilation warnings
(defvar povray-command)
(defvar pov-run-default)
(defvar pov-run-test)
(defvar pov-run-low)
(defvar pov-run-mid)
(defvar pov-run-high)
(defvar pov-run-highest)
(defvar pov-external-viewer-command)
(defvar pov-external-view-options)
(defvar pov-icons-location)
(defvar default-toolbar)
(defvar pov-toolbar)
(defvar pov-turn-on-font-lock)
(defvar pov-associate-pov-and-inc-with-pov-mode-flag)
(defvar font-lock-face-attributes)
(defvar font-lock-display-type)
(defvar font-lock-face-attributes)
(defvar font-lock-background-mode)
(defvar pov-imenu-in-menu)
(defvar pov-im-menu)
(defvar pov-indent-level)
(defvar pov-indent-under-declare)
(defvar pov-completion-list)
(defvar pov-density-keywords)
(defvar pov-isosurface-keywords)
(defvar pov-bezier-keywords)
(defvar pov-photons-keywords)
(defvar pov-completion-list)
(defvar pov-default-view-internal)
(defvar pov-image-file)
(defvar pov-default-image-extension)
(defvar rendericon)
(defvar viewicon)
(defvar pov-documentation-directory)
(defvar pov-documentation-keyword-index)
(defvar pov-documentation-index)
(defvar pov-include-dir)
(defvar pov-default-view-internal)
(defvar pov-internal-view)
(defvar pov-external-view)
(defvar pov-command-alist)
(defvar pov-imenu-only-macros)
(defvar pov-render-dialog-desc)
(defvar pov-insertmenu-location)
(defvar pov-errors)
;; end initialisation


(require 'cl)
(require 'imenu)
(require 'font-lock) 
(require 'browse-url)
(require 'newcomment)

(defconst pov-mode-version '3.2   ;; this is the only occurence
  "The povray mode version.")

(defvar pov-tab-width)
(defvar pov-autoindent-endblocks t)


(when (> 1000 max-lisp-eval-depth)
  (customize-set-value 'max-lisp-eval-depth 1000))


(defgroup  pov nil
  "*Major mode for editing povray 3.X scene files <http://www.povray.org>."
  :group 'languages)


(defcustom pov-include-dir "/usr/local/share/povray-3.6/include"
  "*The directory in which the povray includefiles reside."
  :type 'directory
  :group 'pov)

(defcustom  pov-documentation-directory "/usr/local/share/doc/povray-3.6/html"
  "*The directory that contains the html documentation of povray"
  :type 'directory
  :group 'pov
  )

(defcustom pov-insertmenu-location 
  (file-name-as-directory 
   (concat 
    (file-name-directory 
     (locate-library "pov-mode")) "InsertMenu"))
  "*Path to the InsertMenu directory. You need a valid location for 
this feature"
  :type 'directory
  :group 'pov
  )

(defcustom povray-command "povray"
  "*Command used to invoke the povray."
  :type 'string
  :group 'pov)

(defcustom pov-external-viewer-command 
  (cond
   ((memq system-type '(windows-nt ms-dos cygwin darwin))
    "open")
   (t "display")) ;; MP
  ;; It seems that ImageMagick is quite popular
  "*The external viewer to call."
  :type 'string
  :group 'pov)

(defcustom pov-external-view-options "%s"
  "*The options for the viewer; %s is replaced with the name of the rendered image."
  :type 'string
  :group 'pov)

(defcustom  pov-default-image-extension 
  (cond
   ((memq system-type '(windows-nt ms-dos cygwin))
    "bmp")
   (t "png")) ;; MP
  "*The default extension of the output image. Windows user should 
set it to bmp It you are going to customize this options, you must
customize the options of the povray commands also. So beware!"
  :type 'string
  :group 'pov)

(defcustom pov-default-view-internal t 
  "*Should the pictures be displayed internally by default?"
  :type 'boolean
  :group 'pov
  )


(defcustom pov-run-default "+i%s"
  "*The default options for the Render command (%s is replaced
by the filename)."
  :type 'string
  :group 'pov
  )
(defcustom pov-run-test "res120 -Q3 +i%s"
  "*The default options for the Test Render command (%s is replaced by the filename)."
  :type 'string
  :group 'pov
  )
(defcustom pov-run-low "res320 +i%s"
  "*The default options for the Test Render command (%s is replaced by the filename)."
  :type 'string
  :group 'pov
  )
(defcustom pov-run-mid "res640 +i%s"
  "*The default options for the Medium Res Render command (%s is replaced by the filename)."
  :type 'string
  :group 'pov
  )
(defcustom pov-run-high "res800 +i%s"
  "*The default options for the High Res Render command (%s is replaced by the filename)."
  :type 'string
  :group 'pov
  )
(defcustom pov-run-highest "res1k +i%s"
  "*The default options for the Higest Res Render command (%s is replaced by the filename)."
  :type 'string
  :group 'pov
  )

(defcustom pov-icons-location (file-name-directory 
			       (locate-library "pov-mode"))
  "*Location of the menubaricons. Change only if you want 
to try your own icons"
  :type 'directory
  :group 'pov
  )

      


(defvar pov-external-view
  "External view")
(defvar pov-internal-view
  "Internal view")
;; I think that this alist messes up things. Customize-group pov, and
;; set to ``only current session'' doesn't work, because this alist
;; won't be refreshed. So what? Should I rewrite all?
(defvar pov-command-alist (list (list "Render"
				      povray-command pov-run-default
				      '()) ;history for the command
				(list "Test quality render"
				      povray-command pov-run-test
				      '())
				(list "Low quality render"
				      povray-command pov-run-low
				      '())
				(list "Medium quality render"
				      povray-command pov-run-mid
				      '())
				(list "High quality render"
				      povray-command pov-run-high
				      '())
				(list "Highest quality render"
				      povray-command pov-run-highest
				      '())
				(list pov-external-view
				      pov-external-viewer-command
				      pov-external-view-options
				      '())
				(list pov-internal-view
				      (list pov-internal-view)
				      '()))
  "the commands to run")

      
(defcustom pov-documentation-index "idx.html"
  "*Edit this only if the search-keyword function doesn't work!
This file should contain the general index for *all* the documentation"
  :type 'file
  :group 'pov)
      
(defcustom pov-documentation-keyword-index "s_97.html"
  "*Edit this only if the search-keyword function doesn't work!
This file (tested on povlinux-3.6) should contain the index for the keywords (section 3)"
  :type 'file
  :group 'pov)

(defcustom pov-associate-pov-and-inc-with-pov-mode-flag t
  "*If t then files ending with .pov and .inc will automatically start
pov-mode when loaded, unless those file-endings are already in use."
  :type 'boolean
  :group 'pov)

(defcustom pov-fontify-insanely t
  "*Non-nil means colorize every povray keyword.  
This may take a while on large files.  Maybe disable this on slow systems."
  :type 'boolean
  :group 'pov)

(defcustom pov-imenu-in-menu t 
  "*Non-nil means have #locals, #declares, #macros and something else
in a menu called PoV in the menubar. This permits to jump to the point of the 
buffer where this # has been declared."
  :type 'boolean
  :group 'pov)
;; CH
(defcustom pov-imenu-only-macros nil
  "*Non-nil means to restrict imenu to macro declarations."
  :type 'boolean
  :group 'pov)
;; /end CH
(defcustom pov-indent-level 2
  "*Indentation to be used inside of PoVray blocks or arrays."
  :type 'integer
  :group 'pov)

(defcustom pov-autoindent-endblocks t
  "*When non-nil, automatically reindents when you type break, 
end, or else."
  :type 'boolean
  :group 'pov
  )

(defcustom pov-indent-under-declare 2
  "*Indentation under a `#declare Object=' line."
  :type 'integer
  :group 'pov)

(defcustom pov-tab-width 8
  "*Tab stop width for PoV mode."
  :type 'integer
  :group 'pov)

(defcustom pov-turn-on-font-lock t
  "*Turn on syntax highlighting automatically"
  :type 'boolean
  :group 'pov)
      


(defcustom font-pov-csg-face 'font-lock-function-name-face
  "*What color does CSG-object have. Also try
font-pov-csg-face"
  :type 'face
  :group 'pov)

(defcustom font-pov-object-face 'font-lock-type-face
  "*What color does objects have. Also try 
font-pov-object-face"
  :type 'face
  :group 'pov)

(defcustom font-pov-variable-face 'font-lock-variable-name-face 
  "*What color does variables (in declarations) have. Also try
font-pov-variable-face"
  :type 'face
  :group 'pov)
      
(defcustom font-pov-macro-name-face 'default
  "*Face to use for #macro names. Just black and white. Try also
font-pov-macro-name-face"
  :type 'face
  :group 'pov
  )

(defcustom font-pov-operator-face  'default 
  "*Face to use for PoV operators. Just black and white. You should try
font-pov-operator-face or font-lock-builtin-face to get the most of the 
syntax coloring "
  :type 'face
  :group 'pov)


(defcustom font-pov-directive-face 'font-lock-preprocessor-face 
  "*What color does (#)-directives have. Also try 
font-pov-directive-face"
  :type 'face
  :group 'pov)

(defcustom font-pov-number-face 'default
  "*What color does numbers have. Just black and white. Try 
font-pov-number-face or font-lock-constant-face to view colored number"
  :type 'face
  :group 'pov)

(defcustom font-pov-keyword-face 'font-lock-keyword-face
  "*What color does keywords have. Also try 
font-pov-keyword-face"
  :type 'face
  :group 'pov)
;;      ))
	



;; Abbrev support
(defvar pov-mode-abbrev-table nil
  "Abbrev table in use in pov-mode buffers.")
(define-abbrev-table 'pov-mode-abbrev-table ())

;; (cond ((or font-pov-is-XEmacs20-2 font-pov-is-Emacs20)
;;        (when pov-turn-on-font-lock
;; 	 (turn-on-font-lock))
       ;; associate *.pov and *.inc with pov if flag is set and no other
       ;; modes already have
;;;###autoload (add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))

(cond (pov-associate-pov-and-inc-with-pov-mode-flag 
       (when (not (assoc "\\.pov\\'" auto-mode-alist))
	 (add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode)))
       (when (not (assoc "\\.inc\\'" auto-mode-alist))
	 (add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode)))))
;;END AS

(defvar font-pov-do-multi-line t
  "*Set this to nil to disable the multi-line fontification prone to infinite loop bugs.")

(defun font-pov-setup ()  
  "Setup this buffer for PoV font-lock."
  (if (or font-pov-is-Emacs20 font-pov-is-Emacs21 font-pov-is-Emacs22)
    ;; Tell Font Lock about the support.
    (make-local-variable 'font-lock-defaults)))



(cond
 ((or font-pov-is-Emacs20 font-pov-is-XEmacs20-2 
      font-pov-is-Emacs21 font-pov-is-Emacs22)
  (defface font-pov-object-face
    '((((class grayscale) (background light)) (:foreground "DimGray" :bold t))
      (((class grayscale) (background dark))  (:foreground "LightGray" :bold t))
      (((class color) (background light))    (:foreground "DarkOliveGreen" :bold t))
      (((class color) (background dark))     (:foreground "White" :bold t ))
      (t (:bold t)))
    "Font Lock mode face used for objects."
    :group 'font-pov-faces)
  
  (defface font-pov-directive-face
    '((((class grayscale) (background light)) (:foreground "DimGray"))
      (((class grayscale) (background dark))  (:foreground "LightGray"))
      (((class color) (background light))     (:foreground "DarkRed"))
      (((class color) (background dark))      (:foreground "lightgreen"))
      (t (:italic t)))
    "Font Lock mode face used to highlight PoV directives."
    :group 'font-pov-faces)
  
  (defface font-pov-number-face
    '((((class grayscale) (background light))(:foreground "DimGray" :underline t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :underline t))
      (((class color) (background light))    (:foreground "SaddleBrown"))
      (((class color) (background dark))     (:foreground "wheat"))
      (t (:underline t)))
    "Font Lock mode face used to highlight numbers in PoV."
    :group 'font-pov-faces)
  
  (defface font-pov-variable-face
    '((((class grayscale) (background light)) (:foreground "DimGray"))
      (((class grayscale) (background dark))  (:foreground "LightGray"))
      (((class color) (background light))     (:foreground "ForestGreen"))
      (((class color) (background dark))      (:foreground "gray80"))
      )
    "Font Lock mode face used to highlight variabledeclarations in PoV."
    :group 'font-pov-faces)
  
  (defface font-pov-csg-face
    '((((class grayscale) (background light)) (:foreground "DimGray"))
      (((class grayscale) (background dark))  (:foreground "LightGray"))
      (((class color) (background light))     (:foreground "Blue"))
      (((class color) (background dark))      (:foreground "red"))
      )
    "Font Lock mode face used to highlight CSGs in PoV."
    :group 'font-pov-faces)
  
;; -- C.H. --

  (defface font-pov-macro-name-face
    '((((class grayscale) (background light)) (:foreground "DimGray" :bold t))
      (((class grayscale) (background dark))  (:foreground "LightGray" :bold t))
      (((class color) (background light))     (:foreground "Blue2" :bold t))
      (((class color) (background dark))      (:foreground "gray80" :bold t))
      )
    "Font Lock mode face used to highlight macro declarations in PoV."
    :group 'font-pov-faces)

  (defface font-pov-keyword-face
    '((((class grayscale) (background light)) (:foreground "DimGray"))
      (((class grayscale) (background dark))  (:foreground "LightGray"))
      (((class color) (background light))     (:foreground "Blue4"))
      (((class color) (background dark))      (:foreground "Blue"))
      )
    "Font Lock mode face used to highlight general keywords in PoV."
    :group 'font-pov-faces)

;; -- end C.H. --


  (defface font-pov-operator-face
    '((((class grayscale)(background light)) (:foreground "DimGray" :bold t))
      (((class grayscale)(background dark))  (:foreground "LightGray" :bold t))
      (((class color)(background light))     (:foreground "Limegreen" :bold t ))
      (((class color)(background dark))      (:foreground "Limegreen" :bold t ))
      (t (:bold t)))
    "Font Lock mode face used to highlight operators in PoV."
    :group 'font-pov-faces)))



(font-pov-setup) ;; Setup and register the fonts...

(defun pov-make-tabs (stop)
  (and (< stop 132) (cons stop (pov-make-tabs (+ stop pov-tab-width)))))

(defconst pov-tab-stop-list (pov-make-tabs pov-tab-width)
  "Tab stop list for PoV mode")

(defvar pov-mode-map nil
  "Keymap used in PoV mode buffers")

(defvar pov-mode-syntax-table nil
  "PoV mode syntax table")

(defconst pov-comment-start-regexp "//\\|/\\*"
  "Dual comment value for `comment-start-regexp'.")

(defvar pov-comment-syntax-string ". 124b"
  "PoV hack to handle Emacs/XEmacs foo")

(defvar pov-begin-re 
  "\\<#\\(if\\(n?def\\)?\\|case\\|macro\\|range\\|switch\\|while\\)\\>")

(defvar pov-end-re  "\\<#break\\|#end\\>")

(defvar pov-else-re "\\<#else\\>")

(defvar pov-begin-end-re (concat
			  pov-begin-re
			  "\\|"
			  pov-end-re
			  "\\|"
			  pov-else-re))

(defun pov-setup-syntax-table nil
;;   (if (or (string-match "Lucid" emacs-version)
;; 	  (string-match "XEmacs" emacs-version))
;;       (setq pov-comment-syntax-string ". 1456"))
  (if pov-mode-syntax-table
      ()
    (setq pov-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?_ "w" pov-mode-syntax-table)
    (modify-syntax-entry ?# "w" pov-mode-syntax-table)
    (modify-syntax-entry ?/ pov-comment-syntax-string pov-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" pov-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" pov-mode-syntax-table)
    (set-syntax-table pov-mode-syntax-table)))

;; -- C.H. --

(defvar pov-all-keyword-matcher
  (eval-when-compile
    (concat "\\<\\("
	    (regexp-opt '("aa_level" "aa_threshold" "abs" "absorption"
			  "accuracy" "acos" "acosh" "adaptive"
			  "adc_bailout" "agate" "agate_turb" "all"
			  "all_intersections" "alpha" "altitude"
			  "always_sample" "ambient" "ambient_light"
			  "angle" "aperture" "append" "arc_angle"
			  "area_light" "array" "asc" "ascii" "asin"
			  "asinh" "assumed_gamma" "atan" "atan2"
			  "atanh" "autostop" "average" "b_spline"
			  "background" "bezier_spline" "bicubic_patch"
			  "black_hole" "blob" "blue" "blur_samples"
			  "bounded_by" "box" "boxed" "bozo" "break"
			  "brick" "brick_size" "brightness"
			  "brilliance" "bump_map" "bump_size" "bumps"
			  "camera" "case" "caustics" "ceil" "cells"
			  "charset" "checker" "chr" "circular"
			  "clipped_by" "clock" "clock_delta"
			  "clock_on" "collect" "color" "color_map"
			  "colour" "colour_map" "component"
			  "composite" "concat" "cone" "confidence"
			  "conic_sweep" "conserve_energy"
			  "contained_by" "control0" "control1"
			  "coords" "cos" "cosh" "count" "crackle"
			  "crand" "cube" "cubic" "cubic_spline"
			  "cubic_wave" "cutaway_textures" "cylinder"
			  "cylindrical" "debug" "declare" "default"
			  "defined" "degrees" "density" "density_file"
			  "density_map" "dents" "df3" "difference"
			  "diffuse" "dimension_size" "dimensions"
			  "direction" "disc" "dispersion"
			  "dispersion_samples" "dist_exp" "distance"
			  "div" "double_illuminate" "eccentricity"
			  "else" "emission" "end" "error"
			  "error_bound" "evaluate" "exp"
			  "expand_thresholds" "exponent" "exterior"
			  "extinction" "face_indices" "facets"
			  "fade_color" "fade_colour" "fade_distance"
			  "fade_power" "falloff" "falloff_angle"
			  "false" "fclose" "file_exists" "filter"
			  "final_clock" "final_frame" "finish"
			  "fisheye" "flatness" "flip" "floor"
			  "focal_point" "fog" "fog_alt" "fog_offset"
			  "fog_type" "fopen" "form" "frame_number"
			  "frequency" "fresnel" "function" "gather"
			  "gif" "global_lights" "global_settings"
			  "gradient" "granite" "gray" "gray_threshold"
			  "green" "height_field" "hexagon"
			  "hf_gray_16" "hierarchy" "hypercomplex"
			  "hollow" "if" "ifdef" "iff" "ifndef"
			  "image_height" "image_map" "image_pattern"
			  "image_width" "include" "initial_clock"
			  "initial_frame" "inside" "inside_vector"
			  "int" "interior" "interior_texture"
			  "internal" "interpolate" "intersection"
			  "intervals" "inverse" "ior" "irid"
			  "irid_wavelength" "isosurface" "jitter"
			  "jpeg" "julia" "julia_fractal" "lambda"
			  "lathe" "leopard" "light_group"
			  "light_source" "linear_spline"
			  "linear_sweep" "ln" "load_file" "local"
			  "location" "log" "look_at" "looks_like"
			  "low_error_factor" "macro" "magnet"
			  "major_radius" "mandel" "map_type" "marble"
			  "material" "material_map" "matrix" "max"
			  "max_extent" "max_gradient"
			  "max_intersections" "max_iteration"
			  "max_sample" "max_trace" "max_trace_level"
			  "media" "media_attenuation"
			  "media_interaction" "merge" "mesh" "mesh2"
			  "metallic" "method" "metric" "min"
			  "min_extent" "minimum_reuse" "mod" "mortar"
			  "natural_spline" "nearest_count" "no"
			  "no_bump_scale" "no_image" "no_reflection"
			  "no_shadow" "noise_generator" "normal"
			  "normal_indices" "normal_map"
			  "normal_vectors" "number_of_waves" "object"
			  "octaves" "off" "offset" "omega" "omnimax"
			  "on" "once" "onion" "open" "orient"
			  "orientation" "orthographic" "panoramic"
			  "parallel" "parametric" "pass_through"
			  "pattern" "perspective" "pgm" "phase"
			  "phong" "phong_size" "photons" "pi"
			  "pigment" "pigment_map" "pigment_pattern"
			  "planar" "plane" "png" "point_at" "poly"
			  "poly_wave" "polygon" "pot" "pow" "ppm"
			  "precision" "precompute" "pretrace_end"
			  "pretrace_start" "prism" "prod"
			  "projected_through" "pwr" "quadratic_spline"
			  "quadric" "quartic" "quaternion"
			  "quick_color" "quick_colour" "quilted"
			  "radial" "radians" "radiosity" "radius"
			  "rainbow" "ramp_wave" "rand" "range"
			  "range_divider" "ratio" "read" "reciprocal"
			  "recursion_limit" "red" "reflection"
			  "reflection_exponent" "refraction" "render"
			  "repeat" "rgb" "rgbf" "rgbft" "rgbt" "right"
			  "ripples" "rotate" "roughness" "samples"
			  "save_file" "scale" "scallop_wave"
			  "scattering" "seed" "select" "shadowless"
			  "sin" "sine_wave" "sinh" "size" "sky"
			  "sky_sphere" "slice" "slope" "slope_map"
			  "smooth" "smooth_triangle" "solid" "sor"
			  "spacing" "specular" "sphere" "sphere_sweep"
			  "spherical" "spiral1" "spiral2" "spline"
			  "split_union" "spotlight" "spotted" "sqr"
			  "sqrt" "statistics" "str" "strcmp"
			  "strength" "strlen" "strlwr" "strupr"
			  "sturm" "substr" "sum" "superellipsoid"
			  "switch" "sys" "t" "tan" "tanh" "target"
			  "text" "texture" "texture_list"
			  "texture_map" "tga" "thickness" "threshold"
			  "tiff" "tightness" "tile2" "tiles"
			  "tolerance" "toroidal" "torus" "trace"
			  "transform" "translate" "transmit"
			  "triangle" "triangle_wave" "true" "ttf"
			  "turb_depth" "turbulence" "type" "u"
			  "u_steps" "ultra_wide_angle" "undef" "union"
			  "up" "use_alpha" "use_color" "use_colour"
			  "use_index" "utf8" "uv_indices" "uv_mapping"
			  "uv_vectors" "v" "v_steps" "val" "variance"
			  "vaxis_rotate" "vcross" "vdot" "version"
			  "vertex_vectors" "vlength" "vnormalize"
			  "vrotate" "vstr" "vturbulence" "warning"
			  "warp" "water_level" "waves" "while" "width"
			  "wood" "wrinkles" "write" "yes"
;; I think we don't need this, however...			  
			  "x" "y" "z"
			  )
			  t) "\\)\\>")))

(defvar pov-all-directives-matcher
  (eval-when-compile
    (concat "\\<\\("
	    (regexp-opt '("#break" "#case" "#debug" "#declare"
			  "#default" "#else" "#end" "#error" "#fclose"
			  "#fopen" "#if" "#ifdef" "#ifndef" "#include"
			  "#local" "#macro" "#range" "#read" "#render"
			  "#statistics" "#switch" "#undef" "#version"
			  "#warning" "#while" "#write") t)
	    "\\)\\>")))

(defvar pov-all-objects-matcher
  (eval-when-compile
    (concat "\\<\\("
	    (regexp-opt '("background" "bicubic_patch" "blob" "box"
			  "camera" "cone" "cubic" "cylinder" "disc"
			  "fog" "height_field" "isosurface"
			  "julia_fractal" "lathe" "light_group"
			  "light_source" "mesh" "mesh2" "object"
			  "parametric" "plane" "poly" "polygon"
			  "prism" "rainbow" "sky_sphere"
			  "smooth_triangle" "sor" "sphere"
			  "sphere_sweep" "superellipsoid" "text"
			  "torus" "triangle" "quadric" "quartic") t)
	    "\\)\\>")))
		       
		       

(defvar pov-font-lock-keywords
  (list
   ;; highlight variable names after '#declare/#local'
   (list "\\<\\(#declare\\|#local\\)\\>[ \t\n]*\\(\\sw+\\)" 
	 '(2 font-pov-variable-face nil t))
   ;; highlight csg-keywords
   (list "\\<\\(difference\\|intersection\\|merge\\|union\\)\\>" 
	 '(1 font-pov-csg-face))
   ;; highlight variable names after '#macro'
   (list "\\<\\(#macro\\)\\>[ \t\n]*\\(\\sw+\\)" 
	 '(2 font-pov-macro-name-face nil t))
   ;; highlight numbers with type-face
   (list 
    "\\(\\<\\([0-9]*\\.[0-9]+\\|[0-9]+\\)\\|\\.[0-9]+\\)\\([eE][+\\-]?[0-9]+\\)?\\>"
	 '(1 font-pov-number-face))
   ;; highlight operators keywords
   (list "\\([\\-\\+\\|\\^=&!?:/\\>\\<\\*]+\\)" '(1 font-pov-operator-face))
   ;; highlight directives
   (list pov-all-directives-matcher '(1 font-pov-directive-face))
   ;; highlight objects
   (list pov-all-objects-matcher '(1 font-pov-object-face))
   ;; highlight general keywords
   (list pov-all-keyword-matcher '(1 font-pov-keyword-face))
   )
  "Expressions to highlight in PoV mode."
)
;; -- end C.H --


;;;###autoload
(defun pov-mode nil
  "Major mode for editing PoV files. (Version 3.2)

   In this mode, TAB and \\[indent-region] attempt to indent code
based on the position of {} pairs and #-type directives.  The variable
pov-indent-level controls the amount of indentation used inside
arrays and begin/end pairs.  The variable pov-indent-under-declare
determines indent level when you have something like this:
#declare foo =
   some_object {

This mode also provides PoVray keyword fontification using font-lock.
Set pov-fontify-insanely to nil to disable (recommended for large
files!).

\\[pov-complete-word] runs pov-complete-word, which attempts to complete the
current word based on point location.
\\[pov-keyword-help] looks up a povray keyword in the povray documentation.
\\[pov-command-query] will render or display the current file.

\\{pov-mode-map}

\\[pov-mode] calls the value of the variable pov-mode-hook  with no args, if that value is non-nil.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pov-mode-map)
  (pov-setup-syntax-table)
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-column)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'tab-stop-list)
  (make-local-variable 'font-lock-defaults)

  (setq font-lock-keywords pov-font-lock-keywords)
  (setq font-lock-defaults '(pov-font-lock-keywords))
;;   (if (and (boundp 'running-xemacs) running-xemacs)
;;       (pov-toolbar))
  (if pov-imenu-in-menu
      (pov-helper-imenu-setup))

  ;; Create and show the insert menu
  (if (and (file-exists-p pov-insertmenu-location) 
       (file-directory-p pov-insertmenu-location))
       (pov-call-the-insert-menu))
     

  (set-syntax-table pov-mode-syntax-table)
  (setq comment-start "// "
	comment-start-skip "/\\*+ *\\|// *"
	comment-end ""
	comment-multi-line nil
	comment-column 60
	indent-line-function 'pov-indent-line
	tab-stop-list pov-tab-stop-list)
  (setq mode-name "PoV")
  (setq major-mode 'pov-mode)
  (run-hooks 'pov-mode-hook)
)

(defun pov-tab ()
  "Command assigned to the TAB key in PoV mode."
  (interactive)
  (if (save-excursion (skip-chars-backward " \t") (bolp))
      (pov-indent-line)
    (save-excursion
      (pov-indent-line))))

(defun pov-indent-line nil
  "Indents a line of PoV code."
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (if (pov-top-level-p)
      (pov-indent-top-level)
    (if (not (pov-top-level-p))
	(if (pov-in-star-comment-p)
	    (indent-to '2)
	  (if (and (< (point) (point-max))
		   (or
		    (eq ?\) (char-syntax (char-after (point))))
		    (or
		     (looking-at "\\<#\\(end\\|break\\)\\>")
		     (and (looking-at "\\<#else\\>")
			  (not (pov-in-switch-p 0))))))
	      (pov-indent-close)                ; indent close-delimiter
	    (pov-indent-in-block))))))  ; indent line after open delimiter

(defun pov-newline nil
  "Terminate line and indent next line."
  (interactive)
  (newline)
  (pov-indent-line))

(defun pov-in-star-comment-p nil
  "Return true if in a star comment"
  (let ((state
	 (save-excursion
	   (parse-partial-sexp (point-min) (point)))))
    (nth 4 state)))

(defun pov-open nil
  (interactive)
  (insert last-command-char))

(defun pov-close nil
  "Inserts and indents a close delimiter."
  (interactive)
  (insert last-command-char)
  (backward-char 1)
  (pov-indent-close)
  (forward-char 1)
  (blink-matching-open))

(defun pov-indent-close nil
  "Internal function to indent a line containing a close delimiter."
  (if (save-excursion (skip-chars-backward " \t") (bolp))
      (let (x (oldpoint (point)))
	(if (looking-at "#end\\|#else\\|#break")
	    (progn
	      (goto-char (pov-find-begin 0))
	      (if (and (looking-at "#else")
		       (pov-in-switch-p 0))
		  (goto-char (pov-find-begin 0))))
	  (forward-char) (backward-sexp))       ;XXX
	(if (and (eq 1 (count-lines (point) oldpoint))
		 (> 1 (- oldpoint (point))))
	    (goto-char oldpoint)
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (setq x (current-column))
	  (goto-char oldpoint)
	  (delete-horizontal-space)
	  (indent-to x)))))

(defun pov-indent-in-block nil
  "Indent a line which does not open or close a block."
  (let ((goal (pov-block-start)))
    (setq goal (save-excursion
		 (goto-char goal)
		 (back-to-indentation)
		 (if (bolp)
		     pov-indent-level
		   (back-to-indentation)
		   (+ (current-column) pov-indent-level))))
    (indent-to goal)))

(defun pov-indent-top-level nil
  (if (save-excursion 
	(forward-line -1)
	(looking-at "\\<#declare[ \t]+[0-9a-zA-Z_]+[ \t]*=[ \t]*$"))
      (indent-to pov-indent-under-declare)))

;;; returns nil if at top-level, or char pos of beginning of current block

(defun pov-block-start nil
  "Returns the character position of the character following the nearest
enclosing `{' or `begin' keyword."
  (save-excursion
    (let (open (skip 0))
      (setq open (condition-case nil
		     (save-excursion
		       (backward-up-list 1)
		       (1+ (point)))
		   (error nil)))
      (pov-find-begin open))))
(defsubst pov-re-search-backward (REGEXP BOUND NOERROR)
  "Like re-search-backward, but skips over matches in comments or strings"
  (set-match-data '(nil nil))
  (while (and
	  (re-search-backward REGEXP BOUND NOERROR)
	  (pov-skip-backward-comment-or-string)
	  (not (set-match-data '(nil nil))))
    ())
  (match-end 0))

(defun pov-find-begin (start)
  "Search backwards from point to START for enclosing `begin' and returns the
character number of the character following `begin' or START if not found."
  (save-excursion
    (let ((depth 1) match)
      (while (and (> depth 0)
		  (pov-re-search-backward pov-begin-end-re start t))
	(setq depth (if (looking-at pov-end-re)
			(if (and (looking-at "#end")
				 (pov-in-switch-p start))
			    (progn
			      (pov-re-search-backward "\\<#switch\\>" start t)
			      depth)
			  (+ 1 depth))
		      (if (looking-at "\\<#else\\>")
			  (if (pov-in-switch-p start)
			      (1- depth)
			    depth)
			(1- depth)))))
      (if (not (eq 0 depth))
	  start
	(point)))))

(defun pov-in-switch-p (start)
  "Return t if one level under a switch."
  (save-excursion
    (if (looking-at "\\<#end\\>")
	(pov-re-search-backward pov-begin-end-re start t))
    (beginning-of-line)
    (pov-re-search-backward pov-begin-end-re start t)
    (if (looking-at "\\<#else\\>>") (forward-word -1))
    (while (looking-at "\\<#break\\>")
      (progn
	(pov-re-search-backward "\\<#case\\|#range\\>" start t)
	(pov-re-search-backward pov-begin-end-re start t)))
    (pov-re-search-backward pov-begin-end-re start t)
    (looking-at "\\<#switch\\>")))

(defun pov-top-level-p nil
  "Awful test to see whether we are inside some sort of PoVray block."
  (and (condition-case nil
	   (not (scan-lists (point) -1 1))
	 (error t))
       (not (pov-find-begin nil))))


(defun pov-autoindent-endblock nil
  "Hack to automatically reindent end, break, and else."
  (interactive)
  (self-insert-command 1)
  (save-excursion
    (forward-word -1)
    (if (looking-at "\\<#else\\|#end\\|#break\\>")
	(pov-indent-line))))

; Taken from verilog-mode.el
(defun pov-skip-backward-comment-or-string ()
  "Return true if in a string or comment"
  (let ((state
	 (save-excursion
	   (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 state)                      ;Inside string
      (search-backward "\"")
      t)
     ((nth 7 state)                      ;Inside // comment
      (search-backward "//")
      t)
     ((nth 4 state)                      ;Inside /* */ comment
      (search-backward "/*")
      t)
     (t
      nil))))

; *******************
; *** Completions ***
; *******************
;; -------------------------------------------------------------------
;; C.H.: adapted completion to POV-Ray 3.5 syntax to some extent
;; -------------------------------------------------------------------
(defvar pov-completion-str nil)
(defvar pov-completion-all nil)
(defvar pov-completion-pred nil)
;(defvar pov-completion-buffer-to-use nil)
(defvar pov-completion-flag nil)

(defvar pov-global-keywords
  '("#break" "#case" "#debug" "#declare" "#default" "#else" "#end"
    "#fclose" "#fopen" "#include" "#local" "#macro" "#read" "#render"
    "#statistics" "#switch" "#undef" "#version" "#warning" "#write"))

(defvar pov-top-level-keywords
  '("global_settings" "camera" "light_source" "light_group" "media"
  "background" "sky_sphere" "photons" "rainbow"))

(defvar pov-csg-scope-re 
  "\\<inverse\\|union\\|intersection\\|difference\\|merge\\>")

(defvar pov-solid-primitive-keywords
  '("blob" "box" "cone" "cylinder" "height_field" "julia_fractal"
    "lathe" "object" "prism" "sphere" "sphere_sweep" "superellipsoid"
    "sor" "text" "torus" "isosurface" "parametric"))

(defvar pov-blob-keywords
  '("threshold" "cylinder" "sphere" "component" "hierarchy" "sturm"))

(defvar pov-heightfield-keywords
  '("hierarchy" "smooth" "water_level"))

(defvar pob-isosurface-keywords
  '("accuracy" "all_intersections" "contained_by" "evaluate"
    "function" "max_gradient" "max_trace" "method" "open" "threshold"))

(defvar pov-juliafractal-keywords
  '("max_iteration" "precision" "slice" "quaternion" "hypercomplex" "slice"))

(defvar pov-prism-keywords
  '("linear_sweep" "conic_sweep" "linear_spline" "quadratic_spline"
  "cubic_spline" "bezier_spline" "sturm"))

(defvar pov-patch-primitive-keywords
  '("bicubic_patch" "disc" "smooth_triangle" "triangle" "polygon" "mesh" "mesh2"))

(defvar pov-bicubic-keywords
  '("type" "flatness" "u_steps" "v_steps"))

;defvar pov-bezier-keywords
; '("accuracy" "rational" "trimmed_by"))

(defvar pov-infinite-solid-keywords
  '("plane" "cubic" "poly" "quadric" "quartic"))

(defvar pov-csg-keywords
  '("inverse" "union" "intersection" "difference" "merge" "split_union"))

(defvar pov-light-source-keywords
  '("color" "spotlight" "point_at" "radius" "falloff" "tightness"
    "area_light" "adaptive" "jitter" "looks_like" "shadowless"
    "cylinder" "fade_distance" "fade_power" "media_attenuation"
    "media_interaction" "rgb" "circular" "orient" "groups" "parallel"))

(defvar pov-object-modifier-keywords
  '("clipped_by" "bounded_by" "hollow" "no_shadow" "no_reflection"
  "no_image" "interior_texture"))

(defvar pov-transformation-keywords
  '("rotate" "scale" "translate" "matrix" "transform"))

(defvar pov-camera-keywords
  '("perspective" "orthographic" "fisheye" "ultra_wide_angle"
    "omnimax" "panoramic" "cylinder" "spherical" "location" "look_at"
    "right" "up" "direction" "sky" "sphere" "spherical_camera"
    "h_angle" "v_angle" "angle" "blur_samples" "aperture"
    "focal_point" "normal" "rotate" "translate"))

(defvar pov-isosurface-keywords
  '("function" "max_gradient" "contained_by" "threshold" "accuracy" "evaluate" "open"
    "max_trace"
  "all_intersections"))

(defvar pov-texture-keywords
  '("pigment" "normal" "finish" "halo" "texture_map" "material_map"
  "boxed" "planar" "cylindrical" "spherical"))

(defvar pov-pigment-keywords
  '("color" "colour" "colour_map" "color_map" "pigment_map" "pigment"
  "image_map" "quick_color"))

(defvar pov-normal-keywords
  '("slope_map" "normal_map" "bump_map" "bump_size" "boxed"
  "cylindrical" "planar" "spherical"))

(defvar pov-finish-keywords
  '("ambient" "diffuse" "brilliance" "phong" "phong_size" "specular" 
    "roughness" "metallic" "reflection" "irid" 
    "crand"))

(defvar pov-reflection-keywords
  '("fresnel" "falloff" "exponent" "metallic" ))

(defvar pov-irid-keywords
  '("thickness" "turbulence"))


(defvar pov-pattern-keywords
  '("agate" "average" "boxed" "bozo" "brick" "bumps" "cells" "checker"
    "crackle" "cylindrical" "density_file" "dents" "julia" "mandel"
    "magnet" "function" "gradient" "granite" "hexagon" "image_pattern"
    "leopard" "marble" "object" "onion" "pigment_pattern" "planar"
    "quilted" "radial" "ripples" "slope" "spherical" "spiral1"
    "spiral2" "spotted" "wood" "waves" "wrinkles" "frequency" "phase"
    "ramp_wave" "triangle_wave" "sine_wave" "scallop_wave"
    "cubic_wave" "poly_wave" "noise_generator" "turbulence" "octaves"
    "omega" "lambda" "warp"))

(defvar pov-media-keywords '("intervals" "samples" "confidence"
  "variance" "ratio" "absorption" "emission" "scattering" "density"
  "color_map" "density_map" "light_group" "sample_method" "aa_level"
  "aa_threshold" "jitter" "method"))

(defvar pov-interior-keywords '("ior" "caustics" "fade_distance"
  "fade_power" "media" "dispersion" "dispersion_samples"
  "fade_color"))

(defvar pov-texture-keywords '("pigment" "normal" "finish"
  "texture_map" "material_map"))

(defvar pov-material-keywords '("texture" "interior"))

(defvar pov-warp-keywords '("repeat" "black_hole" "turbulence"
  "cylindrical" "spherical" "toroidal" "planar" "orientation"
  "dist_exp" "major_radius" "offset" "flip" "strength" "falloff"
  "inverse"))

(defvar pov-density-keyword '("colour" "colour_map" "boxed" "planar"
  "cylindrical" "spherical"))

(defvar pov-fog-keywords '("fog_type" "distance" "color" "turbulence"
 "turb_depth" "omega" "lambda" "octaves" "fog_offset" "fog_alt" "up"))

(defvar pov-rainbow-keywords '("direction" "angle" "width" "distance"
 "jitter" "up" "arc_angle" "falloff_angle"))

(defvar pov-global-settings-keywords '("adc_bailout" "ambient_light"
  "assumed_gamma" "charset" "hf_gray_16" "irid_wavelength"
  "max_intersections" "max_trace_level" "number_of_waves" "radiosity"
  "reflection_samples" "photons" "noise_generator"))

(defvar pov-radiosity-keywords '("adc_bailout" "always_sample"
  "brightness" "count" "distance_maximum" "error_bound"
  "gray_threshold" "low_error_factor" "minimum_reuse" "nearest_count"
  "recursion_limit" "max_sample" "media" "normal" "pretrace_end"
  "pretrace_start" "recursion_limit" "save_file"))

(defvar pov-object-keywords '("texture" "pigment" "finish" "interior"
 "normal" "no_shadow"))

;;AS
(defvar pov-keyword-completion-alist
  (mapcar (function
	   (lambda (item) (list item item)))
	   (append 
	    pov-global-keywords
	    pov-top-level-keywords
	    pov-solid-primitive-keywords
	    pov-blob-keywords
	    pov-heightfield-keywords
	    pov-juliafractal-keywords
	    pov-prism-keywords
	    pov-patch-primitive-keywords
	    pov-bicubic-keywords
	    pov-infinite-solid-keywords
	    pov-csg-keywords
	    pov-light-source-keywords
	    pov-object-modifier-keywords
	    pov-transformation-keywords
	    pov-camera-keywords
	    pov-texture-keywords
	    pov-pigment-keywords
	    pov-normal-keywords
	    pov-finish-keywords
	    pov-reflection-keywords
	    pov-irid-keywords
	    pov-pattern-keywords
	    pov-pattern-keywords
	    pov-media-keywords
	    pov-interior-keywords
	    pov-texture-keywords
	    pov-material-keywords
	    pov-warp-keywords
	    pov-fog-keywords
	    pov-rainbow-keywords
	    pov-global-settings-keywords
	    pov-radiosity-keywords
	    pov-object-keywords
	    )))

(defun pov-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

(defun pov-get-scope nil
  "Return the scope of the POV source at point"
  (interactive)
  (save-excursion
    (if (not (pov-top-level-p))
	(progn
	  (backward-up-list 1)
	  (forward-word -1)
	  (cond
	   ((looking-at "camera")
	    (setq pov-completion-list pov-camera-keywords))
	   ((looking-at "texture")
	    (setq pov-completion-list 
		  (append pov-texture-keywords pov-pattern-keywords)))
	   ((looking-at "material")
	    (setq pov-completion-list 
		  (append pov-material-keywords pov-pattern-keywords)))
	   ((looking-at "pigment")
	    (setq pov-completion-list 
		  (append pov-pigment-keywords pov-pattern-keywords)))
	   ((looking-at "normal")
	    (setq pov-completion-list 
		  (append pov-normal-keywords pov-pattern-keywords)))
	   ((looking-at "density")
	    (setq pov-completion-list 
		  (append pov-density-keywords pov-pattern-keywords)))
	   ((looking-at "finish")
	    (setq pov-completion-list pov-finish-keywords))
	   ((looking-at "warp")
	    (setq pov-completion-list pov-warp-keywords))
	   ((looking-at "finish")
	    (setq pov-completion-list pov-finish-keywords))
	   ((looking-at "reflection")
	    (setq pov-completion-list pov-reflection-keywords))
	   ((looking-at "irid")
	    (setq pov-completion-list pov-irid-keywords))
	   ((looking-at "blob")
	    (setq pov-completion-list pov-blob-keywords))
	   ((looking-at "isosurface")
	    (setq pov-completion-list pov-isosurface-keywords))
	   ((looking-at "heightfield")
	    (setq pov-completion-list pov-heightfield-keywords))
	   ((looking-at "prism")
	    (setq pov-completion-list pov-prism-keywords))
	   ((looking-at "julia_fractal")
	    (setq pov-completion-list pov-juliafractal-keywords))
	   ((looking-at "bicubic")
	    (setq pov-completion-list pov-bicubic-keywords))
	   ((looking-at "bezier")
	    (setq pov-completion-list pov-bezier-keywords))
	   ((looking-at "trimmed_by")
	    (setq pov-completion-list pov-bezier-keywords))
	   ((looking-at "light_source")
	    (setq pov-completion-list pov-light-source-keywords))
	   ((looking-at "interior")
	    (setq pov-completion-list pov-interior-keywords))
	   ((looking-at "media")
	    (setq pov-completion-list pov-media-keywords))
	   ((looking-at "fog")
	    (setq pov-completion-list pov-fog-keywords))
	   ((looking-at "global_settings")
	    (setq pov-completion-list pov-global-settings-keywords ))
	   ((looking-at "radiosity")
	    (setq pov-completion-list pov-radiosity-keywords))
	   ((looking-at "photons")
	    (setq pov-completion-list pov-photons-keywords))
	   ((looking-at pov-csg-scope-re)
	    (setq pov-completion-list (append
				       pov-solid-primitive-keywords 
				       pov-infinite-solid-keywords 
				       pov-object-modifier-keywords 
				       pov-csg-keywords)))
	   (t
	    (setq pov-completion-list 
		  (append pov-object-modifier-keywords 
			  pov-object-keywords))))
	  (setq pov-completion-list 
		(append pov-completion-list pov-transformation-keywords)))
      (setq pov-completion-list (append 
				 pov-top-level-keywords
				 pov-solid-primitive-keywords 
				 pov-infinite-solid-keywords 
				 pov-patch-primitive-keywords 
				 pov-csg-keywords)))
    ;Append the language directives so that they are available at all places.
    (setq pov-completion-list (append pov-completion-list pov-global-keywords))))

(defun pov-completion (pov-completion-str pov-completion-pred pov-completion-flag)
  (save-excursion
    (let ((pov-completion-all nil))
      (pov-get-scope)
      (mapcar '(lambda (s)
		 (if (string-match (concat "\\<" pov-completion-str) s)
		     (setq pov-completion-all (cons s pov-completion-all))))
	      pov-completion-list)
      ;; Now we have built a list of all matches. Give response to caller
      (pov-completion-response))))

(defun pov-completion-response ()
  (cond ((or (equal pov-completion-flag 'lambda) (null pov-completion-flag))
	 ;; This was not called by all-completions
	 (if (null pov-completion-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr pov-completion-all))
		  (match (car pov-completion-all))
		  (min (length match))
		  tmp)
	     (if (string= match pov-completion-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (pov-string-diff match (car elm))) min)
		     (progn
		       (setq min tmp)
		       (setq match (substring match 0 min))))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) pov-completion-str)
		     (progn
		       (setq match t)
		       (setq elm nil))
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal pov-completion-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(pov-completion-flag
	 pov-completion-all)))

(defun pov-complete-word ()
  "Complete word at current point based on POV syntax."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9#_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9#_") (point)))
	 (pov-completion-str (buffer-substring b e))
	 ;; The following variable is used in pov-completion
	 ;(pov-buffer-to-use (current-buffer))
	 (allcomp (all-completions pov-completion-str 'pov-completion))
	 (match (try-completion
		 pov-completion-str (mapcar '(lambda (elm)
					       (cons elm 0)) allcomp))))
    ;; Delete old string
    (delete-region b e)
    ;; Insert match if found, or the original string if no match
    (if (or (null match) (equal match 't))
	(progn (insert "" pov-completion-str)
	       (message "(No match)"))
      (insert "" match))
    ;; Give message about current status of completion
    (cond ((equal match 't)
	   (if (not (null (cdr allcomp)))
	       (message "(Complete but not unique)")
	     (message "(Sole completion)")))
	  ;; Display buffer if the current completion didn't help
	  ;; on completing the label.
	  ((and (not (null (cdr allcomp))) (= (length pov-completion-str)
					      (length match)))
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list allcomp))
	   ;; Wait for a keypress. Then delete *Completion*  window
	   (momentary-string-display "" (point))
	   (delete-window (get-buffer-window (get-buffer "*Completions*")))))))

(defun pov-get-the-default-image-name nil
  "Return the default file name of the rendered image"
  (concat (file-name-sans-extension (buffer-file-name)) "."
	  pov-default-image-extension))



;; wrappers
(defun pov-tool-bar-command-render nil
  "Wrapper for the tool-bar: render the buffer at default quality"
  (interactive)
  (pov-render-file "Render" (buffer-file-name) nil))

(defun pov-menu-render-test nil
  "Wrapper for the menu and the keybinding: render the buffer at test quality
without questions"
  (interactive)
  (pov-render-file "Test quality render" (buffer-file-name) nil))

(defun pov-menu-render-low nil
  "Wrapper for the menu and the keybinding: render the buffer at low quality
without questions"
  (interactive)
  (pov-render-file "Low quality render" (buffer-file-name) nil))

(defun pov-menu-render-mid nil
  "Wrapper for the menu and the keybinding: render the buffer at medium quality
without questions"
  (interactive)
  (pov-render-file "Medium quality render" (buffer-file-name) nil))

(defun pov-menu-render-high nil
  "Wrapper for the menu and the keybinding: render the buffer at high quality
without questions"
  (interactive)
  (pov-render-file "High quality render" (buffer-file-name) nil))

(defun pov-menu-render-highest nil
  "Wrapper for the menu and the keybinding: render the buffer at highest quality
without questions"
  (interactive)
  (pov-render-file "Highest quality render" (buffer-file-name) nil))

(defun pov-tool-bar-command-view nil
  "Wrapper for the tool-bar: view the rendered image"
  (interactive)
  (if pov-default-view-internal
      (pov-display-image-xemacs pov-image-file)
    (pov-display-image-externally pov-image-file nil)))

(defun pov-menu-external-viewer nil
  "View the rendered image using the external viewer"
  (interactive)
  (pov-display-image-externally pov-image-file nil))

(defun pov-menu-internal-viewer nil
  "View the rendered image using the internal viewer"
  (interactive)
  (pov-display-image-xemacs pov-image-file))

;;; initialize the keymap if it doesn't already exist
(if (null pov-mode-map)
    (progn
      (setq pov-mode-map (make-sparse-keymap))
      (define-key pov-mode-map "{" 'pov-open)
      (define-key pov-mode-map "}" 'pov-close)
      (define-key pov-mode-map "\t" 'pov-tab)
      (define-key pov-mode-map "\M-\t" 'pov-complete-word)
      (define-key pov-mode-map "\r" 'pov-newline)
      (define-key pov-mode-map "\C-c\C-cc" 'pov-command-query) ;AS
      (define-key pov-mode-map "\C-c\C-ch" 'pov-keyword-help) 
      (define-key pov-mode-map "\C-c\C-cr" 'pov-tool-bar-command-render)
      (define-key pov-mode-map "\C-c\C-cl" 'pov-show-render-output) 
      (define-key pov-mode-map "\C-c\C-c1" 'pov-menu-render-test)
      (define-key pov-mode-map "\C-c\C-c2" 'pov-menu-render-low)
      (define-key pov-mode-map "\C-c\C-c3" 'pov-menu-render-mid)
      (define-key pov-mode-map "\C-c\C-c4" 'pov-menu-render-high)      
      (define-key pov-mode-map "\C-c\C-c5" 'pov-menu-render-highest)
      (define-key pov-mode-map "\C-c\C-ci" 'pov-open-include-file) 
      (define-key pov-mode-map "\C-c\C-ce" 'pov-menu-external-viewer)      
      (define-key pov-mode-map "\C-c\C-cv" 'pov-menu-internal-viewer)  

      ;; Misc menu 

      (define-key pov-mode-map [menu-bar Misc] 
	(cons "Misc" (make-sparse-keymap "Misc")))
      (define-key pov-mode-map [menu-bar Misc comment]
	    '(menu-item "Comment out region" comment-region
			:help "Comment out the region")) 
     (define-key pov-mode-map [menu-bar Misc uncomment]
       '(menu-item "Uncomment the region" uncomment-region
		   :help "Uncomment the region"))   
     (define-key pov-mode-map [menu-bar Misc kwhelp]
       '(menu-item "Keyword look up" pov-keyword-help
		   :help "Open the documentation searching for a keyword"))
     (define-key pov-mode-map [menu-bar Misc open-the-include]
       '(menu-item "Open the standard include files" pov-open-include-file
		   :help "Open the standard include files"))
     
      ;;  View menu
      
      (define-key pov-mode-map [menu-bar View] 
	(cons "View" (make-sparse-keymap "View")))
      (define-key pov-mode-map [menu-bar View ext]
	'(menu-item "External" pov-menu-external-viewer
		    :help "View the image using an external viewer")) 
      (define-key pov-mode-map [menu-bar View int]
	'(menu-item "Internal" pov-menu-internal-viewer
		    :help "View the image internally"))
      

	  
      ;; Render menu
      (define-key pov-mode-map [menu-bar Render]
	(cons "Render" (make-sparse-keymap "Render")))

      (define-key pov-mode-map [menu-bar Render highest]
	'(menu-item "Highest quality"  pov-menu-render-highest
		    :help "Go! Render at highest quality!"))

      (define-key pov-mode-map [menu-bar Render high]
	'(menu-item "High quality"  pov-menu-render-high
		    :help "Render at high quality!"))

      (define-key pov-mode-map [menu-bar Render mid]
	'(menu-item "Medium quality"  pov-menu-render-mid
		    :help "Render at medium quality"))

      (define-key pov-mode-map [menu-bar Render low]
	'(menu-item "Low quality"  pov-menu-render-low
		    :help "Render at low quality"))

      (define-key pov-mode-map [menu-bar Render test]
	'(menu-item "Test quality"  pov-menu-render-test
		    :help "Just test quality"))

      (define-key pov-mode-map [menu-bar Render default]
	'(menu-item "Default"  pov-tool-bar-command-render
		    :help "Render at default quality"))
      ;; tool-bar entries for GNU Emacs
      (if font-pov-is-Emacs
	  (progn 
	    (setq viewicon (concat pov-icons-location "povview.xpm")
		  rendericon (concat pov-icons-location "povrender.xpm"))
	    (define-key pov-mode-map [tool-bar  render] 
	      ;;      '(menu-item "Render" pov-render-dialog
	      `(menu-item "Render this file using the default quality" 
			  pov-tool-bar-command-render
			  :image ,(create-image rendericon )))

	    (define-key pov-mode-map [tool-bar view] 
	      `(menu-item "Preview"   pov-tool-bar-command-view
			  :image ,(create-image viewicon)))))))
  


     

;; Hack to redindent end/else/break
(if pov-autoindent-endblocks
    (progn
      (define-key pov-mode-map "e" 'pov-autoindent-endblock)
      (define-key pov-mode-map "k" 'pov-autoindent-endblock)
      (define-key pov-mode-map "d" 'pov-autoindent-endblock)))






;; ***********************
;; *** povkeyword help *** 
;; *********************** 



(defun pov-keyword-help nil
   "look up the appropriate place for keyword in the POV documentation 
using an external browser (set by browse-url.el). Keyword can be entered 
and autocompleteted, default is word at point"
 (interactive)
  (let* ((default (current-word))
	 (input (completing-read
		 (format "lookup keyword (default %s): " default)
		 pov-keyword-completion-alist))
	 (kw (if (equal input "")
		 default
	       input))
	 (buffer (find-file-noselect  
		  (concat 
		   (file-name-as-directory pov-documentation-directory) 
		   pov-documentation-keyword-index )))
	 (buffer-index (find-file-noselect
			(concat (file-name-as-directory pov-documentation-directory)
				pov-documentation-index)))
	; ( browse-url-generic-program pov-external-browser )
	 target-file)
    (save-excursion 
      (set-buffer buffer)
      (setq buffer-read-only t)
      (widen)
      (goto-char (point-min))
      (setq case-fold-search t) ;; it's buffer-local 
      (if (re-search-forward 
	   (concat "<code>" kw "</code>") 
	  (save-excursion       
	      (search-forward "<code>z</code>")) t) 
	  (progn (re-search-backward  "href=\"\\([^\"]+\\)\">")
		 (setq target-file (match-string-no-properties 1))
		 (if (not (string-match "s_.*\\.html" target-file))
		     (setq target-file (concat 
					pov-documentation-keyword-index  
					target-file)))
		 (browse-url (concat "file://" (file-name-as-directory 
						pov-documentation-directory)
				     target-file)))
	(progn (set-buffer buffer-index)
	       (setq buffer-read-only t)
	       (widen)
	       (goto-char (point-min))
	       (setq case-fold-search t) ;; it's buffer-local 
	       (if  (re-search-forward (concat "^[ \t]*" kw)
				       (save-excursion (point-max)) t) 
					; just the 1^ entry
		    (progn (re-search-forward "href=\"\\([^\"]+\\)\">")
		   	 (setq target-file (match-string-no-properties 1))
			 (browse-url (concat "file://" 
					     (file-name-as-directory 
					      pov-documentation-directory)
					     target-file)))
		 (message "Couldn't find keyword: %s, maybe you misspelled it" kw))))
      (kill-buffer buffer)
      (kill-buffer buffer-index))))

;; ;; search the pov-ray documentation with google
;; STILL IN DEVELOPMENT. UNCOMMENT AND RECOMPILE IF YOU WANT TO TRY THIS
;; (defun pov-online-search nil
;;   "Search the POV-Ray site and on-line documentation for a keyword"
;;   (interactive)
;;   (let* ((default (current-word))
;; 	 (input (completing-read
;; 		 (format "lookup keyword (default %s): " default)
;; 		 pov-keyword-completion-alist))
;; 	 (kw (if (equal input "")
;; 		 default
;; 	       input)))
;;     (browse-url (concat "http://www.google.com/custom?q=" 
;; 			kw 
;; 			"&sa=Google+Search&domains=povray.org%3B+news.povray.org%3B+"
;; 			"www.povray.org&sitesearch=www.povray.org"))))


; **********************************
; *** Open standard include file ***
; **********************************
;; (defun pov-open-include-file nil
;;   (interactive)
;;   "Open one of the standard include files"
;;   (let* ((default (current-word))
;; 	 (input (completing-read
;; 		 (format "File to open (default %s): " default)
;; 		 pov-keyword-completion-alist))
;; 	 (kw (if (equal input "")
;; 		 default
;; 	       input)))
;;     ;(get-buffer-create kw)
;;     ;(switch-to-buffer-other-window kw)
;;     ;(message (concat pov-include-dir (concat kw ".inc")))
;;     (find-file-read-only (concat (file-name-as-directory pov-include-dir) 
;; 				 (concat kw ".inc")))))
;;MP
(defun pov-open-include-file nil
  (interactive)
  "Open one of the standard include files"
  (let* ((default (current-word))
	 (input (completing-read
		 (format 
		  "File to open (default %s.inc), complete with TAB: " default)
		 (directory-files pov-include-dir nil "\\.inc")))
	 (kw (if (equal input "")
		 (concat default ".inc")
	       input))
	 (target-file (concat (file-name-as-directory pov-include-dir) kw)))
    (if (file-exists-p target-file)
	(find-file-read-only target-file)
      (message "I can't find %s. 
Maybe you misspelled it?" target-file))))

; ***************************
; *** Commands for povray ***
; ***************************

;;; Execution of Povray and View processes
(defvar pov-next-default-command "Render" ;XXX
  "The default command to run next time pov-command-query is run")
(defvar pov-last-render-command "Render" ;XXX
  "The last command used to render a scene")
(defvar pov-rendered-succesfully nil
  "Whether the last rendering completed without errors")
(defvar pov-doc-buffer-name "*Povray Doc*"
  "The name of the buffer in which the documentation will be displayed")
;; will be set to *Pov Render <buffer-name>*
(defvar pov-render-buffer-name ""
  "The name of the buffer that contains the rendering output")
(defvar pov-current-render-process nil
  "The process rendering at the moment or nil")
(defvar pov-current-view-processes (make-hash-table)
  "The processes that display pictures at the moment")
(defvar pov-buffer-modified-tick-at-last-render 0
  "The number of modifications at time of last render")

;;make all the render variables buffer-local that are pov-file
;;dependent, so that users can render more than one file at the same
;;time etc.  Note: for the *view processes* a hash is used (rather
;;then making the variables local, because somebody might want to view
;;a file from a different render buffer.

(mapc 'make-variable-buffer-local
      '(pov-command-alist ;because of history XXX
	pov-next-default-command
	pov-last-render-command
	pov-image-file
	pov-render-buffer-name
	pov-buffer-modified-tick-at-last-render
	pov-current-render-process))

(defvar pov-image-file ""
  "The name of the rendered image that should be displayed"
  )

(defun pov-default-view-command ()
  "Return the default view command (internal or external)"
  (if pov-default-view-internal
      pov-internal-view
    pov-external-view))

(defun pov-command-query () ;XXX
  "Query the user which command to execute"
  ;;XXX this one is still a mess
  (interactive)
  ;;Check whether the buffer has been modified since last call,
  ;;and the last rendering was succesful. If so he probably
  ;;wants to render, otherwise he wants to view.
  (let* ((default
	   (if (and (= (buffer-modified-tick)
		       pov-buffer-modified-tick-at-last-render)
		    pov-rendered-succesfully)
	       (pov-default-view-command)
	     pov-last-render-command))
	 (completion-ignore-case t)
	 (pov-command (completing-read
		       (format "Which command (default: %s)? " default)
		       pov-command-alist nil t nil t)))
    (setq pov-command
	  (if (not (string-equal pov-command ""))
	      pov-command
	    default))
    (setq pov-next-default-command pov-command)
  ;;XXX argl: all this information should be in pov-command-alist
  (cond ((string-match pov-command pov-internal-view)
	 (pov-display-image-xemacs pov-image-file)) ;XXX
	((string-match pov-command pov-external-view)
	 (pov-display-image-externally pov-image-file t))
	(t
	   (setq pov-buffer-modified-tick-at-last-render
		 (buffer-modified-tick))
	;   (message (format
	;	     "DEBUG: buffer %s modified tick%d "
	;	     (buffer-name)
	;	     (buffer-modified-tick)))
	   (pov-render-file pov-command (buffer-file-name) t)
	   ))))

(defun pov-render-file (pov-command file verify-render)
  "Render a file using pov-command."
  ;;XXX Check that there isn't already a render running
  (when
      (or
       (not pov-current-render-process)
       (and pov-current-render-process
	    (cond ((y-or-n-p
		    ;;XXX could theoretically be also running in other buffer...
		    "There is a render process already running: abort it?")
		   (delete-process pov-current-render-process) ; MP
		   (message "Process killed")
		   t)
		  )))
    (let ((render-command nil)
	  (render-command-options nil)
	  (render-command-history nil)
	  (old-buffer (current-buffer))
	  (process nil))

      ;; if the user hasn't saved his pov-file, ask him
    (if (buffer-modified-p)
	(and (y-or-n-p
	      (concat (buffer-name (current-buffer)) " modified; save ? "))
	     (save-buffer)))
    ;; assign the buffer local value of the render buffer name
    (setq pov-render-buffer-name (format "*Povray Render %s*" file))
    (set-buffer (get-buffer-create pov-render-buffer-name))
    ;(switch-to-buffer (buffer-name))   % XXX use this for 2.11, and fix it
    (erase-buffer)
    (setq render-command (second (assoc pov-command pov-command-alist)))
    (setq render-command-options (format
				  (third (assoc pov-command pov-command-alist))
				  file))
    (setq render-command-history
	  (fourth (assoc pov-command pov-command-alist)))
    ;(message (format "DEBUG FUCK %s %s"render-command-options (or
    ;		render-command-history "NIL")))
    (if verify-render
	 (setq render-command-options
	       (read-string "Render with the following options: "
			    render-command-options
			    'render-command-history)))
    (message (format "Running %s on %s" pov-command file))
    (insert (format "Running %s on %s with: %s %s..." pov-command file
		    render-command  render-command-options))

    (setq process (apply 'start-process pov-command (current-buffer)
			 render-command
			 (split-string render-command-options)))
    ;; memorize what we are doing
    (setq pov-last-render-command pov-command)
    ;; FIXME this might be dubious
    (setf (fourth (assoc pov-command pov-command-alist))
	  render-command-history)
    ;;(message (format "DEBUG proc: %s" process))
    ;;XXX 'coz pov-current-render-process is buffer-local
    ;(get-buffer old-buffer)
    (set-buffer old-buffer)
    (setq pov-current-render-process process)
    (set-process-filter process 'pov-render-filter)
    (set-process-sentinel process 'pov-render-sentinel))))

(defun pov-show-render-output ()
  "Pop up the output of the last render command."
  (interactive)
  (let ((buffer (get-buffer pov-render-buffer-name)))
    (if buffer
	(let ((old-buffer (current-buffer)))
	  (pop-to-buffer buffer t)
	  (bury-buffer buffer)
	  (goto-char (point-max))
	  (pop-to-buffer old-buffer))
      (error "No rendering done so far"))))

(defun pov-render-sentinel (process event)
 "Sentinel for povray call."
 ;;so we aren't rendering any more ;XXX
 (setq pov-current-render-process nil)
 ;;If the process exists successfully then kill the ouput buffer
 (cond ((and 
	 (equal 0 (process-exit-status process))
	 (not pov-errors))
	(setq pov-rendered-succesfully t)
	(message "Image rendered succesfully"))
       (t
	(message (concat "Errors in " (process-name process)
			", press C-c C-c l to display"))
	(setq pov-rendered-succesfully nil))))


(defun pov-render-filter (process string)
  "Filter to process povray output. Scrolls and extracts the
filename of the output image (XXX with a horrible buffer-local-hack...)"
  ;(message (format "DEBUG buffer name %s" (buffer-name (current-buffer))))
;; I'm going to use (concat (file-name-sans-extension
;; (buffer-file-name)) ".png"). Much simplier, and avoids the
;; long-long-path-bug
  (let ((image-file nil))
    (save-excursion
      (set-buffer (process-buffer process))
      (save-excursion
	;; find out how our file is called
	(if (string-match "^ *Output file: \\(.*\\), [0-9]+ bpp.*$" string)
	    (setq image-file (match-string 1 string)))
	(if (string-match "Error:" string)
	    (setq pov-errors t)
	  (setq pov-errors nil))

	(goto-char (process-mark process))
	(insert-before-markers string)
	(set-marker (process-mark process) (point))))
    (if image-file  (setq pov-image-file image-file))))

(defun pov-external-view-sentinel (process event)
  ;;seems like we finished viewing => remove process from hash
  (cl-remhash (process-name process) pov-current-view-processes)
  (if (equal 0 (process-exit-status process))
      (message (concat "view completed successfully")) ;XXX
    (message (format "view exit status %d"
		     (process-exit-status process)))))

(defun pov-display-image-externally (file verify-display)
  "Display the rendered image using external viewer"
  ;;if we don't have a file, prompt for one
  (when (or (not file) (string-equal file ""))
    (if (file-exists-p (pov-get-the-default-image-name))
	(setq file (pov-get-the-default-image-name)) ;; MP
      (setq file
	  (read-file-name "Which image file should I display? "))))
  (let ((view-command nil)
	(view-options nil)
	(view-history nil)
	(other-view (cl-gethash (concat pov-external-view file) 
				pov-current-view-processes))
	(process nil))
    (if (and other-view (processp other-view))  ;external
	(if (not (y-or-n-p
		  (format "Do yo want to want to kill the old view of %s?" file)))
	    (delete-process other-view)))
    (setq view-command (second (assoc pov-external-view pov-command-alist)))
    (setq view-options (format
			(third (assoc pov-external-view pov-command-alist))
			file))
    (setq view-history (fourth (assoc pov-external-view pov-command-alist)))
    (if verify-display 
	(setq view-options (read-string "View with the following options: "
					view-options
					view-history)))
    (message (format "Viewing %s with %s %s" file view-command view-options))
    (setq process (apply 'start-process (concat pov-external-view file) nil
			 view-command (split-string view-options)))
    ;;; remember what we have done
    (cl-puthash (process-name process) process pov-current-view-processes)
    ;; update history
    (setf (fourth (assoc pov-external-view pov-command-alist)) view-history)
    ;;Sentinel for viewer call (XXX argl, what a hack)
    (set-process-sentinel process 'pov-external-view-sentinel)))
    ;;	'(lambda (process event)

(defun pov-display-image-xemacs (file)
  "Display the rendered image"
  ;;TODO: set frame according to image-size (seems difficult)
  (when (or (not file) (string-equal file ""))
    (if (file-exists-p (pov-get-the-default-image-name))
	(setq file (pov-get-the-default-image-name))
      (setq file
	  (read-file-name "Which image file should I display? "))))
  (let ((buffer (get-buffer-create
		 (format "*Povray View %s*" file))))
    (save-excursion
      (set-buffer buffer)
      (toggle-read-only -1)
      (erase-buffer)
      (if (and  font-pov-is-Emacs22 (image-type-available-p 'png)) ;; MP
	  (progn (clear-image-cache) ;; really important!
		 (insert "\n")
		 (goto-char (point-max))
		 (insert-image (create-image file) )
		 (insert "              \n\nType C-x b to go back!") ;; avoid blinking cursor
		 (goto-char  (- (point-max) 1))
		 (switch-to-buffer (current-buffer)))))))



; *************
; *** Imenu ***  
; *************
(defun pov-helper-imenu-setup ()
  (interactive)
  ; (require 'imenu) ;; Make an index for imenu  
  (if pov-imenu-only-macros
  (setq imenu-generic-expression 
	'((nil "^#macro\\s-+\\([A-Za-z_][A-Za-z_0-9]*\\)" 1)))
  ;;I assume that #macro and #declare start at the beginning of the line,
  ;;without spaces. Otherwise could be recursion loops.
  (setq imenu-generic-expression 
	'((nil "^#\\(declare\\|macro\\)\\s-+\\([A-Za-z_][A-Za-z_0-9]*\\)" 2)
	  (nil "^\\(camera\\)" 1) ;; there is only one camera, isn't it?
	  ("Lights" "^\\(light_source\\)" 1)
	  ("Include" "^#include\\s-+\"\\([A-Za-z0-9_]+\\)\\.inc\"" 1))))
  (imenu-add-to-menubar "PoV"))

;; STAR INSERT MENU SUPPORT ;;

(defun pov-im-get-submenunames ()
  (interactive)
  (pov-im-get-dirs (cddr (directory-files pov-insertmenu-location t)))
  )

(defun pov-im-get-dirs (dirs)
  (when (eq dirs nil)
    (return ()))
  (catch '--cl-block-nil--
    (if (file-directory-p (car dirs))
	(return (cons
		 (car dirs)
		 (if (eq (cdr dirs) nil)
		     nil
		   (pov-im-get-dirs (cdr dirs)))
		 ))
      (return (pov-im-get-dirs (cdr dirs)))))
  )

(defun pov-im-make-menu ()
  (easy-menu-define
   pov-im-menu
   pov-mode-map
   "The POV-Ray Insert menu"
   (cons "Insert" (pov-im-create-menu (pov-im-get-submenunames))))
  )

(defun pov-im-create-menu (dirs)
  (when (eq dirs nil)
    (return ()))
  (catch '--cl-block-nil--
    (return (cons
	     (pov-im-create-submenu (car dirs))
	     (if (eq (cdr dirs) nil)
		 nil
	       (pov-im-create-menu (cdr dirs)))
	     )))
  )

(defun pov-im-create-submenu (dir)
  (cons (substring (file-name-nondirectory dir) 5) 
	(pov-im-get-menuitems (directory-files dir t ".*txt")))
  )

(defun pov-im-get-menuitems (items)
  (when (eq items nil)
    (return nil))
  (catch '--cl-block-nil--
    (if (string-match "-.txt" (car items))
	(return (cons
		 "----"
		 (if (eq (cdr items) nil)
		     nil
		   (pov-im-get-menuitems (cdr items)))
		 ))
      (if (string-match " - " (file-name-nondirectory (car items)))
	  (return (cons
		   (vector (substring (file-name-sans-extension
				       (file-name-nondirectory (car items))) 5)
			   (list 'pov-im-include-file (car items)) t)
		   (if (eq (cdr items) nil)
		       nil
		     (pov-im-get-menuitems (cdr items)))
		   ))
	(return (cons
		 (vector (file-name-sans-extension
			  (file-name-nondirectory (car items)))
			 (list 'pov-im-include-file (car items)) t)
		 (if (eq (cdr items) nil)
		     nil
		   (pov-im-get-menuitems (cdr items)))
		 ))
	)))
  )

(defun pov-im-include-file (file)
  (insert-file-contents file)
  )

(defun pov-call-the-insert-menu ()
  "Create the InsertMenu add-on"
  (interactive)
  (pov-im-make-menu)
  (easy-menu-add pov-im-menu))

;; END INSERT MENU SUPPORT ;;

(provide 'pov-mode)
;;; pov-mode.el ends here
