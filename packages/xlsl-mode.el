;-*- coding: utf-8 -*-
;; xlsl-mode.el -- Major mode for editing LSL (Linden Scripting Language).

;; Copyright © 2008, 2009, 2010, 2011 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: LSL, Second Life, Linden Scripting Language

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; DESCRIPTION

;; A major mode for editing LSL (Linden Scripting Language).
;; for download location and documentation, see:
;; http://xahlee.org/sl/ls-emacs.html

;;; INSTALL

;; Open the file, then type Alt+x eval-buffer.
;; Open any LSL source code, then type Alt+x xlsl-mode, you'll have syntax coloring and other features.

;; To have emacs automatically load the file when it restarts, and automatically use the mode when opening files ending in “.lsl”, follow these steps:

;; Rename the file to “xlsl-mode.el” (if the file is not already that name). Now, put the following lines in your emacs init file “.emacs”:

;; (add-to-list 'load-path "~/.emacs.d/") ;; create the dir if it doesn't exist
;; (autoload 'xlsl-mode "xlsl-mode" "Load xlsl-mode for editing Linden Scripting Lang." t)
;; (add-to-list 'auto-mode-alist '("\\.lsl\\'" . xlsl-mode))

;; Then, restart emacs.

;;; DOCUMENTATION

;; Full documentation is at: http://xahlee.org/sl/ls-emacs.html

;; To see the inline documentation in emacs, type “C-h m”
;; (describe-mode). (if you have not load the mode type, first type
;; Alt+x xlsl-mode)

;;; HISTORY

;; version 1.6.1, 2011-05-04 • added constants OBJECT_RUNNING_SCRIPT_COUNT OBJECT_SCRIPT_MEMORY OBJECT_TOTAL_SCRIPT_COUNT
;; version 1.6.0, 2011-04-25 • Added function completion for these functions: llCastRay llClearPrimMedia llGetEnv llGetLinkNumberOfSides llGetLinkPrimitiveParams llGetPrimMediaParams llGetSPMaxMemory llGetUsedMemory llGetUsername llLinkParticleSystem llRegionSayTo llRequestUsername llScriptProfiler llSetLinkPrimitiveParamsFast llSetLinkTextureAnim llSetPrimMediaParams llTextBox.
;; version 1.5.16, 2010-12-19 • Added completion for AGENT_BY_LEGACY_NAME, AGENT_BY_USERNAME, llGetDisplayName, llRequestDisplayName.
;; version 1.5.15, 2010-01-16 • Added about 5 STATUS_* constants, e.g. STATUS_BLOCK_GRAB.
;; version 1.5.14, 2009-12-14 • Minor improvement in the help menu “LSL‣About xlsl-mode.”, and added the corresponding command xlsl-about. 
;; version 1.5.13, 2009-11-15 • Added AGENT Constant.
;; version 1.5.12, 2009-10-10 • Added these constants for keyword completion: OBJECT_NAME OBJECT_DESC OBJECT_POS OBJECT_ROT OBJECT_VELOCITY OBJECT_OWNER OBJECT_GROUP OBJECT_CREATOR
;; version 1.5.11, 2009-08-27 • if emacs 23, turn on linum-mode.
;; version 1.5.10, 2009-04-28 • Added constants: DATA_BORN DATA_NAME DATA_ONLINE DATA_PAYINFO 
;; version 1.5.9, 2009-04-28 • Added constants: STRING_TRIM STRING_TRIM_HEAD STRING_TRIM_TAIL 
;; version 1.5.8, 2009-04-27 • Added about 13 constants starting with “AGENT_”.
;; version 1.5.7, 2009-04-26 • Modified xlsl-color-vectors-region so that if a vector component is not a number between 0 to 1, no color is applied. Added chars such as < > + - * & etc to syntax table, so that emacs knows better when lsl keyword char stops.
;; version 1.5.6, 2009-04-24 • Added the “jump” lang keyword.
;; version 1.5.5, 2009-04-11 • prettify source code formatting kinda stuff.
;; version 1.5.4, 2009-04-03 • Added: llResetLandBanList llResetLandPassList llSHA1String llDetectedTouchPos llGetRegionAgentCount .
;; version 1.5.3, 2009-04-03 • Removed these: llTakeCamera llXorBase64Strings. • Added: llDetectedTouchBinormal llDetectedTouchFace llDetectedTouchNormal llDetectedTouchST llDetectedTouchUV llGetObjectDetails llGetObjectPrimCount llGetParcelDetails llGetParcelMaxPrims llGetParcelPrimCount llGetParcelPrimOwners llRegionSay llSetClickAction llSetLinkPrimitiveParams llSetLinkTexture llStringTrim
;; version 1.5.1, 2009-04-03 • Added these keywords: llGetAgentLanguage llGetFreeURLs llGetHTTPHeader llHTTPResponse llReleaseURL llRequestSecureURL llRequestURL.
;; version 1.5, 2009-04-01 • Added a customizable variable xlsl-mode-format-style. Its value should be a integer. If 0, then no automatic formating are done. The Tab key insert a tab. If 1, then the formating style emulates that of embedded LSL client (implemented by loading c-mode). If 2, then the formatting style is whatever you have setup for c-mode.
;; version 1.4.0, 2009-03-19. • Added a user function xlsl-color-vectors-region. This will color vector text such as “<1, 0, .5>” using its own value. • Much improved xlsl-convert-rgb. Now it converts text under cursor between #rrggbb and <r, g, b> formats. • Minor fix on xlsl-complete-symbol, so that it wont give lisp error when there's no text before cursor. • Minor implementation cleanup on xlsl-lookup-lsl-ref and xlsl-lookup-lsl-ref2.
;; version 1.3.3, 2009-03-14. Fixed syntax table so that “=” is a punctuation and not “word” or “symbol”. So that, commands like forward-word etc when used on “lengthX= 3;” will stop on the “X” and not “=”.
;; version 1.3.2.1, 2009-03-04. Fix on xlsl-lookup-lsl-ref and xlsl-lookup-lsl-ref2 so that if a region contain a space char, it still works.
;; version 1.3.2, 2009-03-04. Added a second lsl wiki ref lookup: xlsl-lookup-lsl-ref2. One for looking up “lslwiki.net”, and the other for “wiki.secondlife.com”.
;; version 1.3.1, 2009-03-04. Fixed hooks by changing run-hooks to run-mode-hooks.
;; version 1.3.0, 2009-03-03. Added keyword completion feature. xlsl-complete-symbol
;; version 1.2.3, 2009-03-02. Changed the file name from “lsl-mode_Xah_Lee.el” to “xlsl-mode.el”.
;; version 1.2.2, 2009-02-24. Minior inline doc wording tweak.
;; version 1.2.1, 2009-02-20. Fixed a type in hook (run-hooks 'xlsl-mode-hook).
;; version 1.2, 2009-02-17. Added a graphical menu.
;; version 1.1.4, 2009-01-27. No code change. Minor doc change.
;; version 1.1.3, 2008-11-19. Forgot the damn (interactive) in xlsl-mode. Now fixed. Fixed also a typo where “'word” should've been “'words”, which causes parts of user variable got highlighted as keyword. e.g. the “for” in “inform”.
;; version 1.1.1, 2008-11-18. Fixed syntax table for “<”, “>” so that they are not matching pairs.
;; version 1.1, 2008-10-31. Made it call c-mode to support indentation. Added a keybinding for xlsl-convert-rgb.
;; version 1.0, 2008-10-29. First version.

;;; Code:

(require 'thingatpt )

(defvar xlsl-mode-version)
(setq xlsl-mode-version "1.6.1")

(defgroup xlsl-mode nil
  "Major mode for editing Linden Scripting Language."
  :group 'languages)

(defcustom xlsl-lslint-path ""
  "Full path to lslint utility.
The value must be lslint's full path, not its parent dir.
This variable is used by the command `xlsl-syntax-check'.
lslint can be downloaded at http://w-hat.com/lslint as of 2009-03."
  :type '(file :must-match t)
  :group 'xlsl-mode)

(defcustom xlsl-mode-format-style 1
  "Specifies how source code auto formatting is done.
The value should be a integer. If 0, then no automatic formating are done. The Tab key insert a tab. If 1, then the formating style emulates that of embedded LSL client (implemented by loading c-mode). If 2, then the formatting style is whatever you have setup for c-mode."
  :type '(integer)
  :group 'xlsl-mode)

(defcustom xlsl-reference-url "http://lslwiki.net/lslwiki/wakka.php?wakka="
  "URL for LSL reference website.
The value is used by `xlsl-lookup-lsl-ref'.
The value should be one of:
“http://lslwiki.net/lslwiki/wakka.php?wakka=”
“http://wiki.secondlife.com/wiki/”"
  :type '(string)
  :group 'xlsl-mode)

(defcustom xlsl-reference-url2 "http://wiki.secondlife.com/wiki/"
  "URL for LSL reference website.
The value is used by `xlsl-lookup-lsl-ref2'.
The value can be any of:
“http://wiki.secondlife.com/wiki/”
“http://en.wikipedia.org/wiki/”
“http://www.google.com/search?q=”"
  :type '(string)
  :group 'xlsl-mode)

(defvar xlsl-mode-hook nil "Standard hook for xlsl-mode.")

(defvar xlsl-mode-map nil "Keymap for xlsl-mode")

(when (not xlsl-mode-map)
  (setq xlsl-mode-map (make-sparse-keymap))
  (define-key xlsl-mode-map (kbd "C-c C-c") 'xlsl-copy-all)
  (define-key xlsl-mode-map (kbd "C-c C-l") 'xlsl-syntax-check)
  (define-key xlsl-mode-map (kbd "C-c C-r") 'xlsl-lookup-lsl-ref)
  (define-key xlsl-mode-map (kbd "C-c C-f") 'xlsl-lookup-lsl-ref2)
  (define-key xlsl-mode-map (kbd "C-c C-g") 'xlsl-convert-rgb)
  (define-key xlsl-mode-map (kbd "M-TAB") 'xlsl-complete-symbol)
  (define-key xlsl-mode-map [remap comment-dwim] 'xlsl-comment-dwim)

  (define-key xlsl-mode-map [menu-bar] (make-sparse-keymap))

  (let ((menuMap (make-sparse-keymap "LSL")))
    (define-key xlsl-mode-map [menu-bar xlsl] (cons "LSL" menuMap))

    (define-key menuMap [goto-home-page] '("Goto xlsl-mode website" . (lambda () (interactive) (browse-url "http://xahlee.org/sl/ls-emacs.html"))))

    (define-key menuMap [about] '("About xlsl-mode" . xlsl-about))

    (define-key menuMap [customize] '("Customize xlsl-mode" . (lambda () (interactive) (customize-group 'xlsl-mode))))

    (define-key menuMap [separator] '("--"))
    (define-key menuMap [convert-rgb] '("Convert #rrggbb under cursor" . xlsl-convert-rgb))
    (define-key menuMap [color-vectors] '("Color vectors in region" . xlsl-color-vectors-region))
    (define-key menuMap [copy-all] '("Copy whole buffer" . xlsl-copy-all))
    (define-key menuMap [syntax-check] '("Check syntax" . xlsl-syntax-check))
    (define-key menuMap [lookup-onlne-doc2] '("Lookup ref second site" . xlsl-lookup-lsl-ref2))
    (define-key menuMap [lookup-onlne-doc] '("Lookup ref on current word" . xlsl-lookup-lsl-ref))
    (define-key menuMap [keyword-completion] '("Keyword Completion" . xlsl-complete-symbol))))

;;; syntax table
(defvar xlsl-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)

    (modify-syntax-entry ?= "." synTable)
    (modify-syntax-entry ?< "." synTable)
    (modify-syntax-entry ?> "." synTable)
    (modify-syntax-entry ?+ "." synTable)
    (modify-syntax-entry ?- "." synTable)
    (modify-syntax-entry ?* "." synTable)
    (modify-syntax-entry ?& "." synTable)
    (modify-syntax-entry ?| "." synTable)

    synTable)
  "Syntax table for `xlsl-mode'.")

;;; functions

(defun xlsl-about ()
  "Show the author, version number, and description about this package."
  (interactive)
  (with-output-to-temp-buffer "*About xlsl-mode*"
    (princ
     (concat "Mode name: xlsl-mode.\n\n"
             "Author: Xah Lee\n\n"
             "Version: " xlsl-mode-version "\n\n"
             "To see inline documentation, type “Alt+x `describe-mode'” while you are in xlsl-mode.\n\n"
             "Home page: URL `http://xahlee.org/sl/ls-emacs.html' \n\n")
     )
    )
  )

(defun xlsl-copy-all ()
  "Copy buffer content into the kill-ring.
If narrow-to-region is in effect, then just copy that region."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer content copied."))

(defun xlsl-syntax-check ()
  "Validate LSL syntax of the current file.
You need to have lslint installed, and have `xlsl-lslint-path' set."
  (interactive)
  (if (not (= (length xlsl-lslint-path) 0))
      (shell-command (concat xlsl-lslint-path " \"" (buffer-file-name) "\""))
      (message "Error: xlsl-lslint-path not set.")))

;; implementation using “newcomment.el”.
(defun xlsl-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "//") (comment-end ""))
     (comment-dwim arg)))

(defun downcase-word-or-region ()
  "Downcase current word or region."
(interactive)
(let (pos1 pos2 bds)
  (if (and transient-mark-mode
           mark-active)
      (setq pos1 (region-beginning) pos2 (region-end))
    (progn
      (setq bds (bounds-of-thing-at-point 'symbol))
      (setq pos1 (car bds) pos2 (cdr bds))))

  ;; now, pos1 and pos2 are the starting and ending positions of the current word, or current text selection if exist. Sample code:
  (downcase-region pos1 pos2)
))


(defun xlsl-lookup-lsl-site (site-url)
  "Switch to browser to particular url.
SITE-URL is a url string.
This is a internal function.
This function is called by xlsl-lookup-lsl-ref and xlsl-lookup-lsl-ref2."
  (let (pos1 pos2 bds meat myurl)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    (setq meat
          (if (and pos1 pos2 )
              (buffer-substring-no-properties pos1 pos2)
            nil ))

    (if meat
        (progn
          (setq meat (replace-regexp-in-string " " "%20" meat))
          (setq myurl (concat site-url meat))
          (browse-url myurl))
      (progn (ding) (message "No word under cursor to lookup.")))))

(defun xlsl-lookup-lsl-ref ()
  "Look up current word in LSL ref site in a browser.
If there is a text selection (a phrase), lookup that phrase.
Set variable xlsl-reference-url if you want to change the url used.

See also `xlsl-lookup-lsl-ref2'."
  (interactive)
  (xlsl-lookup-lsl-site xlsl-reference-url))

(defun xlsl-lookup-lsl-ref2 ()
  "Look up current word in LSL ref site in a browser.
If there is a text selection (a phrase), lookup that phrase.
Set variable xlsl-reference-url2 if you want to change the url used.

See also `xlsl-lookup-lsl-ref'."
  (interactive)
  (xlsl-lookup-lsl-site xlsl-reference-url2))

(defun xlsl-convert-color-hex-to-vec (hexcolor)
  "Convert HEXCOLOR from “\"rrggbb\"” format to “[r g b]” format.
Example: \"00ffcc\" ⇒ [0.0 1.0 0.8]

Note: The input string must not start with “#”.
If so, the return value is nil."
(when (= 6 (length hexcolor))
  (vector (/ (float (string-to-number (substring hexcolor 0 2) 16)) 255.0)
          (/ (float (string-to-number (substring hexcolor 2 4) 16)) 255.0)
          (/ (float (string-to-number (substring hexcolor 4) 16)) 255.0))))

(defun xlsl-convert-color-vec-to-hex (rgb)
  "Convert color RGB from “[r g b]” format to “\"rrggbb\"” format.
The RGB can be a lisp vector or list.
Example: [0 1 0.5] ⇒ \"00ff80\""
  (mapconcat
   (lambda (x)
     (format "%02x" (round (* x 255.0)) ))
   rgb ""))

(defun xlsl-color-vectors-region (start end)
  "Color all vectors <x,y,z> under region.
The color is chosen from the vector's values, where
x, y, z, are taken to be r, g, b.

This function is useful when you work a lot with colors.
It lets you visually see what color each vector represent.

See also: `xlsl-convert-rgb' and `list-colors-display'."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward-regexp
            "< *\\([0-9.]+\\) *, *\\([0-9.]+\\) *, *\\([0-9.]+\\) *>" nil t)
      (when
          (and
           (<= (string-to-number (match-string 1)) 1.0)
           (<= (string-to-number (match-string 2)) 1.0)
           (<= (string-to-number (match-string 3)) 1.0))
  (put-text-property
   (match-beginning 0)
   (match-end 0) 'font-lock-face
   (list :background
         (concat "#" (xlsl-convert-color-vec-to-hex
                      (vector
                       (string-to-number (match-string 1))
                       (string-to-number (match-string 2))
                       (string-to-number (match-string 3))))))) ) ))) 

(defun xlsl-convert-rgb ()
  "Convert color spec under cursor between “#rrggbb” and “<r,g,b>”.
This command acts on texts near the cursor position.  For
example, if cursor is somewhere on the text “#ff00aa”, it becomes
“<1.0000,0.0000,0.6667>”, and vice versa.

See also `xlsl-color-vectors-region' and `list-colors-display'."
  (interactive)
  (let (pos1 pos2 bds currentWord currentPos p1 p2 currentVec)
    (setq bds (bounds-of-thing-at-point 'word))
    (setq pos1 (car bds) pos2 (cdr bds))

    (if (and pos1 pos2
               (setq currentWord (buffer-substring-no-properties pos1 pos2))
               (string-match "[a-fA-F0-9]\\{6\\}" currentWord))

        ;; the case when current word is of the form #rrggbb
        (progn
          (delete-region pos1 pos2)
          (if (looking-back "#")
              (delete-backward-char 1))
          (insert "<"
                  (mapconcat
                   (lambda (x)
                     (format "%.4f" x))
                   (xlsl-convert-color-hex-to-vec currentWord)
                   ",") ">"))

        ;; the case when current word not the form #rrggbb. Try to grab a vector <...>
      (progn
        (setq currentPos (point))
        (setq p1 (search-backward "<" (- currentPos 35 ) t))
        (setq p2 (search-forward ">" (+ currentPos 35 ) t))
        (if (or (null p1) (null p2))
            (message "Failed to convert color.\nCursor does not appear to be on a color hex #rrggbb or inside a vector <r, g, b>.")
          (progn
            (setq currentVec (buffer-substring-no-properties p1 p2))
            (delete-region p1 p2)
            (insert "#" (xlsl-convert-color-vec-to-hex
                         (mapcar 'string-to-number
                                 (split-string currentVec "[<> ,]+" t))))))))))

;;; font-lock

(defvar xlsl-keywords
  '("break" "default" "do" "else" "for" "if" "return" "state" "while" "jump")
  "LSL keywords.")

(defvar xlsl-types
  '("float" "integer" "key" "list" "rotation" "string" "vector")
  "LSL types.")

(defvar xlsl-constants
'(
"ACTIVE"
"AGENT"
"AGENT_ALWAYS_RUN"
"AGENT_ATTACHMENTS"
"AGENT_AWAY"
"AGENT_BUSY"
"AGENT_BY_LEGACY_NAME"
"AGENT_BY_USERNAME"
"AGENT_CROUCHING"
"AGENT_FLYING"
"AGENT_IN_AIR"
"AGENT_MOUSELOOK"
"AGENT_ON_OBJECT"
"AGENT_SCRIPTED"
"AGENT_SITTING"
"AGENT_TYPING"
"AGENT_WALKING"
"ALL_SIDES"
"ATTACH_BACK"
"ATTACH_CHEST"
"ATTACH_HEAD"
"ATTACH_LFOOT"
"ATTACH_LHAND"
"ATTACH_LSHOULDER"
"ATTACH_RFOOT"
"ATTACH_RHAND"
"ATTACH_RSHOULDER"
"CHANGED_ALLOWED_DROP"
"CHANGED_COLOR"
"CHANGED_INVENTORY"
"CHANGED_LINK"
"CHANGED_OWNER"
"CHANGED_REGION"
"CHANGED_SCALE"
"CHANGED_SHAPE"
"CHANGED_TELEPORT"
"CHANGED_TEXTURE"
"CONTROL_BACK"
"CONTROL_DOWN"
"CONTROL_FWD"
"CONTROL_LBUTTON"
"CONTROL_LEFT"
"CONTROL_ML_LBUTTON"
"CONTROL_RIGHT"
"CONTROL_ROT_LEFT"
"CONTROL_ROT_RIGHT"
"CONTROL_UP"
"DATA_BORN"
"DATA_NAME"
"DATA_ONLINE"
"DATA_PAYINFO"
"DEBUG_CHANNEL"
"DEG_TO_RAD"
"EOF"
"FALSE"
"HTTP_BODY_MAXLENGTH"
"HTTP_BODY_TRUNCATED"
"HTTP_METHOD"
"HTTP_MIMETYPE"
"HTTP_VERIFY_CERT"
"INVENTORY_ALL"
"INVENTORY_ANIMATION"
"INVENTORY_BODYPART"
"INVENTORY_CLOTHING"
"INVENTORY_GESTURE"
"INVENTORY_LANDMARK"
"INVENTORY_NONE"
"INVENTORY_NOTECARD"
"INVENTORY_OBJECT"
"INVENTORY_SCRIPT"
"INVENTORY_SOUND"
"INVENTORY_TEXTURE"
"LAND_LARGE_BRUSH"
"LAND_LEVEL"
"LAND_LOWER"
"LAND_MEDIUM_BRUSH"
"LAND_RAISE"
"LAND_SMALL_BRUSH"
"LINK_ALL_CHILDREN"
"LINK_ALL_OTHERS"
"LINK_ROOT"
"LINK_SET"
"LINK_THIS"
"NULL_KEY"
"OBJECT_CREATOR"
"OBJECT_DESC"
"OBJECT_GROUP"
"OBJECT_NAME"
"OBJECT_OWNER"
"OBJECT_POS"
"OBJECT_ROT"
"OBJECT_RUNNING_SCRIPT_COUNT"
"OBJECT_SCRIPT_MEMORY"
"OBJECT_TOTAL_SCRIPT_COUNT"
"OBJECT_VELOCITY"
"PARCEL_MEDIA_COMMAND_AGENT"
"PARCEL_MEDIA_COMMAND_AUTO_ALIGN"
"PARCEL_MEDIA_COMMAND_LOOP"
"PARCEL_MEDIA_COMMAND_PAUSE"
"PARCEL_MEDIA_COMMAND_PLAY"
"PARCEL_MEDIA_COMMAND_STOP"
"PARCEL_MEDIA_COMMAND_TEXTURE"
"PARCEL_MEDIA_COMMAND_TIME"
"PARCEL_MEDIA_COMMAND_UNLOAD"
"PARCEL_MEDIA_COMMAND_URL"
"PASSIVE"
"PAYMENT_INFO_ON_FILE"
"PAYMENT_INFO_USED"
"PAY_DEFAULT"
"PAY_HIDE"
"PERMISSION_ATTACH"
"PERMISSION_CHANGE_JOINTS"
"PERMISSION_CHANGE_LINKS"
"PERMISSION_CHANGE_PERMISSIONS"
"PERMISSION_CONTROL_CAMERA"
"PERMISSION_DEBIT"
"PERMISSION_RELEASE_OWNERSHIP"
"PERMISSION_REMAP_CONTROLS"
"PERMISSION_TAKE_CONTROLS"
"PERMISSION_TRACK_CAMERA"
"PERMISSION_TRIGGER_ANIMATION"
"PI"
"PI_BY_TWO"
"PRIM_BUMP_BARK"
"PRIM_BUMP_BLOBS"
"PRIM_BUMP_BRICKS"
"PRIM_BUMP_BRIGHT"
"PRIM_BUMP_CHECKER"
"PRIM_BUMP_CONCRETE"
"PRIM_BUMP_DARK"
"PRIM_BUMP_DISKS"
"PRIM_BUMP_GRAVEL"
"PRIM_BUMP_LARGETILE"
"PRIM_BUMP_NONE"
"PRIM_BUMP_SHINY"
"PRIM_BUMP_SIDING"
"PRIM_BUMP_STONE"
"PRIM_BUMP_STUCCO"
"PRIM_BUMP_SUCTION"
"PRIM_BUMP_TILE"
"PRIM_BUMP_WEAVE"
"PRIM_BUMP_WOOD"
"PRIM_COLOR"
"PRIM_GLOW"
"PRIM_HOLE_CIRCLE"
"PRIM_HOLE_DEFAULT"
"PRIM_HOLE_SQUARE"
"PRIM_HOLE_TRIANGLE"
"PRIM_MATERIAL"
"PRIM_MATERIAL_FLESH"
"PRIM_MATERIAL_GLASS"
"PRIM_MATERIAL_LIGHT"
"PRIM_MATERIAL_METAL"
"PRIM_MATERIAL_PLASTIC"
"PRIM_MATERIAL_RUBBER"
"PRIM_MATERIAL_STONE"
"PRIM_MATERIAL_WOOD"
"PRIM_PHANTOM"
"PRIM_PHYSICS"
"PRIM_POSITION"
"PRIM_ROTATION"
"PRIM_SHINY_HIGH"
"PRIM_SHINY_LOW"
"PRIM_SHINY_MEDIUM"
"PRIM_SHINY_NONE"
"PRIM_SIZE"
"PRIM_TEMP_ON_REZ"
"PRIM_TEXTURE"
"PRIM_TYPE"
"PRIM_TYPE"
"PRIM_TYPE_BOX"
"PRIM_TYPE_CYLINDER"
"PRIM_TYPE_PRISM"
"PRIM_TYPE_RING"
"PRIM_TYPE_SPHERE"
"PRIM_TYPE_TORUS"
"PRIM_TYPE_TUBE"
"PSYS_PART_BOUNCE_MASK"
"PSYS_PART_EMISSIVE_MASK"
"PSYS_PART_END_ALPHA"
"PSYS_PART_END_COLOR"
"PSYS_PART_END_SCALE"
"PSYS_PART_FLAGS"
"PSYS_PART_FOLLOW_SRC_MASK"
"PSYS_PART_FOLLOW_VELOCITY_MASK"
"PSYS_PART_INTERP_COLOR_MASK"
"PSYS_PART_INTERP_SCALE_MASK"
"PSYS_PART_MAX_AGE"
"PSYS_PART_START_ALPHA"
"PSYS_PART_START_COLOR"
"PSYS_PART_START_SCALE"
"PSYS_PART_TARGET_LINEAR_MASK"
"PSYS_PART_TARGET_POS_MASK"
"PSYS_PART_WIND_MASK"
"PSYS_SRC_ACCEL"
"PSYS_SRC_ANGLE_BEGIN"
"PSYS_SRC_ANGLE_END"
"PSYS_SRC_BURST_PART_COUNT"
"PSYS_SRC_BURST_RADIUS"
"PSYS_SRC_BURST_RATE"
"PSYS_SRC_BURST_SPEED_MAX"
"PSYS_SRC_BURST_SPEED_MIN"
"PSYS_SRC_MAX_AGE"
"PSYS_SRC_OMEGA"
"PSYS_SRC_PATTERN"
"PSYS_SRC_PATTERN_ANGLE"
"PSYS_SRC_PATTERN_ANGLE_CONE"
"PSYS_SRC_PATTERN_ANGLE_CONE_EMPTY"
"PSYS_SRC_PATTERN_DROP"
"PSYS_SRC_PATTERN_EXPLODE"
"PSYS_SRC_TARGET_KEY"
"PSYS_SRC_TEXTURE"
"PUBLIC_CHANNEL"
"RAD_TO_DEG"
"REMOTE_DATA_CHANNEL"
"REMOTE_DATA_REPLY"
"REMOTE_DATA_REQUEST"
"SCRIPTED"
"SQRT2"
"STATUS_BLOCK_GRAB"
"STATUS_CAST_SHADOWS"
"STATUS_DIE_AT_EDGE"
"STATUS_PHANTOM"
"STATUS_PHYSICS"
"STATUS_RETURN_AT_EDGE"
"STATUS_ROTATE_X"
"STATUS_ROTATE_Y"
"STATUS_ROTATE_Z"
"STATUS_SANDBOX"
"STRING_TRIM"
"STRING_TRIM_HEAD"
"STRING_TRIM_TAIL"
"TRUE"
"TWO_PI"
"TYPE_FLOAT"
"TYPE_INTEGER"
"TYPE_INVALID"
"TYPE_KEY"
"TYPE_QUATERNION"
"TYPE_STRING"
"TYPE_VECTOR"
"ZERO_ROTATION"
"ZERO_VECTOR"
)
  "LSL constants.")

(defvar xlsl-events
  '("at_rot_target" "at_target" "attach" "changed" "collision" "collision_end" "collision_start" "control" "dataserver" "email" "http_response" "land_collision" "land_collision_end" "land_collision_start" "link_message" "listen" "money" "moving_end" "moving_start" "no_sensor" "not_at_rot_target" "not_at_target" "object_rez" "on_rez" "remote_data" "run_time_permissions" "sensor" "state_entry" "state_exit" "timer" "touch" "touch_end" "touch_start")
  "LSL events.")

(defvar xlsl-functions
'(
"llAbs"
"llAcos"
"llAddToLandBanList"
"llAddToLandPassList"
"llAdjustSoundVolume"
"llAllowInventoryDrop"
"llAngleBetween"
"llApplyImpulse"
"llApplyRotationalImpulse"
"llAsin"
"llAtan2"
"llAttachToAvatar"
"llAvatarOnSitTarget"
"llAxes2Rot"
"llAxisAngle2Rot"
"llBase64ToInteger"
"llBase64ToString"
"llBreakAllLinks"
"llBreakLink"
"llCSV2List"
"llCastRay"
"llCeil"
"llClearCameraParams"
"llClearPrimMedia"
"llCloseRemoteDataChannel"
"llCloud"
"llCollisionFilter"
"llCollisionSound"
"llCollisionSprite"
"llCos"
"llCreateLink"
"llDeleteSubList"
"llDeleteSubString"
"llDetachFromAvatar"
"llDetectedGrab"
"llDetectedGroup"
"llDetectedKey"
"llDetectedLinkNumber"
"llDetectedName"
"llDetectedOwner"
"llDetectedPos"
"llDetectedRot"
"llDetectedTouchBinormal"
"llDetectedTouchFace"
"llDetectedTouchNormal"
"llDetectedTouchPos"
"llDetectedTouchST"
"llDetectedTouchUV"
"llDetectedType"
"llDetectedVel"
"llDialog"
"llDie"
"llDumpList2String"
"llEdgeOfWorld"
"llEjectFromLand"
"llEmail"
"llEscapeURL"
"llEuler2Rot"
"llFabs"
"llFloor"
"llForceMouselook"
"llFrand"
"llGetAccel"
"llGetAgentInfo"
"llGetAgentLanguage"
"llGetAgentSize"
"llGetAlpha"
"llGetAndResetTime"
"llGetAnimation"
"llGetAnimationList"
"llGetAttached"
"llGetBoundingBox"
"llGetCameraPos"
"llGetCameraRot"
"llGetCenterOfMass"
"llGetColor"
"llGetCreator"
"llGetDate"
"llGetDisplayName"
"llGetEnergy"
"llGetEnv"
"llGetForce"
"llGetFreeMemory"
"llGetFreeURLs"
"llGetGMTclock"
"llGetGeometricCenter"
"llGetHTTPHeader"
"llGetInventoryCreator"
"llGetInventoryKey"
"llGetInventoryName"
"llGetInventoryNumber"
"llGetInventoryPermMask"
"llGetInventoryType"
"llGetKey"
"llGetLandOwnerAt"
"llGetLinkKey"
"llGetLinkName"
"llGetLinkNumber"
"llGetLinkNumberOfSides"
"llGetLinkPrimitiveParams"
"llGetListEntryType"
"llGetListLength"
"llGetLocalPos"
"llGetLocalRot"
"llGetMass"
"llGetNextEmail"
"llGetNotecardLine"
"llGetNumberOfNotecardLines"
"llGetNumberOfPrims"
"llGetNumberOfSides"
"llGetObjectDesc"
"llGetObjectDetails"
"llGetObjectMass"
"llGetObjectName"
"llGetObjectPermMask"
"llGetObjectPrimCount"
"llGetOmega"
"llGetOwner"
"llGetOwnerKey"
"llGetParcelDetails"
"llGetParcelFlags"
"llGetParcelMaxPrims"
"llGetParcelPrimCount"
"llGetParcelPrimOwners"
"llGetPermissions"
"llGetPermissionsKey"
"llGetPos"
"llGetPrimMediaParams"
"llGetPrimitiveParams"
"llGetRegionAgentCount"
"llGetRegionCorner"
"llGetRegionFPS"
"llGetRegionFlags"
"llGetRegionName"
"llGetRegionTimeDilation"
"llGetRootPosition"
"llGetRootRotation"
"llGetRot"
"llGetSPMaxMemory"
"llGetScale"
"llGetScriptName"
"llGetScriptState"
"llGetSimulatorHostname"
"llGetStartParameter"
"llGetStatus"
"llGetSubString"
"llGetSunDirection"
"llGetTexture"
"llGetTextureOffset"
"llGetTextureRot"
"llGetTextureScale"
"llGetTime"
"llGetTimeOfDay"
"llGetTimestamp"
"llGetTorque"
"llGetUnixTime"
"llGetUsedMemory"
"llGetUsername"
"llGetVel"
"llGetWallclock"
"llGiveInventory"
"llGiveInventoryList"
"llGiveMoney"
"llGround"
"llGroundContour"
"llGroundNormal"
"llGroundRepel"
"llGroundSlope"
"llHTTPRequest"
"llHTTPResponse"
"llInsertString"
"llInstantMessage"
"llIntegerToBase64"
"llKey2Name"
"llLinkParticleSystem"
"llLinks"
"llList2CSV"
"llList2Float"
"llList2Integer"
"llList2Key"
"llList2List"
"llList2ListStrided"
"llList2Rot"
"llList2String"
"llList2Vector"
"llListFindList"
"llListInsertList"
"llListRandomize"
"llListReplaceList"
"llListSort"
"llListStatistics"
"llListen"
"llListenControl"
"llListenRemove"
"llLoadURL"
"llLog"
"llLog10"
"llLookAt"
"llLoopSound"
"llLoopSoundMaster"
"llLoopSoundSlave"
"llMD5String"
"llMapDestination"
"llMessageLinked"
"llMinEventDelay"
"llModPow"
"llModifyLand"
"llMoveToTarget"
"llOffsetTexture"
"llOpenRemoteDataChannel"
"llOverMyLand"
"llOwnerSay"
"llParcelMediaCommandList"
"llParcelMediaQuery"
"llParseString2List"
"llParseStringKeepNulls"
"llParticleSystem"
"llPassCollisions"
"llPassTouches"
"llPlaySound"
"llPlaySoundSlave"
"llPointAt"
"llPow"
"llPreloadSound"
"llPushObject"
"llRefreshPrimURL"
"llRegionSay"
"llRegionSay"
"llRegionSayTo"
"llReleaseCamera"
"llReleaseControls"
"llReleaseURL"
"llRemoteDataReply"
"llRemoteDataSetRegion"
"llRemoteLoadScriptPin"
"llRemoveFromLandBanList"
"llRemoveFromLandPassList"
"llRemoveInventory"
"llRemoveVehicleFlags"
"llRequestAgentData"
"llRequestDisplayName"
"llRequestInventoryData"
"llRequestPermissions"
"llRequestSecureURL"
"llRequestSimulatorData"
"llRequestURL"
"llRequestUsername"
"llResetLandBanList"
"llResetLandPassList"
"llResetOtherScript"
"llResetScript"
"llResetTime"
"llRezAtRoot"
"llRezObject"
"llRot2Angle"
"llRot2Axis"
"llRot2Euler"
"llRot2Fwd"
"llRot2Left"
"llRot2Up"
"llRotBetween"
"llRotLookAt"
"llRotTarget"
"llRotTargetRemove"
"llRotateTexture"
"llRound"
"llSHA1String"
"llSameGroup"
"llSay"
"llScaleTexture"
"llScriptDanger"
"llScriptProfiler"
"llSendRemoteData"
"llSensor"
"llSensorRemove"
"llSensorRepeat"
"llSetAlpha"
"llSetBuoyancy"
"llSetCameraAtOffset"
"llSetCameraEyeOffset"
"llSetCameraParams"
"llSetClickAction"
"llSetColor"
"llSetDamage"
"llSetForce"
"llSetForceAndTorque"
"llSetHoverHeight"
"llSetLinkAlpha"
"llSetLinkColor"
"llSetLinkPrimitiveParams"
"llSetLinkPrimitiveParamsFast"
"llSetLinkTexture"
"llSetLinkTextureAnim"
"llSetLocalRot"
"llSetObjectDesc"
"llSetObjectName"
"llSetParcelMusicURL"
"llSetPayPrice"
"llSetPos"
"llSetPrimMediaParams"
"llSetPrimURL"
"llSetPrimitiveParams"
"llSetRemoteScriptAccessPin"
"llSetRot"
"llSetScale"
"llSetScriptState"
"llSetSitText"
"llSetSoundQueueing"
"llSetSoundRadius"
"llSetStatus"
"llSetText"
"llSetTexture"
"llSetTextureAnim"
"llSetTimerEvent"
"llSetTorque"
"llSetTouchText"
"llSetVehicleFlags"
"llSetVehicleFloatParam"
"llSetVehicleRotationParam"
"llSetVehicleType"
"llSetVehicleVectorParam"
"llShout"
"llSin"
"llSitTarget"
"llSleep"
"llSqrt"
"llStartAnimation"
"llStopAnimation"
"llStopHover"
"llStopLookAt"
"llStopMoveToTarget"
"llStopPointAt"
"llStopSound"
"llStringLength"
"llStringToBase64"
"llStringTrim"
"llSubStringIndex"
"llTakeControls"
"llTan"
"llTarget"
"llTargetOmega"
"llTargetRemove"
"llTeleportAgentHome"
"llTextBox"
"llToLower"
"llToUpper"
"llTriggerSound"
"llTriggerSoundLimited"
"llUnSit"
"llUnescapeURL"
"llVecDist"
"llVecMag"
"llVecNorm"
"llVolumeDetect"
"llWater"
"llWhisper"
"llWind"
"llXorBase64StringsCorrect"
)  
  "LSL functions.")

(defvar xlsl-keywords-regexp (regexp-opt xlsl-keywords 'words))
(defvar xlsl-type-regexp (regexp-opt xlsl-types 'words))
(defvar xlsl-constant-regexp (regexp-opt xlsl-constants 'words))
(defvar xlsl-event-regexp (regexp-opt xlsl-events 'words))
(defvar xlsl-functions-regexp (regexp-opt xlsl-functions 'words))

(setq xlsl-font-lock-keywords
  `(
    (,xlsl-type-regexp . font-lock-type-face)
    (,xlsl-constant-regexp . font-lock-constant-face)
    (,xlsl-event-regexp . font-lock-builtin-face)
    (,xlsl-functions-regexp . font-lock-function-name-face)
    (,xlsl-keywords-regexp . font-lock-keyword-face)
;; note: order above matters. Keywords goes last here because, for example, otherwise the keyword “state” in the function “state_entry” would be highlighted.
))

;; keyword completion
(defvar xlsl-kwdList nil "LSL keywords.")

(setq xlsl-kwdList (make-hash-table :test 'equal))
(mapc (lambda (x) (puthash x t xlsl-kwdList)) xlsl-keywords)
(mapc (lambda (x) (puthash x t xlsl-kwdList)) xlsl-types)
(mapc (lambda (x) (puthash x t xlsl-kwdList)) xlsl-constants)
(mapc (lambda (x) (puthash x t xlsl-kwdList)) xlsl-events)
(mapc (lambda (x) (puthash x t xlsl-kwdList)) xlsl-functions)
(put 'xlsl-kwdList 'risky-local-variable t)

(defun xlsl-complete-symbol ()
  "Perform keyword completion on word before cursor.
Keywords include all LSL's event handlers, functions, and CONSTANTS."
  (interactive)
  (let ((posEnd (point))
         (meat (thing-at-point 'symbol))
         maxMatchResult)

    (when (not meat) (setq meat ""))

    (setq maxMatchResult (try-completion meat xlsl-kwdList))
    (cond ((eq maxMatchResult t))
          ((null maxMatchResult)
           (message "Can't find completion for “%s”" meat)
           (ding))
          ((not (string= meat maxMatchResult))
           (delete-region (- posEnd (length meat)) posEnd)
           (insert maxMatchResult))
          (t (message "Making completion list...")
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list
                (all-completions meat xlsl-kwdList)
                meat))
             (message "Making completion list...%s" "done")))))

;; clear memory
(setq xlsl-keywords nil)
(setq xlsl-types nil)
(setq xlsl-constants nil)
(setq xlsl-events nil)
(setq xlsl-functions nil)

(defun xlsl-mode ()
;; (define-derived-mode xlsl-mode c-mode "LSL"
  "Major mode for editing LSL (Linden Scripting Language).

Shortcuts             Command Name
\\[comment-dwim]       `comment-dwim'

\\[xlsl-complete-symbol]      `xlsl-complete-symbol'

\\[xlsl-lookup-lsl-ref]     `xlsl-lookup-lsl-ref'

\\[xlsl-lookup-lsl-ref2]    `xlsl-lookup-lsl-ref2'

\\[xlsl-syntax-check]    `xlsl-syntax-check'

\\[xlsl-convert-rgb]     `xlsl-convert-rgb'

\\[xlsl-color-vectors-region]     `xlsl-color-vectors-region'

\\[xlsl-copy-all]          `xlsl-copy-all'

Complete documentation at URL `http://xahlee.org/sl/ls-emacs.html'."
  (interactive)
  (kill-all-local-variables)

  (cond
   ((= xlsl-mode-format-style 0) nil)
   ((= xlsl-mode-format-style 1) (progn
                                   ;; borrow c's indentation
                                   (c-mode)
                                   (setq indent-tabs-mode nil)
                                   (setq c-basic-offset 4)
                                   (c-set-offset 'substatement-open 0)
                                   (c-set-offset 'block-open 0)
                                   (c-set-offset 'statement-cont 0)))
   ((= xlsl-mode-format-style 2) (c-mode)))

  (setq major-mode 'xlsl-mode)
  (setq mode-name "LSL")
  (set-syntax-table xlsl-mode-syntax-table)
  (use-local-map xlsl-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((xlsl-font-lock-keywords) nil nil))

  ;; clear memory
  (setq xlsl-keywords-regexp nil)
  (setq xlsl-types-regexp nil)
  (setq xlsl-constants-regexp nil)
  (setq xlsl-events-regexp nil)
  (setq xlsl-functions-regexp nil)

  ;; if emacs 23, turn on linum-mode
  (when
      (and
       (fboundp 'linum-mode)
       (>= emacs-major-version 23)
       (>= emacs-minor-version 1)
       )
    ;; (setq linum-format #'(lambda (n) (format "%3d" (1- n)))) ;; make it start at 0 to be compatible with inworld editor. Prob with this is that it sticks. Need to revert it back when out of xlsl-mode.
    (linum-mode 1)
    )

  (run-mode-hooks 'xlsl-mode-hook))

(provide 'xlsl-mode)
