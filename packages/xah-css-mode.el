;;; xah-css-mode.el --- Major mode for editing CSS code. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-04-18
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; Commentary:
;; Major mode for editing CSS code. Alpha stage.

;;; HISTORY

;; version 0.3, 2013-04-30 added xcm-hex-color-to-hsl, and other improvements
;; version 0.2, 2013-04-22 added xcm-compact-css-region
;; version 0.1, 2013-04-18 first version

(require 'xfrp_find_replace_pairs)
;(require 'xeu_elisp_util)
(require 'color) ; part of emacs 24.3

(defvar xah-css-mode-hook nil "Standard hook for `xah-css-mode'")



(defun xcm-insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
Example output: hsl(100,24%,82%);"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%);" (random 360) (random 100) (random 100))) )

(defun xcm-hex-color-to-hsl ()
  "Convert color spec under cursor from “#rrggbb” to CSS HSL format.
 ⁖ #ffefd5 → hsl(37,100%,91%)
"
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'word))
         (p1 (car bds))
         (p2 (cdr bds))
         (currentWord (buffer-substring-no-properties p1 p2)))

    (if (string-match "[a-fA-F0-9]\\{6\\}" currentWord)
        (progn 
          (delete-region p1 p2 )
          (if (looking-back "#") (delete-char -1))
          (insert (xcm-hex-to-hsl-color currentWord )))
      (progn
        (error "The current word 「%s」 is not of the form #rrggbb." currentWord)
        )
      )))

(defun xcm-hex-to-hsl-color (hexStr)
  "Convert hexStr color to CSS HSL format.
Return a string.
 ⁖ #ffefd5 → hsl(37,100%,91%)
"
  (let* (
         (colorVec (xcm-convert-color-hex-to-vec hexStr))
         (xR (elt colorVec 0))
         (xG (elt colorVec 1))
         (xB (elt colorVec 2))
         (hsl (color-rgb-to-hsl xR xG xB) )
         (xH (elt hsl 0))
         (xS (elt hsl 1))
         (xL (elt hsl 2))
         )
    (format "hsl(%d,%d%%,%d%%)" (* xH 360) (* xS 100) (* xL 100) )
    ))

;(xcm-convert-color-hex-to-vec "aabbcc")

(defun xcm-convert-color-hex-to-vec (hexcolor)
  "Convert HEXCOLOR from “\"rrggbb\"” string to a elisp vector [r g b], where the values are from 0 to 1.
Example: \"00ffcc\" ⇒ [0.0 1.0 0.8]

Note: The input string must not start with “#”. If so, the return value is nil."
(when (= 6 (length hexcolor))
  (vector (/ (float (string-to-number (substring hexcolor 0 2) 16)) 255.0)
          (/ (float (string-to-number (substring hexcolor 2 4) 16)) 255.0)
          (/ (float (string-to-number (substring hexcolor 4) 16)) 255.0))))


;;; functions

(defun xcm-compact-css-region (p1 p2)
  "Remove unnecessary whitespaces of CSS source code in region.
WARNING: not robust."
  (interactive "r")
  (progn
    (save-restriction
      (narrow-to-region p1 p2)
      (replace-regexp-pairs-region (point-min) (point-max) '(["  +" " "]))
      (replace-pairs-region (point-min) (point-max)
                            '(
                              ["\n" ""]
                              [" /* " "/*"]
                              [" */ " "*/"]
                              [" {" "{"]
                              ["{ " "{"]
                              ["; " ";"]
                              [": " ":"]

                              [";}" "}"]
                              ["}" "}\n"]
                              )) ) ) )


(defvar xcm-html-tag-names nil "a list of HTML5 tag names.")
(setq xcm-html-tag-names
'("a" "abbr" "address" "applet" "area" "article" "aside" "audio" "b" "base" "basefont" "bdi" "bdo" "blockquote" "body" "br" "button" "canvas" "caption" "cite" "code" "col" "colgroup" "command" "datalist" "dd" "del" "details" "dfn" "div" "dl" "doctype" "dt" "em" "embed" "fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins" "kbd" "keygen" "label" "legend" "li" "link" "map" "mark" "menu" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span" "strong" "style" "sub" "summary" "sup" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "u" "ul" "var" "video" "wbr")
 )

(defvar xcm-color-names nil "a list of CSS color names.")
(setq xcm-color-names
'("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet" "brown" "burlywood" "cadetblue" "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk" "crimson" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray" "darkgreen" "darkgrey" "darkkhaki" "darkmagenta" "darkolivegreen" "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen" "darkslateblue" "darkslategray" "darkslategrey" "darkturquoise" "darkviolet" "deeppink" "deepskyblue" "dimgray" "dimgrey" "dodgerblue" "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro" "ghostwhite" "gold" "goldenrod" "gray" "green" "greenyellow" "grey" "honeydew" "hotpink" "indianred" "indigo" "ivory" "khaki" "lavender" "lavenderblush" "lawngreen" "lemonchiffon" "lightblue" "lightcoral" "lightcyan" "lightgoldenrodyellow" "lightgray" "lightgreen" "lightgrey" "lightpink" "lightsalmon" "lightseagreen" "lightskyblue" "lightslategray" "lightslategrey" "lightsteelblue" "lightyellow" "lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise" "mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin" "navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange" "orangered" "orchid" "palegoldenrod" "palegreen" "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue" "purple" "red" "rosybrown" "royalblue" "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell" "sienna" "silver" "skyblue" "slateblue" "slategray" "slategrey" "snow" "springgreen" "steelblue" "tan" "teal" "thistle" "tomato" "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow" "yellowgreen")
 )

(defvar xcm-property-names nil "a list of CSS property names.")
(setq xcm-property-names
'(

;:hover
;:after
;:before
;:visited
;:link
;@media

"background" "background-color" "background-image" "background-position" "background-repeat" "border" "border-bottom" "border-collapse" "border-color" "border-left" "border-radius" "border-top" "box-shadow" "clear" "color" "content" "cursor" "direction" "display" "filter" "float" "font-family" "font-size" "font-style" "font-weight" "height" "line-height" "list-style" "list-style-image" "list-style-type" "margin" "margin-bottom" "margin-left" "margin-right" "margin-top" "max-width" "min-width" "opacity" "orphans" "overflow" "padding" "padding-left" "padding-right" "padding-top" "page-break-after" "page-break-inside" "position" "pre-wrap" "table" "table-cell" "text-align" "text-decoration" "unicode-bidi" "vertical-align" "white-space" "widows" "width" "word-wrap" "z-index"

) )

(defvar xcm-unit-names nil "a list of CSS unite names.")
(setq xcm-unit-names '("px" "pt" "pc" "cm" "mm" "in" "em" "ex" "%") )

(defvar xcm-value-kwds nil "a list of CSS value names")
(setq xcm-value-kwds
'(

"!important" "absolute" "alpha" "auto" "avoid" "block" "bold" "both" "bottom" "break-word" "center" "collapse" "dashed" "dotted" "embed" "fixed" "help" "hidden" "hsl" "hsla" "inherit" "inline" "inline-block" "italic" "large" "left" "ltr" "middle" "monospace" "no-repeat" "none" "normal" "nowrap" "pointer" "relative" "rgb" "rgba" "right" "rtl" "sans-serif" "serif" "small" "smaller" "solid" "square" "static" "thin" "top" "transparent" "underline" "url" "x-large" "xx-large"

) )


;; syntax coloring related

(setq xcm-font-lock-keywords
      (let (
          (cssPropertieNames (regexp-opt xcm-property-names ) )
          (cssValueNames (regexp-opt xcm-value-kwds ) )
          (cssColorNames (regexp-opt xcm-color-names 'words) )
          (htmlTagNames (regexp-opt xcm-html-tag-names 'words) )
          (cssUnitNames (regexp-opt xcm-unit-names ) )
          )
        `(
          (,cssPropertieNames . font-lock-type-face)
          (,cssValueNames . font-lock-keyword-face)
          (,cssColorNames . font-lock-constant-face)
          (,htmlTagNames . font-lock-function-name-face)
          (,cssUnitNames . font-lock-builtin-face)
          ("'[^']+'" . font-lock-string-face)
          ) ) )

(defvar xcm-colorfy-hex
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(font-lock-add-keywords 'xah-css-mode xcm-colorfy-hex)


;; keybinding

(defvar xcm-keymap nil "Keybinding for `xah-css-mode'")
(progn
  (setq xcm-keymap (make-sparse-keymap))
;  (define-key xcm-keymap [remap comment-dwim] 'xcm-comment-dwim)
)


;; syntax table
(defvar xcm-syntax-table nil "Syntax table for `xah-css-mode'.")
(setq xcm-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 14" synTable) ; /* java style comment*/
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))



;; define the mode
(define-derived-mode xah-css-mode fundamental-mode
  "ξCSS "
  "A major mode for CSS.

CSS keywords are colored. Basically that's it.

\\{xcm-keymap}"
  (setq font-lock-defaults '((xcm-font-lock-keywords)))

  (set-syntax-table xcm-syntax-table)

  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-start-skip) "/\\*+[ \t]*")
  (set (make-local-variable 'comment-end) "*/")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\*+/")

  (use-local-map xcm-keymap)
  (run-mode-hooks 'xah-css-mode-hook)
)

(provide 'xah-css-mode)
