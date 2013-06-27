;;; xah-html-mode.el --- Major mode for editing pure html5. -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2012-05-12
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; Commentary:
;; Major mode for editing pure HTML5 files.
;; home page: http://ergoemacs.org/emacs/xah-html-mode.html
;; beta stage. Mostly just used by me. There are about 20 functions that act on HTML. They have inline doc. But don't have keys. No over-all doc.

;;; HISTORY

;; 0.6.6, 2013-06-20 critical bug-fix on xhm-htmlize-or-de-precode. Before, it'll just remove html entities for & < >.
;; 0.6.5, 2013-05-10 improved on “xhm-make-citation”
;; 0.6.4, 2013-04-29 added xhm-change-current-tag
;; 0.6.3, 2013-04-23 now xhm-wrap-html-tag will smartly decide to wrap tag around word or line or text block, depending on the tag given, when there's no text selection.
;; 0.6.2, 2013-04-22 now, ‘single curly quoted text’ also colored.
;; 0.6.1, 2013-04-21 added xhm-pre-source-code.
;; 0.6.0, 2013-04-17 added feature to htmlize <pre> code block. ⁖ xhm-htmlize-or-de-precode and xhm-get-precode-make-new-file. The function names may change in the future.
;; 0.5.9, 2013-04-10 added “xhm-emacs-to-windows-kbd-notation” and improved “xhm-htmlize-keyboard-shortcut-notation” to take emacs notation.
;; 0.5.8, 2013-03-19 now xhm-extract-url will also put result to kill-ring
;; 0.5.7, 2013-03-03 removed the id option in xhm-wrap-html-tag
;; 0.5.6, 2013-02-16 added xhm-replace-html-named-entities
;; 0.5.5, 2013-02-03 added xhm-replace-html-&<>-to-entities, xhm-replace-html-chars-to-unicode
;; 0.5.4, 2013-01-26 lots additions and changes. added xhm-wrap-html-tag xhm-wrap-p-tag xhm-lines-to-html-list xhm-make-html-table xhm-wikipedia-linkify xhm-wrap-url xhm-wikipedia-url-linkify xhm-source-url-linkify xhm-make-link-defunct xhm-make-citation xhm-update-title xhm-extract-url xhm-remove-html-tags xhm-remove-span-tag-region xhm-htmlize-keyboard-shortcut-notation
;; 0.5.3, 2012-12-07 removed loading sgml-mode and all call to its functions. The sgml-mode seems to have bugs about keys. That is, global numberpad keys won't work.
;; 0.5.2, 2012-09-25 added a color for curly quoted text.
;; 0.5, 2012-05-13 fixed sgml-skip-tag-forward sgml-skip-tag-backward. But sgml-delete-tag still doesn't work.
;; 0.4, 2012-05-13 added sgml-delete-tag sgml-skip-tag-forward sgml-skip-tag-backward.
;; 0.3, 2012-05-13 added comment handling. improved syntax coloring. Added keymap and syntax table."
;; 0.2, 2012-05-12 first version

(require 'ido)
(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)
(require 'sgml-mode)
(require 'htmlize)
(require 'hi-lock) ; uses its face definitions

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")

(defcustom xhm-html5-tag-names nil
  "alist of HTML5 tag names. The value is a vector of one element. w means word, l means line, b means block, others are unknown. They indicate the default ways to wrap the tag around cursor. "
; todo: need to go the the list and look at the type carefully. Right now it's just quickly done. lots are “z”, for unkown. Also, some are self closing tags, current has mark of “n”.
:group 'xah-html-mode
)
(setq xhm-html5-tag-names
'(

("a" . ["l"])
("abbr" . ["w"])
("address" . ["w"])
("applet" . ["l"])
("area" . ["l"])
("article" . ["b"])
("aside" . ["b"])
("audio" . ["l"])
("b" . ["w"])
("base" . ["l"])
("basefont" . ["l"])
("bdi" . ["w"])
("bdo" . ["w"])
("blockquote" . ["b"])
("body" . ["b"])
("br" . ["n"])
("button" . ["w"])
("canvas" . ["b"])
("caption" . ["l"])
("cite" . ["l"])
("code" . ["l"])
("col" . ["n"])
("colgroup" . ["l"])
("command" . ["z"])
("datalist" . ["z"])
("dd" . ["z"])
("del" . ["z"])
("details" . ["z"])
("dfn" . ["z"])
("div" . ["b"])
("dl" . ["l"])
("doctype" . ["l"])
("dt" . ["l"])
("em" . ["w"])
("embed" . ["l"])
("fieldset" . ["z"])
("figcaption" . ["l"])
("figure" . ["b"])
("footer" . ["b"])
("form" . ["l"])
("h1" . ["l"])
("h2" . ["l"])
("h3" . ["l"])
("h4" . ["l"])
("h5" . ["l"])
("h6" . ["l"])
("head" . ["b"])
("header" . ["z"])
("hgroup" . ["z"])
("hr" . ["n"])
("html" . ["b"])
("i" . ["w"])
("iframe" . ["z"])
("img" . ["l"])
("input" . ["l"])
("ins" . ["z"])
("kbd" . ["w"])
("keygen" . ["z"])
("label" . ["z"])
("legend" . ["z"])
("li" . ["l"])
("link" . ["z"])
("map" . ["z"])
("mark" . ["w"])
("menu" . ["z"])
("meta" . ["z"])
("meter" . ["z"])
("nav" . ["b"])
("noscript" . ["l"])
("object" . ["z"])
("ol" . ["b"])
("optgroup" . ["z"])
("option" . ["z"])
("output" . ["z"])
("p" . ["z"])
("param" . ["z"])
("pre" . ["z"])
("progress" . ["z"])
("q" . ["z"])
("rp" . ["z"])
("rt" . ["z"])
("ruby" . ["z"])
("s" . ["w"])
("samp" . ["z"])
("script" . ["z"])
("section" . ["b"])
("select" . ["z"])
("small" . ["z"])
("source" . ["z"])
("span" . ["w"])
("strong" . ["w"])
("style" . ["z"])
("sub" . ["z"])
("summary" . ["z"])
("sup" . ["z"])
("table" . ["z"])
("tbody" . ["z"])
("td" . ["z"])
("textarea" . ["z"])
("tfoot" . ["z"])
("th" . ["z"])
("thead" . ["z"])
("time" . ["w"])
("title" . ["z"])
("tr" . ["z"])
("u" . ["z"])
("ul" . ["b"])
("var" . ["w"])
("video" . ["z"])
("wbr" . ["z"])

)
 )

(defvar xhm-html5-tag-list nil "list version of `xhm-html5-tag-names'")
(setq xhm-html5-tag-list (mapcar (lambda (x) (car x)) xhm-html5-tag-names))

(defcustom xhm-attribute-names nil
  "HTML5 attribute names."
:group 'xah-html-mode)
(setq xhm-attribute-names '( "id" "class" "style" "title" "href" "type" "rel" "http-equiv" "content" "charset" "alt" "src" "width" "height" "controls" "autoplay" "preload" "name" "value" "size" ))

(defcustom xhm-html5-self-close-tags nil
  "a list of HTML5 self-closing tag name. "
:group 'xah-html-mode )
(setq xhm-html5-self-close-tags
'(
"area"
"base"
"br"
"col"
"command"
"embed"
"hr"
"img"
"input"
"keygen"
"link"
"meta"
"param"
"source"
"track"
"wbr"
)
 )

(defvar xhm-css-color-names nil "a list of CSS color names.")
(setq xhm-css-color-names
'("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet" "brown" "burlywood" "cadetblue" "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk" "crimson" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray" "darkgreen" "darkgrey" "darkkhaki" "darkmagenta" "darkolivegreen" "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen" "darkslateblue" "darkslategray" "darkslategrey" "darkturquoise" "darkviolet" "deeppink" "deepskyblue" "dimgray" "dimgrey" "dodgerblue" "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro" "ghostwhite" "gold" "goldenrod" "gray" "green" "greenyellow" "grey" "honeydew" "hotpink" "indianred" "indigo" "ivory" "khaki" "lavender" "lavenderblush" "lawngreen" "lemonchiffon" "lightblue" "lightcoral" "lightcyan" "lightgoldenrodyellow" "lightgray" "lightgreen" "lightgrey" "lightpink" "lightsalmon" "lightseagreen" "lightskyblue" "lightslategray" "lightslategrey" "lightsteelblue" "lightyellow" "lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise" "mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin" "navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange" "orangered" "orchid" "palegoldenrod" "palegreen" "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue" "purple" "red" "rosybrown" "royalblue" "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell" "sienna" "silver" "skyblue" "slateblue" "slategray" "slategrey" "snow" "springgreen" "steelblue" "tan" "teal" "thistle" "tomato" "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow" "yellowgreen")
 )

(defvar xhm-css-property-names nil "a list of CSS property names.")
(setq xhm-css-property-names
'(
"background" "background-color" "background-image" "background-position" "background-repeat" "border" "border-bottom" "border-collapse" "border-color" "border-left" "border-radius" "border-top" "box-shadow" "clear" "color" "content" "cursor" "direction" "display" "filter" "float" "font-family" "font-size" "font-style" "font-weight" "height" "line-height" "list-style" "list-style-image" "list-style-type" "margin" "margin-bottom" "margin-left" "margin-right" "margin-top" "max-width" "min-width" "opacity" "orphans" "overflow" "padding" "padding-left" "padding-right" "padding-top" "page-break-after" "page-break-inside" "position" "pre-wrap" "table" "table-cell" "text-align" "text-decoration" "unicode-bidi" "vertical-align" "white-space" "widows" "width" "word-wrap" "z-index"
) )

(defvar xhm-css-unit-names nil "a list of CSS unite names.")
(setq xhm-css-unit-names '("px" "pt" "pc" "cm" "mm" "in" "em" "ex" "%") )

(defvar xhm-css-value-kwds nil "a list of CSS value names")
(setq xhm-css-value-kwds
'(
"!important" "absolute" "alpha" "auto" "avoid" "block" "bold" "both" "bottom" "break-word" "center" "collapse" "dashed" "dotted" "embed" "fixed" "help" "hidden" "hsl" "hsla" "inherit" "inline" "inline-block" "italic" "large" "left" "ltr" "middle" "monospace" "no-repeat" "none" "normal" "nowrap" "pointer" "relative" "rgb" "rgba" "right" "rtl" "sans-serif" "serif" "small" "smaller" "solid" "square" "static" "thin" "top" "transparent" "underline" "url" "x-large" "xx-large"
) )



(defun xhm-get-tag-type (tagName)
  "Return the wrap-type info of tagName in `xhm-html5-tag-names'"
  (elt
   (cdr
    (assoc tagName xhm-html5-tag-names)
    ) 0))

(defvar xhm-lang-name-map nil "a alist that maps lang name. Each element has this form 「(‹lang code› . [‹emacs major mode name› ‹file_extension›])」")
(setq xhm-lang-name-map
'(
           ("ahk" . ["ahk-mode" "ahk"])

           ("code" . ["fundamental-mode" "txt"])
           ("output" . ["fundamental-mode" "txt"])

           ("bash" . ["sh-mode" "sh"])
           ("bash-output" . ["fundamental-mode" "txt"])
           ("unix-config" . ["conf-space-mode" "conf"])
           ("cmd" . ["dos-mode" "bat"])

           ("bbcode" . ["xbbcode-mode" "bbcode"])
           ("c" . ["c-mode" "c"])
           ("cpp" . ["c++-mode" "cpp"])
           ("cl" . ["lisp-mode" "lisp"])

           ("org-mode" . ["org-mode" "org"])

           ("clojure" . ["clojure-mode" "clj"])
           ("css" . ["css-mode" "css"])
           ("elisp" . ["emacs-lisp-mode" "el"])
           ("emacs-lisp" . ["xah-elisp-mode" "el"])
           ("haskell" . ["haskell-mode" "hs"])
           ("html" . ["html-mode" "html"])
           ("mysql" . ["sql-mode" "sql"])
           ("xml" . ["sgml-mode"])
           ("html6" . ["xah-html6-mode" "html6"])
           ("java" . ["java-mode" "java"])
           ("js" . ["js-mode" "js"])
           ("lsl" . ["xlsl-mode" "lsl"])
           ("ocaml" . ["tuareg-mode" "ocaml"])
           ("org" . ["org-mode" "org"])
           ("perl" . ["cperl-mode" "pl"])
           ("php" . ["php-mode" "php"])
           ("povray" . ["pov-mode" "pov"])
           ("powershell" . ["powershell-mode" "ps1"])
           ("python" . ["python-mode" "py"])
           ("python3" . ["python-mode" "py3"])
           ("qi" . ["shen-mode" "qi"])
           ("ruby" . ["ruby-mode" "rb"])
           ("scala" . ["scala-mode" "scala"])
           ("scheme" . ["scheme-mode" "scm"])
           ("yasnippet" . ["snippet-mode" "yasnippet"])
           ("vbs" . ["visual-basic-mode" "vbs"])
           ("visualbasic" . ["visual-basic-mode" "vbs"])
           ("mma" . ["fundamental-mode" "m"])

           ("slim" . ["slim-mode" "slim"])
           ("yaml" . ["yaml-mode" "yaml"])

           ) )

;(defvar xhm-lang-name-list nil "a alist that maps lang name. Each element has this form 「(‹lang code› . [‹emacs major mode name› ‹file_extension›])」")
; (mapcar (lambda (x) (car x)) xhm-lang-name-map)

(defun xhm-precode-htmlized-p (p1 p2)
  "Return true if region p1 p2 is htmlized code.
WARNING: it just losely check if it contains span tag."
  (progn
    (goto-char p1)
    (re-search-forward "<span class=" p2 "NOERROR")
    ))

(defun xhm-get-precode-langCode ()
  "Get the langCode and position boundary of current HTML pre block.
A pre block is text of this form
 <pre class=\"‹langCode›\">…▮…</pre>.
Your cursor must be between the tags.

Returns a vector [langCode pos1 pos2], where pos1 pos2 are the boundary of the text content."
  (interactive)
  (let (langCode p1 p2)
    (if (region-active-p)
        (progn
          (setq p1 (region-beginning) )
          (setq p2 (region-end) )
          ;; (setq langCode (read-string "langcode:"))
          (setq langCode (ido-completing-read "langcode:" (mapcar (lambda (x) (car x)) xhm-lang-name-map) ))
          (vector langCode p1 p2)
          )

;;       (progn
;; (save-excursion
;;         (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
;;         (setq langCode (match-string 1))
;;         (setq p1 (search-forward ">"))    ; text content begin
;;         (search-forward "</pre>")
;;         (setq p2 (search-backward "<"))   ; text content end
;;         (vector langCode p1 p2)
;;  ))

      (progn
        (save-excursion
          (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
          (setq langCode (match-string 1))
          (setq p1 (search-forward ">"))    ; text content begin
          (backward-char 1)
          (xhm-skip-tag-forward)
          (setq p2 (search-backward "<"))   ; text content end
(message "thissss 「%s」" (vector langCode p1 p2)) ; todo xxxxxxxxx 2013-06-25
          (vector langCode p1 p2)
          ))

 ) ))

(defun xhm-get-precode-make-new-file (ξlangNameMap)
  "Create a new file on current dir with text inside pre code block.
For example, if the cursor is somewhere between the tags:
<pre class=\"…\">…▮…</pre>

after calling, all a new file of name 「xx-‹random›.‹suffix›」 is created in current dir, with content from the block.

If there's a text selection, use that region as content."
  (interactive (list xhm-lang-name-map))
  (let* (
        (ξxx (xhm-get-precode-langCode))
        (ξlangCode (elt ξxx 0))
        (p1 (elt ξxx 1))
        (p2 (elt ξxx 2))
        (ξyy (cdr (assoc ξlangCode ξlangNameMap)))
        (ξfileSuffix (elt ξyy 1))
        (ξtextContent (buffer-substring-no-properties p1 p2) )
        )

    (progn
      (delete-region p1 p2 )
      (split-window-vertically)
      (find-file (format "xx-testscript-%d.%s" (random 9008000 ) ξfileSuffix) )
      (insert ξtextContent)
      (when (xhm-precode-htmlized-p (point-min) (point-max))
        (xhm-remove-span-tag-region (point-min) (point-max))
        )
;      (save-buffer )
      )
    )
  )

(defun xhm-htmlize-string (ξsourceCodeStr ξmajorModeName)
  "Take ξsourceCodeStr and return a htmlized version using major mode ξmajorModeName.
The purpose is to syntax color source code in HTML.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive)
  (let (htmlizeOutputBuffer resultStr)
    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert ξsourceCodeStr)
      (funcall (intern ξmajorModeName))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuffer (htmlize-buffer))
      )
    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuffer
      (let (p1 p2 )
        (setq p1 (search-forward "<pre>"))
        (setq p2 (search-forward "</pre>"))
        (setq resultStr (buffer-substring-no-properties (+ p1 1) (- p2 6))) ) )
    (kill-buffer htmlizeOutputBuffer)
    resultStr ) )

(defun xhm-htmlize-precode (ξlangCodeMap)
  "Replace text enclosed by “pre” tag to htmlized code.
For example, if the cursor is somewhere between the pre tags <pre class=\"‹langCode›\">…▮…</pre>, then after calling, the text inside the pre tag will be htmlized.  That is, wrapped with many span tags for syntax coloring.

The opening tag must be of the form <pre class=\"‹langCode›\">.  The ‹langCode› determines what emacs mode is used to colorize the text. See `xhm-lang-name-map' for possible ‹langCode›.

See also: `xhm-dehtmlize-precode', `xhm-htmlize-or-de-precode'.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive (list xhm-lang-name-map))
  (let (ξlangCode p1 p2 inputStr ξmodeName )

    (save-excursion
      (let* (
            (t78730 (xhm-get-precode-langCode))
            (ξlangCode (elt t78730 0))
            (p1 (elt t78730 1))
            (p2 (elt t78730 2))
            (ξmodeName (elt (cdr (assoc ξlangCode ξlangCodeMap)) 0))
            )

        ;; remove beginning or trailing whitespace
        (setq inputStr (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" (buffer-substring-no-properties p1 p2))) )

        (progn
          (let ((t38838 (xhm-htmlize-string inputStr ξmodeName) ))
            (if (equal inputStr t38838)
                (progn (message "No change needed."))
              (progn
                (delete-region p1 p2 )
                (goto-char p1)
                (insert t38838) ) ) ) ) ) ) ) )

(defun xhm-dehtmlize-precode ()
  "Delete span tags between pre tags.
Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.
This command does the inverse of `xhm-htmlize-precode'."
  (interactive)
  (let ((t55238 (xhm-get-precode-langCode)))
    (xhm-remove-span-tag-region (elt t55238 1) (elt t55238 2))
    ) )

(defun xhm-htmlize-or-de-precode (langCodeMap)
  "Call `xhm-htmlize-precode' or `xhm-dehtmlize-precode'."
  (interactive (list xhm-lang-name-map))
  (let* (
         (t34342 (xhm-get-precode-langCode))
         (p1 (elt t34342 1))
         (p2 (elt t34342 2))
         )
    (if (xhm-precode-htmlized-p p1 p2)
        (progn
          (message "doing de-htmlizing")
          ;; (xhm-remove-span-tag-region p1 p2)
          (xhm-dehtmlize-precode)
          )
      
      (progn
        (message "doing htmlizing")
        (xhm-htmlize-precode xhm-lang-name-map) )
      ;; (progn               ;; do htmlize
      ;;         (message "doing htmlizing")
      ;;         (let (
      ;;               langCodeResult
      ;;               ξmode-name)
      ;;           (setq langCodeResult (assoc langCode langCodeMap))
      ;;           (if (null langCodeResult)
      ;;               (progn (error "Your lang code 「%s」 is not recognized." langCode))
      ;;             (progn
      ;;               (save-excursion
      ;;                 (setq ξmode-name (elt (cdr langCodeResult) 0))
      ;;                 (let ((newInStr inputStr) resultStr)
      ;;                   (setq newInStr (replace-regexp-in-string "\\`[ \t\n]*" "" newInStr) ) ; trim beginning
      ;;                   (setq newInStr (replace-regexp-in-string "[ \t\n]+\\'" "" newInStr) ) ; trim trailing
      ;;                   (setq resultStr (xhm-htmlize-string newInStr ξmode-name))
      ;;                   (if (equal (length inputStr) (length resultStr))
      ;;                       (message "%s" "htmlize done, but no change necessary.")
      ;;                     (progn
      ;;                       (delete-region p1 p2)
      ;;                       (insert resultStr) ) ) ) )) )) )

      ) ))


;; syntax table
(defvar xhm-syntax-table nil "Syntax table for `xah-html-mode'.")
(setq xhm-syntax-table
      (let ((synTable (make-syntax-table)))

 (progn                                  ; all on US keyboard

 ;; (modify-syntax-entry ?\" "\"" synTable)
   (modify-syntax-entry ?' "w" synTable)

 ;; (modify-syntax-entry ?, "." synTable)
 ;; (modify-syntax-entry ?. "." synTable)
 ;; (modify-syntax-entry ?: "." synTable)
 ;; (modify-syntax-entry ?? "." synTable)
 ;; (modify-syntax-entry ?\; "." synTable)

 ;; (modify-syntax-entry ?! "." synTable)
 ;; (modify-syntax-entry ?@ "." synTable)
 ;; (modify-syntax-entry ?# "." synTable)
 ;; (modify-syntax-entry ?$ "." synTable)
 ;; (modify-syntax-entry ?% "." synTable)
 ;; (modify-syntax-entry ?^ "." synTable)
 ;; (modify-syntax-entry ?& "." synTable)
 ;; (modify-syntax-entry ?* "." synTable)
 ;; (modify-syntax-entry ?+ "." synTable)
 ;; (modify-syntax-entry ?= "." synTable)
 ;; (modify-syntax-entry ?/ "." synTable)
 ;; (modify-syntax-entry ?\ "/" synTable)

 ;; (modify-syntax-entry ?_ "_" synTable)
 ;; (modify-syntax-entry ?- "w" synTable)

 ;; (modify-syntax-entry ?( "(" synTable)
 ;; (modify-syntax-entry ?) ")" synTable)
 ;; (modify-syntax-entry ?[ "(" synTable)
 ;; (modify-syntax-entry ?] ")" synTable)
 ;; (modify-syntax-entry ?{ "(" synTable)
 ;; (modify-syntax-entry ?} ")" synTable)
        (modify-syntax-entry ?< "." synTable)
        (modify-syntax-entry ?> "." synTable)

 ;; (modify-syntax-entry ?| "." synTable)
 ;; (modify-syntax-entry ?` "." synTable)
 ;; (modify-syntax-entry ?~ "." synTable)
 )

        (modify-syntax-entry ?“ "(" synTable)
        (modify-syntax-entry ?” ")" synTable)
        (modify-syntax-entry ?‘ "(" synTable)
        (modify-syntax-entry ?’ ")" synTable)

;; (modify-syntax-entry ?“ "\"" synTable)
;; (modify-syntax-entry ?” "\"" synTable)

        synTable))



(defface xhm-curly“”-quoted-text-face
  '((((class color) (min-colors 88) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#458b00"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#76ee00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)

(defface xhm-curly‘’-quoted-text-face
  '((((class color) (min-colors 88) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 88) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 16) (background light)) (:foreground "#ffa500"))
    (((class color) (min-colors 16) (background dark)) (:foreground "#8b5a00"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'xah-html-mode)


;; keybinding

(defvar xhm-keymap nil "Keybinding for `xah-html-mode'")
(progn
  (setq xhm-keymap (make-sparse-keymap))
;  (define-key xhm-keymap [remap comment-dwim] 'xhm-comment-dwim)
;  (define-key xhm-keymap (kbd "C-c /") 'xhm-sgml-close-tag)
;  (define-key xhm-keymap (kbd "C-c <delete>") 'xhm-delete-tag)
  (define-key xhm-keymap (kbd "<C-right>") 'xhm-skip-tag-forward)
  (define-key xhm-keymap (kbd "<C-left>") 'xhm-skip-tag-backward)

  (define-key xhm-keymap (kbd "<menu> e 0") 'xhm-remove-html-tags)
  (define-key xhm-keymap (kbd "<menu> e 7") 'xhm-htmlize-or-de-precode)
  (define-key xhm-keymap (kbd "<menu> e 8") 'xhm-get-precode-make-new-file)
  (define-key xhm-keymap (kbd "<menu> e c") 'xhm-make-citation)
  (define-key xhm-keymap (kbd "<menu> e k") 'xhm-htmlize-keyboard-shortcut-notation)
  (define-key xhm-keymap (kbd "<menu> e l 6") 'xhm-source-url-linkify)
  (define-key xhm-keymap (kbd "<menu> e l .") 'xhm-lines-to-html-list)
  (define-key xhm-keymap (kbd "<menu> e l u") 'xhm-wrap-url)
  (define-key xhm-keymap (kbd "<menu> e l w") 'xhm-wikipedia-linkify)
  (define-key xhm-keymap (kbd "<menu> e m") 'xhm-pre-source-code)
  (define-key xhm-keymap (kbd "<menu> e p") 'xhm-wrap-p-tag)
  (define-key xhm-keymap (kbd "<menu> e r ,") 'xhm-replace-html-chars-to-unicode)
  (define-key xhm-keymap (kbd "<menu> e r .") 'xhm-replace-html-&<>-to-entities)
  (define-key xhm-keymap (kbd "<menu> e r 3") 'xhm-update-title)
  (define-key xhm-keymap (kbd "<menu> e r e") 'xhm-htmlize-elisp-keywords)
  (define-key xhm-keymap (kbd "<menu> e r k") 'xhm-emacs-to-windows-kbd-notation)
  (define-key xhm-keymap (kbd "<menu> e r m") 'xhm-make-html-table)
  (define-key xhm-keymap (kbd "<menu> e t r") 'xhm-rename-html-inline-image)
  (define-key xhm-keymap (kbd "<menu> e t u") 'xhm-extract-url)
  (define-key xhm-keymap (kbd "<menu> e u") 'xhm-wrap-html-tag)
)



(defun xhm-tag-self-closing-p (tagName)
  "Return true if the tag is a self-closing tag, ⁖ br."
  (interactive)
  (member tagName  xhm-html5-self-close-tags) )

(defun xhm-cursor-in-tag-markup-p (&optional bracketPositions)
  "Return true if cursor is inside a tag markup.
For example,
 <p class=\"…\">…</p>
 If cursor is between the beginning p or ending p markup.
 bracketPositions is optional. If nil, then
 `xhm-get-bracket-positions' is called to get it.
"
  (interactive)
  (let ( pl< pl> pr> pr< )
      (when (not bracketPositions)
        (progn
          (setq bracketPositions (xhm-get-bracket-positions) )
          (setq pl< (elt bracketPositions 0) )
          (setq pl> (elt bracketPositions 1) )
          (setq pr< (elt bracketPositions 2) )
          (setq pr> (elt bracketPositions 3) )
          )
        )
      (if (and (< pl> pl<) (< pr> pr<))
          (progn (message "%s" "yes") t)
        (progn (message "%s" "no") nil)
        ) ) )

(defun xhm-end-tag-p (&optional bracketPositions)
  "Return t if cursor is inside a begin tag, else nil.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
 It simply check if the left brack is followed by a slash or not.

bracketPositions is optional. If nil, then
 `xhm-get-bracket-positions' is called to get it.
"
  (let ( pl< pl> pr> pr< )
      (when (not bracketPositions)
        (progn
          (setq bracketPositions (xhm-get-bracket-positions) )
          (setq pl< (elt bracketPositions 0) )
          (setq pl> (elt bracketPositions 1) )
          (setq pr< (elt bracketPositions 2) )
          (setq pr> (elt bracketPositions 3) )
          )
        )
(goto-char pl<)
(forward-char 1)
(looking-at "/" )
       ) )

(defun xhm-get-tag-name (&optional left<)
  "Return the tag name.
This function assumes your cursor is inside a tag, ⁖ <…▮…>
"
  (let ( p1 p2 )
    (when (not left<)
      (setq left< (search-backward "<") )
      )
    (goto-char left<)
    (forward-char 1)
    (when (looking-at "/" )
      (forward-char 1)
      )
    (setq p1 (point) )
    (search-forward-regexp " \\|>")
    (backward-char 1)
    (setq p2 (point) )
    (buffer-substring-no-properties p1 p2)
    ) )

(defun xhm-get-bracket-positions ()
  "Returns html angle bracket positions.
Returns a vector [ pl< pl> pr< pr> ]
 pl< is the position of < nearest to cursor on the left side
 pl> is the position of > nearest to cursor on the left side
 similar for pr< and pr> for the right side.

this command does not `save-excursion'. You need to call that.
"
    ;; search for current tag.
    ;; find left nearest >, and right nearest <
    ;; or left nearest <, and right nearest >
    ;; determine if it's <…> or >…<.
    (let (
          (p-current (point))
          pl< ; position of first < char to the left of cursor
          pl>
          pr<
          pr>
          )
      (progn
        (goto-char p-current)
        (setq pl< (search-backward "<" nil "NOERROR") )
        (goto-char p-current)
        (setq pl> (search-backward ">" nil "NOERROR") )
        (goto-char p-current)
        (setq pr< (search-forward "<" nil "NOERROR") )
        (goto-char p-current)
        (setq pr> (search-forward ">" nil "NOERROR") )
        (vector pl< pl> pr< pr>)
 ) ) )

(defun xhm-delete-tag ()
  "work in progress. do nothing.
Delete the tag under cursor.
Also delete the matching beginning/ending tag."
  (interactive)
  (save-excursion
    ;; determine if it's inside the tag. ⁖ <…>
    ;; if so, good. else abort.
    ;; now, determine if it's opening tag or closing. ⁖ closing tag start with </
    ;; if it's opening tag, need to delete the matching one to the right
    ;; else, need to delete the matching one to the left
    ;; let's assume it's the opening.
    ;; now, determine if there's nested element. ⁖ <p>…<b>…</b>…</p>
    ;;    to do this, first determine the name of the tag. ⁖ the “p” in  <p …>, then search the matching tag.
    ;; if so, O shit, it's complex. Need to determine if one of the nested has the same tag name. and and …
    ;; if not, then we can proceed. Just find the closing tag and delete it. Also the beginning.
    (let ( )
      (if (xhm-cursor-in-tag-markup-p)
          (progn
            (if (xhm-end-tag-p)
                (progn (message "end %s" (xhm-get-tag-name)))
              (progn (message "begin %s" (xhm-get-tag-name))
                     )
              )
            )
        (progn (message "%s" "cursor needs to be inside a tag.") )
        )
      ) ) )

(defun xhm-skip-tag-forward ()
  "Move cursor to the closing tag."
  (interactive)
  (sgml-skip-tag-forward 1)
  )

(defun xhm-skip-tag-backward ()
  "Move cursor to the beginning tag."
  (interactive)
  (sgml-skip-tag-backward 1)
  )

(defun xhm-change-current-tag ()
  "change the tag name of current tag, and class name if there's one. WARNING:
this is a quick 1 min hackjob, works only when there's no nesting."
  (interactive)
  (let (p1 p2 oldTagName newTagName oldClassName newClassName)
    (search-backward "<" )
    (forward-char 1)
    (setq p1 (point) )
    (setq oldTagName (xhm-get-tag-name) )
    (setq newTagName (ido-completing-read "HTML tag:" xhm-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xhm-html-tag-input-history "span") )
    (goto-char p1)
    (delete-char (length oldTagName))
    (insert newTagName)
    (search-forward (concat "</" oldTagName))
    (delete-char (- (length oldTagName)))
    (insert newTagName)

    (progn
      (goto-char p1)
      (search-forward ">")
      (setq p2  (point) )
      (goto-char p1)
      (when
(search-forward-regexp "class[ \n]*=[ \n]*\"" p2 "NOERROR")
;(string-match "class[ \n]*=[ \n]*\"" (buffer-substring-no-properties p1 p2))
        (progn
               (setq p1 (point) )
               (search-forward "\"")
               (setq p2 (- (point) 1) )
               (setq oldClassName (buffer-substring-no-properties p1 p2) )
               (setq newClassName (read-string "new class name:") )
               (if (string-equal newClassName "")
                   (progn ; todo need to clean this up. don't use bunch of user functions
                     (delete-region p1 p2 )
                          (backward-kill-word 1)
                          (delete-char -1)
                          )
                 (progn (delete-region p1 p2 )
                      (goto-char p1)
                      (insert newClassName) ) ) ) ) ) ))

;; (defun xhm-split-tag ()
;;   "split a HTML element into 2 elements of the same tag, at cursor point.

;; for example:
;;  <p>some▮thing</p>
;; becomes
;;  <p>some</p>
;;  ▮
;;  <p>thing</p>
;; "
;;   (interactive)
;;   )

(defun xhm-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "<!--") (comment-end "-->"))
     (comment-dwim arg)))

(defun xhm-replace-html-&<>-to-entities ()
  "Replace HTML < > & to HTML entities.
This works on the current text selection or block of text.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

See also:
`xhm-replace-html-named-entities'
`xhm-replace-html-&<>-to-entities'
`xhm-replace-html-chars-to-unicode'
"
  (interactive)
  (let (bds p1 p2 myText)
    (setq bds (get-selection-or-unit 'block))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (save-excursion (replace-pairs-region p1 p2 '( ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ) ))
     ) )

(defun xhm-replace-html-chars-to-unicode ()
  "Replace HTML < > & to similar Unicode char.
This works on the current text selection or block of text.
The characters replaced are:
 & ⇒ ＆
 < ⇒ ‹
 > ⇒ ›

See also:
`xhm-replace-html-named-entities'
`xhm-replace-html-&<>-to-entities'
`xhm-replace-html-chars-to-unicode'
"
  (interactive)
  (let (bds p1 p2 myText)
    (setq bds (get-selection-or-unit 'block))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (replace-pairs-region p1 p2 '( ["&" "＆"] ["<" "‹"] [">" "›"] ) ) ) )

(defun xhm-replace-html-named-entities (ξstring &optional ξfrom ξto)
  "Replace HTML entities to Unicode character.
For example, “&copy;” becomes “©”.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto.

The following HTML Entities are not replaced:
 &amp; &
 &lt; <
 &gt; >

See also:
`xhm-replace-html-named-entities'
`xhm-replace-html-&<>-to-entities'
`xhm-replace-html-chars-to-unicode'
"
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)) )
       (list nil (elt bds 1) (elt bds 2))) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))

    (setq outputStr
          (let ((case-fold-search nil))
            (replace-pairs-in-string inputStr
 [
  ["&nbsp;" " "] ["&ensp;" " "] ["&emsp;" " "] ["&thinsp;" " "]

  ["&rlm;" "‏"] ["&lrm;" "‎"] ["&zwj;" "‍"] ["&zwnj;" "‌"]

  ["&iexcl;" "¡"] ["&cent;" "¢"] ["&pound;" "£"] ["&curren;" "¤"] ["&yen;" "¥"] ["&brvbar;" "¦"] ["&sect;" "§"] ["&uml;" "¨"] ["&copy;" "©"] ["&ordf;" "ª"] ["&laquo;" "«"] ["&not;" "¬"] ["&shy;" "­"] ["&reg;" "®"] ["&macr;" "¯"] ["&deg;" "°"] ["&plusmn;" "±"] ["&sup2;" "²"] ["&sup3;" "³"] ["&acute;" "´"] ["&micro;" "µ"] ["&para;" "¶"] ["&middot;" "·"] ["&cedil;" "¸"] ["&sup1;" "¹"] ["&ordm;" "º"] ["&raquo;" "»"] ["&frac14;" "¼"] ["&frac12;" "½"] ["&frac34;" "¾"] ["&iquest;" "¿"] ["&Agrave;" "À"] ["&Aacute;" "Á"] ["&Acirc;" "Â"] ["&Atilde;" "Ã"] ["&Auml;" "Ä"] ["&Aring;" "Å"] ["&AElig;" "Æ"] ["&Ccedil;" "Ç"] ["&Egrave;" "È"] ["&Eacute;" "É"] ["&Ecirc;" "Ê"] ["&Euml;" "Ë"] ["&Igrave;" "Ì"] ["&Iacute;" "Í"] ["&Icirc;" "Î"] ["&Iuml;" "Ï"] ["&ETH;" "Ð"] ["&Ntilde;" "Ñ"] ["&Ograve;" "Ò"] ["&Oacute;" "Ó"] ["&Ocirc;" "Ô"] ["&Otilde;" "Õ"] ["&Ouml;" "Ö"] ["&times;" "×"] ["&Oslash;" "Ø"] ["&Ugrave;" "Ù"] ["&Uacute;" "Ú"] ["&Ucirc;" "Û"] ["&Uuml;" "Ü"] ["&Yacute;" "Ý"] ["&THORN;" "Þ"] ["&szlig;" "ß"] ["&agrave;" "à"] ["&aacute;" "á"] ["&acirc;" "â"] ["&atilde;" "ã"] ["&auml;" "ä"] ["&aring;" "å"] ["&aelig;" "æ"] ["&ccedil;" "ç"] ["&egrave;" "è"] ["&eacute;" "é"] ["&ecirc;" "ê"] ["&euml;" "ë"] ["&igrave;" "ì"] ["&iacute;" "í"] ["&icirc;" "î"] ["&iuml;" "ï"] ["&eth;" "ð"] ["&ntilde;" "ñ"] ["&ograve;" "ò"] ["&oacute;" "ó"] ["&ocirc;" "ô"] ["&otilde;" "õ"] ["&ouml;" "ö"] ["&divide;" "÷"] ["&oslash;" "ø"] ["&ugrave;" "ù"] ["&uacute;" "ú"] ["&ucirc;" "û"] ["&uuml;" "ü"] ["&yacute;" "ý"] ["&thorn;" "þ"] ["&yuml;" "ÿ"] ["&fnof;" "ƒ"] ["&Alpha;" "Α"] ["&Beta;" "Β"] ["&Gamma;" "Γ"] ["&Delta;" "Δ"] ["&Epsilon;" "Ε"] ["&Zeta;" "Ζ"] ["&Eta;" "Η"] ["&Theta;" "Θ"] ["&Iota;" "Ι"] ["&Kappa;" "Κ"] ["&Lambda;" "Λ"] ["&Mu;" "Μ"] ["&Nu;" "Ν"] ["&Xi;" "Ξ"] ["&Omicron;" "Ο"] ["&Pi;" "Π"] ["&Rho;" "Ρ"] ["&Sigma;" "Σ"] ["&Tau;" "Τ"] ["&Upsilon;" "Υ"] ["&Phi;" "Φ"] ["&Chi;" "Χ"] ["&Psi;" "Ψ"] ["&Omega;" "Ω"] ["&alpha;" "α"] ["&beta;" "β"] ["&gamma;" "γ"] ["&delta;" "δ"] ["&epsilon;" "ε"] ["&zeta;" "ζ"] ["&eta;" "η"] ["&theta;" "θ"] ["&iota;" "ι"] ["&kappa;" "κ"] ["&lambda;" "λ"] ["&mu;" "μ"] ["&nu;" "ν"] ["&xi;" "ξ"] ["&omicron;" "ο"] ["&pi;" "π"] ["&rho;" "ρ"] ["&sigmaf;" "ς"] ["&sigma;" "σ"] ["&tau;" "τ"] ["&upsilon;" "υ"] ["&phi;" "φ"] ["&chi;" "χ"] ["&psi;" "ψ"] ["&omega;" "ω"] ["&thetasym;" "ϑ"] ["&upsih;" "ϒ"] ["&piv;" "ϖ"] ["&bull;" "•"] ["&hellip;" "…"] ["&prime;" "′"] ["&Prime;" "″"] ["&oline;" "‾"] ["&frasl;" "⁄"] ["&weierp;" "℘"] ["&image;" "ℑ"] ["&real;" "ℜ"] ["&trade;" "™"] ["&alefsym;" "ℵ"] ["&larr;" "←"] ["&uarr;" "↑"] ["&rarr;" "→"] ["&darr;" "↓"] ["&harr;" "↔"] ["&crarr;" "↵"] ["&lArr;" "⇐"] ["&uArr;" "⇑"] ["&rArr;" "⇒"] ["&dArr;" "⇓"] ["&hArr;" "⇔"] ["&forall;" "∀"] ["&part;" "∂"] ["&exist;" "∃"] ["&empty;" "∅"] ["&nabla;" "∇"] ["&isin;" "∈"] ["&notin;" "∉"] ["&ni;" "∋"] ["&prod;" "∏"] ["&sum;" "∑"] ["&minus;" "−"] ["&lowast;" "∗"] ["&radic;" "√"] ["&prop;" "∝"] ["&infin;" "∞"] ["&ang;" "∠"] ["&and;" "∧"] ["&or;" "∨"] ["&cap;" "∩"] ["&cup;" "∪"] ["&int;" "∫"] ["&there4;" "∴"] ["&sim;" "∼"] ["&cong;" "≅"] ["&asymp;" "≈"] ["&ne;" "≠"] ["&equiv;" "≡"] ["&le;" "≤"] ["&ge;" "≥"] ["&sub;" "⊂"] ["&sup;" "⊃"] ["&nsub;" "⊄"] ["&sube;" "⊆"] ["&supe;" "⊇"] ["&oplus;" "⊕"] ["&otimes;" "⊗"] ["&perp;" "⊥"] ["&sdot;" "⋅"] ["&lceil;" "⌈"] ["&rceil;" "⌉"] ["&lfloor;" "⌊"] ["&rfloor;" "⌋"] ["&lang;" "〈"] ["&rang;" "〉"] ["&loz;" "◊"] ["&spades;" "♠"] ["&clubs;" "♣"] ["&hearts;" "♥"] ["&diams;" "♦"] ["&quot;" "\""] ["&OElig;" "Œ"] ["&oelig;" "œ"] ["&Scaron;" "Š"] ["&scaron;" "š"] ["&Yuml;" "Ÿ"] ["&circ;" "ˆ"] ["&tilde;" "˜"] ["&ndash;" "–"] ["&mdash;" "—"] ["&lsquo;" "‘"] ["&rsquo;" "’"] ["&sbquo;" "‚"] ["&ldquo;" "“"] ["&rdquo;" "”"] ["&bdquo;" "„"] ["&dagger;" "†"] ["&Dagger;" "‡"] ["&permil;" "‰"] ["&lsaquo;" "‹"] ["&rsaquo;" "›"] ["&euro;" "€"]
  ]
 )
            )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun xhm-get-html-file-title (fName)
  "Return FNAME <title> tag's text.
Assumes that the file contains the string
“<title>…</title>”."
  (with-temp-buffer
      (insert-file-contents fName nil nil nil t)
      (goto-char 1)
      (buffer-substring-no-properties
       (search-forward "<title>") (- (search-forward "</title>") 8))
      ))

(defun xhm-lines-to-html-list ()
  "Make the current block of lines into a HTML list.
Any URL in the line will be turned into links.

Example:
If your cursor is in the following block of text:

Castratos are castrated males made for singing: http://en.wikipedia.org/wiki/Castrato , record of the last castrato: http://www.archive.org/details/AlessandroMoreschi
human vocal range: http://en.wikipedia.org/wiki/Vocal_range

It will become:
<ul>
<li>Castratos are castrated males made for singing: <a href=\"http://en.wikipedia.org/wiki/Castrato\">Castrato</a> , record of the last castrato: <a href=\"http://www.archive.org/details/AlessandroMoreschi\">http://www.archive.org/details/AlessandroMoreschi</a></li>
<li>human vocal range: <a href=\"http://en.wikipedia.org/wiki/Vocal_range\">Vocal range</a></li>
</ul>"
  (interactive)
  (let (bds p1 p2 inputStr resultStr)
    (setq bds (get-selection-or-unit 'block))
    (setq inputStr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (save-excursion
      (setq resultStr
            (with-temp-buffer
              (insert inputStr)
              (delete-trailing-whitespace)
              (goto-char 1)
              (while
                  (search-forward-regexp  "\.html$" nil t)
                (backward-char 1)
                (xah-all-linkify)
                )

              (goto-char 1)
              (while
                  (not (equal (line-end-position) (point-max)))
                (beginning-of-line) (insert "<li>")
                (end-of-line) (insert "</li>")
                (forward-line 1 )
                )

              (beginning-of-line) (insert "<li>")
              (end-of-line) (insert "</li>")

              (goto-char 1)
              (insert "<ul>\n")
              (goto-char (point-max))
              (insert "\n</ul>")

              (buffer-string)
              ) )
      )
    (delete-region p1 p2)
    (insert resultStr)
    ) )

(defun xhm-make-html-table-string (textBlock ξdelimiter)
  "Transform the string TEXTBLOCK into a HTML marked up table.

 “\\n” is used as delimiter of rows. Extra newlines at the end is discarded.
The argument ξdelimiter is a char used as the delimiter for columns.

 See the parent function `xhm-make-html-table'."
(let ((txtbk textBlock))
    (setq txtbk (replace-regexp-in-string "\n+$" "\n" (concat txtbk "\n"))) ; make sure ending is just one newline char
    (setq txtbk (replace-regexp-in-string ξdelimiter "</td><td>" txtbk))
    (setq txtbk (replace-regexp-in-string "\n" "</td></tr>\n<tr><td>" txtbk))
    (setq txtbk (substring txtbk 0 -8)) ; delete the beginning “<tr><td>” in last line
    (concat "<table class=\"nrm\">\n<tr><td>" txtbk "</table>")
))

(defun xhm-make-html-table (sep)
  "Transform the current text block or selection into a HTML table.

If there's a text selection, use the selection as input.
Otherwise, used current text block delimited by empty lines.

SEP is a string used as a delimitor for columns.

For example:

a*b*c
1*2*3
this*and*that

with “*” as separator, becomes

<table class=\"nrm\">
<tr><td>a</td><td>b</td><td>c</td></tr>
<tr><td>1</td><td>2</td><td>3</td></tr>
<tr><td>this</td><td>and</td><td>that</td></tr>
</table>"
  (interactive "sEnter string pattern for column separation:")
  (let (bds p1 p2 myStr)

    (setq bds (get-selection-or-unit 'block))
    (setq myStr (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )
    (delete-region p1 p2)
    (insert (xhm-make-html-table-string myStr sep) "\n")
  ))

(defun xhm-make-html-table-undo ()
  "inverse of `xhm-make-html-table'."
  (interactive)
  (let ( p1 p2 myStr)
(search-backward "<table")
    (setq p1 (point) )
(search-forward "</table>")
    (setq p2 (point) )
;(replace-pairs-region p1 p2 [
;])

(replace-regexp-pairs-region p1 p2 [
["<table \\([^>]+?\\)>" ""]
["</th><th>" "•"]
["</td><td>" "•"]
["<tr>" ""]
["</tr>" ""]
["</table>" ""]
]
 "FIXEDCASE" "LITERAL"
)
  ))

(defun xhm-wikipedia-linkify ()
  "Make the current word or text selection into a Wikipedia link.

For Example: 「Emacs」 ⇒ 「<a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>」"
  (interactive)
  (let (linkText bds p1 p2 wikiTerm resultStr)
    (setq bds (get-selection-or-unit 'url))
    (setq linkText (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )
    (setq wikiTerm (replace-regexp-in-string " " "_" linkText) )
    (setq resultStr (concat "<a href=\"http://en.wikipedia.org/wiki/" wikiTerm "\">" linkText "</a>"))
    (delete-region p1 p2)
    (insert resultStr) ))

(defun xhm-remove-span-tag-region (p1 p2)
  "Delete HTML “span” tags in region.
And the following HTML entities are changed:
 &amp; ⇒ &
 &lt; ⇒ <
 &gt; ⇒ >

Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region p1 p2)
      (replace-regexp-pairs-region (point-min) (point-max) '(["<span class=\"[^\"]+\">" ""]))
      (replace-pairs-region (point-min) (point-max) '( ["</span>" ""] ["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">"] ) ) ) ) )

(defun xhm-remove-html-tags (ξstring &optional ξfrom ξto)
"Delete HTML tags in string or region.
Work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto.

WARNING: this command currently does not cover all html tags or convert all html entities.
For robust solution you might use: 「lynx -dump -display_charset=utf-8 URL」."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)) )
       (list nil (elt bds 1) (elt bds 2))) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let ((case-fold-search t) (tempStr inputStr))
(setq tempStr (replace-regexp-pairs-in-string tempStr '(
["<a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a>" "\\2 〔 \\1 〕"]
["<img src=\"\\([^\"]+?\\)\" alt=\"\\([^\"]+?\\)\" width=\"[0-9]+\" height=\"[0-9]+\" */?>" "〔IMAGE “\\2” \\1 〕"]
["<[a-z0-9]+ */?>" ""]
["<[a-z0-9]+ class=\"[^\"]+\">" ""]
["</[a-z0-9]+>" ""]

["&amp;" "&"]
["&lt;" "<"]
["&gt;" ">"]

)))

tempStr
             )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun xhm-extract-url (htmlText &optional convert-relative-URL-p)
  "Returns a list of URLs in the HTML text string htmlText.

When called interactively, use text selection as input, or current text block between empty lines. Output URLs in a buffer named 「*extract URL output*」, also copy output to `kill-ring'.

If `universal-argument' is called first, tries to convert relative URL to HTTP form.

WARNING: this function extract all text of the form 「<a … href=\"…\" …>」 by a simple regex. It does not extract single quote form 「href='…'」 nor 「src=\"…\"」 , nor other considerations."
  (interactive (list (elt (get-selection-or-unit 'block) 0) current-prefix-arg ) )
  (let ((urlList (list)))
    (with-temp-buffer
      (insert htmlText)
      (goto-char 1)
      (while (re-search-forward "<a.+?href=\"\\([^\"]+?\\)\".+?>" nil "NOERROR")
        (setq urlList (cons (match-string 1) urlList))
        )
      (goto-char 1)
      (while (re-search-forward "<img.+?src=\"\\([^\"]+?\\)\".+?>" nil "NOERROR")
        (setq urlList (cons (match-string 1) urlList))
        )
      )
    (setq urlList (reverse urlList) )
    (when convert-relative-URL-p
      (setq urlList
            (mapcar
             (lambda (ξx)
               (xahsite-filepath-to-url (xahsite-href-value-to-filepath ξx (buffer-file-name) )) )
             urlList) ) )

    (when (called-interactively-p 'any)
      (with-output-to-temp-buffer "*extract URL output*"
        (let ((printedResult (mapconcat 'identity urlList "\n")))
          (princ printedResult)
          (kill-new (mapconcat 'identity urlList "\n"))
          (message "URLs placed in kill-ring.")
          ) ) )
    urlList ))

(defun xhm-update-title (newTitle)
  "Update a HTML article's title and h1 tags.
Update the <title>…</title> and <h1>…</h1> of current buffer."
  (interactive
   (let (oldTitle)
     (save-excursion
       (goto-char 1)
       (search-forward-regexp "<title>\\([^<]+?\\)</title>")
       (setq oldTitle (match-string 1 ) )
       )
     (list (read-string "New title:" oldTitle nil oldTitle "INHERIT-INPUT-METHOD")) ) )
  (let (p1 p2)
    (save-excursion
      (goto-char 1)

      (progn (search-forward "<title>")
             (setq p1 (point) )
             (search-forward "<")
             (setq p2 (- (point) 1) )
             (delete-region p1 p2 )
             (goto-char p1)
             (insert newTitle ) )

      (progn (search-forward "<h1>")
             (setq p1 (point) )
             (search-forward "<")
             (setq p2 (- (point) 1) )
             (delete-region p1 p2 )
             (goto-char p1)
             (insert newTitle ) )
      ) ))

(defun xhm-make-citation ()
  "Reformat current text block or selection into a canonical citation format.

For example, place cursor somewhere in the following block:

Circus Maximalist
By PAUL GRAY
Monday, Sep. 12, 1994
http://www.time.com/time/magazine/article/0,9171,981408,00.html

After execution, the lines will become

<cite>Circus Maximalist</cite> <time>1994-09-12</time> By Paul Gray. @ <a href=\"http://www.time.com/time/magazine/article/0,9171,981408,00.html\">Source www.time.com</a>

If there's a text selection, use it for input, otherwise the input is a text block between empty lines.

The order of lines for {title, author, date/time, url} needs not be in that order. Author should start with “by”.
"
  (interactive)
  (let* (
    (bds (get-selection-or-unit 'block))
    (inputText (elt bds 0) )
    (p1 (elt bds 1) )
    (p2 (elt bds 2) )
 myList ξtitle ξauthor ξdate ξurl )

    (setq inputText (replace-regexp-in-string "^[[:space:]]*" "" inputText)) ; remove white space in front

    (setq myList (split-string inputText "[[:space:]]*\n[[:space:]]*" t) )

    (let (x)
      (while (> (length myList) 0)
        (setq x (pop myList) )
        (cond
         ((string-match "^https?://" x ) (setq ξurl x))
         ((string-match "^ *by " x ) (setq ξauthor x))
         ((is-datetimestamp-p x) (setq ξdate x))
         (t (setq ξtitle x))
         )
        )
      )

(message "title %s\n author %s\n date %s\n url %s" ξtitle ξauthor ξdate ξurl)

    (when (null ξtitle) (error "I can't find “title”"))
    (when (null ξauthor) (error "I can't find “author”"))
    (when (null ξdate) (error "I can't find “date”"))
    (when (null ξurl) (error "I can't find “url”"))

    (setq ξtitle (trim-string ξtitle))
    (setq ξtitle (replace-regexp-in-string "^\"\\(.+\\)\"$" "\\1" ξtitle))
    (setq ξtitle (replace-pairs-in-string ξtitle '(["’" "'"] ["&" "＆"] )))

    (setq ξauthor (trim-string ξauthor))
    (setq ξauthor (replace-regexp-in-string "\\. " " " ξauthor)) ; remove period in Initals
    (setq ξauthor (replace-regexp-in-string "By +" "" ξauthor))
    (setq ξauthor (upcase-initials (downcase ξauthor)))

    (setq ξdate (trim-string ξdate))
    (setq ξdate (fix-datetimestamp ξdate))

    (setq ξurl (trim-string ξurl))
    (setq ξurl (with-temp-buffer (insert ξurl) (xhm-source-url-linkify 2) (buffer-string)))

    (delete-region p1 p2 )
    (insert (concat "<cite>" ξtitle "</cite>") " " "<time>" ξdate "</time>"  " By " ξauthor ". @ " ξurl)
    ))

(defun xhm-make-link-defunct ()
  "Make the html link under cursor to a defunct form.
Example:
If cursor is inside this tag
<a class=\"sorc\" href=\"http://example.com/\" data-accessed=\"2008-12-26\">…</a>
 (and inside the opening tag.)

It becomes:
<s class=\"deadurl\" title=\"accessed:2008-12-26; defunct:2008-12-26; http://example.com\">…</s>"
  (interactive)
  (let (p1 p2 wholeLinkStr newLinkStr ξurl titleStr)
    (save-excursion
      ;; get the boundary of opening tag
      (forward-char 3)
      (search-backward "<a " ) (setq p1 (point) )
      (search-forward "</a>") (setq p2 (point) )

      ;; get wholeLinkStr
      (setq wholeLinkStr (buffer-substring-no-properties p1 p2))

      ;; generate replacement text
      (with-temp-buffer
        (insert wholeLinkStr)

        (goto-char 1)
        (search-forward-regexp  "href=\"\\([^\"]+?\\)\"")
        (setq ξurl (match-string 1))

        (search-forward-regexp  "data-accessed=\"\\([^\"]+?\\)\"")
        (setq titleStr (match-string 1))

        (setq newLinkStr (format "<s class=\"deadurl\" title=\"accessed:%s; defunct:%s\">%s</s>" titleStr (format-time-string "%Y-%m-%d") ξurl ) )))

    (delete-region p1 p2)
    (insert newLinkStr)))

(defun xhm-source-url-linkify (prefixArgCode)
  "Make URL at cursor point into a html link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
<a class=\"sorc\" href=\"http://example.com/xyz.htm\" data-accessed=\"2008-12-25\">example.com…</a>

The anchor text may be of 4 possibilities, depending on value of `universal-argument'.

1 → 「‹full url›」
2 or 4 → 「‹domain›…」
3 → 「img src」
0 or any → smartly decide."

  (interactive "P")
  (let (inputStr
        bds p1-input p2-input
        p1-url p2-url p1-tag p2-tag
        ξurl domainName linkText resultLinkStr)

    (setq bds (get-selection-or-unit 'url))
    (setq inputStr (elt bds 0) )
    (setq p1-input (elt bds 1) )
    (setq p2-input (elt bds 2) )

    ;; check if it's just plain URL or already in linked form 「<a href=…>…</a>」
    ;; If latter, you need to get the boundaries for the entire link too.
    (if (string-match "href=\"" inputStr)
        (save-excursion
          (search-backward "href=" (- (point) 104)) ; search boundary as extra guard for error
          (forward-char 6)
          (setq p1-url (point))
          (search-forward "\"" (+ p1-url 104))
          (setq p2-url (- (point) 1))

          (goto-char p1-url)
          (search-backward "<a" (- p1-url 30) )
          (setq p1-tag (point))
          (goto-char p2-url)
          (search-forward "</a>" (+ p2-url 140))
          (setq p2-tag (point))
          )
      (progn
        (setq p1-url p1-input)
        (setq p2-url p2-input)
        (setq p1-tag p1-input)
        (setq p2-tag p2-input) ) )

    (setq ξurl (replace-regexp-in-string "&amp;" "&" (buffer-substring-no-properties p1-url p2-url) nil "LITERAL") ) ; in case it's already encoded. TODO this is only 99% correct.

    ;; get the domainName
    (setq domainName
          (progn
            (string-match "://\\([^\/]+?\\)/" ξurl)
            (match-string 1 ξurl)
            )
          )

    (setq linkText
          (cond
           ((equal prefixArgCode 1) ξurl)           ; full url
           ((or (equal prefixArgCode 2) (equal prefixArgCode 4) (equal prefixArgCode '(4))) (concat domainName "…"))           ; ‹domain›…
           ((equal prefixArgCode 3) "img src")           ; img src
           (t (if
                  (or
                   (string-match "wikipedia\\.org.+jpg$" ξurl)
                   (string-match "wikipedia\\.org.+JPG$" ξurl)
                   (string-match "wikipedia\\.org.+png$" ξurl)
                   (string-match "wikipedia\\.org.+PNG$" ξurl)
                   (string-match "wikipedia\\.org.+svg$" ξurl)
                   (string-match "wikipedia\\.org.+SVG$" ξurl)
                   )
                  "img src"
                ξurl
                ))        ; smart
           )
          )

    (setq ξurl (replace-regexp-in-string "&" "&amp;" ξurl))
    (setq resultLinkStr
          (format "<a class=\"sorc\" href=\"%s\" data-accessed=\"%s\">%s</a>"
                  ξurl (format-time-string "%Y-%m-%d") linkText
                  )
          )

    ;; delete URL and insert the link
    (delete-region p1-tag p2-tag)
    (insert resultLinkStr)
    ))

(defun xhm-wikipedia-url-linkify (ξstring &optional ξfrom-to-pair)
  "Make the URL at cursor point into a html link.

If there is a text selection, use that as input.

Example:
http://en.wikipedia.org/wiki/Emacs
⇒
<a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>.

When called interactively, work on current URL or text selection.

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions in sequence ξfrom-to-pair."

  (interactive
   (if (region-active-p)
       (list nil (vector (region-beginning) (region-end)))
     (let ((bds (get-selection-or-unit 'url)) )
       (list nil (vector (aref bds 1) (aref bds 2))) ) ) )

  (let (workOnStringP inputStr outputStr
                      (ξfrom (elt ξfrom-to-pair 0))
                      (ξto (elt ξfrom-to-pair 1)))
    (setq workOnStringP (if () t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))

    (setq outputStr (format "<a href=\"%s\">%s</a>" (url-percent-encode-string inputStr) (replace-regexp-in-string "_" " " (url-percent-decode-string (file-name-nondirectory inputStr) ) )) )

    (if workOnStringP
        outputStr
      (progn
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun xhm-wrap-url (ξstring &optional ξfrom ξto)
  "Make the URL at cursor point into a html link.

When called interactively, work on current glyph sequence or text selection.

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (unit-at-cursor 'glyphs)) )
       (list nil (elt bds 1) (elt bds 2)) ) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr (concat "<a href=\"" (url-percent-encode-string inputStr) "\">" inputStr "</a>" )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) )
  )

(defun xhm-wrap-p-tag ()
  "Add <p>…</p> tag to current text block or text selection.
If there's a text selection, wrap p around each text block (separated by 2 newline chars.)"
  (interactive)
  (let (bds p1 p2 inputText)

    (setq bds (get-selection-or-unit 'block))
    (setq inputText (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (delete-region p1 p2 )
    (insert "<p>" (replace-regexp-in-string "\n\n+" "</p>\n\n<p>" (trim-string inputText)) "</p>")
    )
  )

(defun xhm-emacs-to-windows-kbd-notation-string (inputStr)
  "Change emacs keyboard-shortcut notation to Windows's notation.
For example:
 「C-h f」⇒ 「Ctrl+h f」
 「M-a」⇒ 「Meta+a」
 「<f9> <f8>」 ⇒ 「F9 F8」

This command will do most emacs syntax correctly, but not 100% correct.
"
(let ((case-fold-search nil))
(replace-regexp-pairs-in-string inputStr
[
                            ["C-\\(.\\)" "Ctrl+\\1"]
                            ["M-\\(.\\)" "Meta+\\1"]
                            ["S-\\(.\\)" "Shift+\\1"]
                            ["s-\\(.\\)" "Super+\\1"]
                            ["H-\\(.\\)" "Hyper+\\1"]

                            ["<prior>" "PageUp"]
                            ["<next>" "PageDown"]
                            ["<home>" "Home"]
                            ["<end>" "End"]

                            ["<f1>" "F1"] ["<f2>" "F2"] ["<f3>" "F3"] ["<f4>" "F4"] ["<f5>" "F5"] ["<f6>" "F6"] ["<f7>" "F7"] ["<f8>" "F8"] ["<f9>" "F9"] ["<f10>" "F10"] ["<f11>" "F11"] ["<f12>" "F12"]

                            ["RET" "Enter"]
                            ["<return>" "Return"]
                            ["TAB" "Tab"]
                            ["<tab>" "Tab"]

                            ["<right>" "→"]
                            ["<left>" "←"]
                            ["<up>" "↑"]
                            ["<down>" "↓"]

                            ["<insert>" "Insert"]
                            ["<delete>" "Delete"]

                            ["<backspace>" "Backspace"]
                            ["DEL" "Delete"]
                            ]
 "FIXEDCASE"))
 )

(defun xhm-emacs-to-windows-kbd-notation (p1 p2)
  "Change emacs key notation to Windows's notation.

For example:
 【C-h f】⇒ 【Ctrl+h f】
 【M-a】⇒ 【Meta+a】

When called interactively, work on text selection or text enclosed in 【…】.

For detail on exactly which string are changed, see `xhm-emacs-to-windows-kbd-notation-string'.
"
  (interactive
   (let ((bds (get-selection-or-unit ["^【" "^】"])) )
     (list (elt bds 1) (elt bds 2)) ) )

  (let (  (case-fold-search nil)
          (inputStr (buffer-substring-no-properties p1 p2))
          )
    (delete-region p1 p2 )
    (insert
     (xhm-emacs-to-windows-kbd-notation-string inputStr) ) ) )

(defun xhm-htmlize-elisp-keywords (p1 p2)
  "Replace curly quoted elisp function/variable names to HTML markup.

Example:
 Call “sort-lines” to sort.
    ⇓
 Call <code class=\"elisp-ƒ\">sort-lines</code> to sort.

 Set “fill-column”
    ⇓
 Set <var class=\"elisp\">fill-column</var>

Works on text selection or current text block.

When called in lisp program, the arguments p1 p2 are region positions.

Note: a word is changed only if all of the following are true:

• The symbol string is tightly enclosed in double curly quotes, e.g. “sort-lines” but not “use sort-lines”.
• `fboundp' or `boundp' returns true (for function and variable.).
• symbol string's char contains only alphanumeric or hyphen, even though elisp identifier allows many other chars. e.g. `yas/reload-all', `color-cie-ε'.

This command also makes a report of changed items.

Some issues:

• Some words are common in other lang, e.g. “while”, “print”, “string”, unix “find”, “grep”, HTML's “kbd” tag, etc. But they are also built-in elisp symbols. This command will tag them, but you may not want that.

• Some function/variable are from 3rd party libs, and some are not bundled with GNU emacs , e.g. 「'cl」, 「'htmlize」. They may or may not be tagged depending whether they've been loaded."
  ;; (interactive (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ) )
  (interactive
   (cond
    ((equal current-prefix-arg nil)    ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ))
    (t                                  ; all other cases
     (list (point-min) (point-max) )) ) )
  (let*
      (inputStr
       resultStr
       (changedItems nil)
       (elispIdentifierRegex "\\([-A-Za-z0-9]+\\)")
       (wantedRegex (concat "“" elispIdentifierRegex "”") )
       )
    (setq inputStr (buffer-substring-no-properties p1 p2) )
    (setq resultStr
            (let ( mStr (case-fold-search nil) (ξsomeStr inputStr) )
              (with-temp-buffer
                (insert ξsomeStr)
                (goto-char 1)
                (while (search-forward-regexp wantedRegex (point-max) t)
                  (setq mStr (match-string 1) )
                  (cond
                   ((fboundp (intern mStr))
                    (progn
                      (setq changedItems (cons (format "ƒ %s" mStr) changedItems ) )
                      (replace-match (concat "<code class=\"elisp-ƒ\">" mStr "</code>") t t)
                      ))
                   ((boundp (intern mStr))
                    (progn
                      (setq changedItems (cons (format "υ %s" mStr) changedItems ) )
                      (replace-match (concat "<var class=\"elisp\">" mStr "</var>") t t)
                       ))
                   (t "do nothing")
                   ) )
                (buffer-string)
                ) ))
    (if (equal (length changedItems) 0)
        (progn (message "%s" "No change needed."))
      (progn
            (delete-region p1 p2)
            (insert resultStr)
            (with-output-to-temp-buffer "*changed items*"
              (mapcar (lambda (x) (princ x) (princ "\n") ) (reverse changedItems)) )
         ) ) ))

(defun xhm-htmlize-keyboard-shortcut-notation ()
  "Wrap a “kbd” tag around keyboard keys on text selection or current line.
Example: 【ctrl+w】 ⇒ 【<kbd>Ctrl</kbd>+<kbd>w</kbd>】
Emacs's key notation also supported. Example: 【C-x t】 ⇒ 【<kbd>Ctrl</kbd>+<kbd>x</kbd> <kbd>t</kbd>】
Same for Alt, Shift, Cmd, Win, Enter, Return, Home… and other strings.
Case shouldn't matter, except when it's emacs's key notation.
"
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'line))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         (replaceList [
                       ;; case in find string shouldn't matter.
                       ["ctrl" "<kbd>Ctrl</kbd>"]
                       ["altgr" "<kbd>AltGr</kbd>"]
                       ["alt" "<kbd>Alt</kbd>"]
                       ["shift" "<kbd>⇧ Shift</kbd>"]
                       ["cmd" "<kbd>⌘ Cmd</kbd>"]
                       ["option" "<kbd>⌥ Opt</kbd>"]
                       ["win" "<kbd>❖ Win</kbd>"]
                       ["menu" "<kbd>▤ Menu</kbd>"]
                       ["meta" "<kbd>Meta</kbd>"]
                       ["super" "<kbd>❖ Super</kbd>"]
                       ["hyper" "<kbd>Hyper</kbd>"]

                       ["return" "<kbd>Return ↩</kbd>"]
                       ["enter" "<kbd>Enter ↵</kbd>"]
                       ["backspace" "<kbd>⌫ Backspace</kbd>"]
                       ["delete" "<kbd>⌦ Delete</kbd>"]
                       ["del" "<kbd>⌦ Delete</kbd>"]
                       ["space" "<kbd>Space</kbd>"]
                       ["capslock" "<kbd>Caps Lock</kbd>"]
                       ["f-lock" "<kbd>F Lock</kbd>"]
                       ["numlock" "<kbd>Num Lock</kbd>"]
                       ["scrolllock" "<kbd>Scroll Lock</kbd>"]
                       ["tab" "<kbd>Tab ↹</kbd>"]
                       ["esc" "<kbd>Esc</kbd>"]

                       ["copy" "<kbd>Copy</kbd>"]
                       ["cut" "<kbd>✂ Cut</kbd>"]
                       ["paste" "<kbd>Paste</kbd>"]
                       ["undo" "<kbd>⎌</kbd>"]
                       ["redo" "<kbd>↷</kbd>"]

                       ["F10" "<kbd>F10</kbd>"]
                       ["F11" "<kbd>F11</kbd>"]
                       ["F12" "<kbd>F12</kbd>"]
                       ["F13" "<kbd>F13</kbd>"]
                       ["F14" "<kbd>F14</kbd>"]
                       ["F15" "<kbd>F15</kbd>"]
                       ["F16" "<kbd>F16</kbd>"]
                       ["F1" "<kbd>F1</kbd>"]
                       ["F2" "<kbd>F2</kbd>"]
                       ["F3" "<kbd>F3</kbd>"]
                       ["F4" "<kbd>F4</kbd>"]
                       ["F5" "<kbd>F5</kbd>"]
                       ["F6" "<kbd>F6</kbd>"]
                       ["F7" "<kbd>F7</kbd>"]
                       ["F8" "<kbd>F8</kbd>"]
                       ["F9" "<kbd>F9</kbd>"]
                       ["Fn" "<kbd>Fn</kbd>"]

                       ["kp0" "<kbd>Keypad 0</kbd>"]
                       ["kp1" "<kbd>Keypad 1</kbd>"]
                       ["kp2" "<kbd>Keypad 2</kbd>"]
                       ["kp3" "<kbd>Keypad 3</kbd>"]
                       ["kp4" "<kbd>Keypad 4</kbd>"]
                       ["kp5" "<kbd>Keypad 5</kbd>"]
                       ["kp6" "<kbd>Keypad 6</kbd>"]
                       ["kp7" "<kbd>Keypad 7</kbd>"]
                       ["kp8" "<kbd>Keypad 8</kbd>"]
                       ["kp9" "<kbd>Keypad 9</kbd>"]

                       ["kp+" "<kbd>Keypad +</kbd>"]
                       ["kp-" "<kbd>Keypad -</kbd>"]
                       ["kp*" "<kbd>Keypad *</kbd>"]
                       ["kp/" "<kbd>Keypad /</kbd>"]

                       ["←" "<kbd>←</kbd>"]
                       ["→" "<kbd>→</kbd>"]
                       ["↑" "<kbd>↑</kbd>"]
                       ["↓" "<kbd>↓</kbd>"]
                       ["Home" "<kbd>↖ Home</kbd>"]
                       ["End" "<kbd>↘ End</kbd>"]
                       ["PageUp" "<kbd>⇞ Page △</kbd>"]
                       ["Page Up" "<kbd>⇞ Page △</kbd>"]
                       ["PgUp" "<kbd>⇞ Page △</kbd>"]
                       ["PageDown" "<kbd>⇟ Page ▽</kbd>"]
                       ["Page Down" "<kbd>⇟ Page ▽</kbd>"]
                       ["PgDn" "<kbd>⇟ Page ▽</kbd>"]
                       ["insert" "<kbd>Insert</kbd>"]
                       ["ins" "<kbd>Insert</kbd>"]
                       ["pause" "<kbd>Pause</kbd>"]
                       ["break" "<kbd>Break</kbd>"]

                       ["‹key›" "<kbd>‹key›</kbd>"]
                       ])
         )

    (let ((case-fold-search t) (case-replace nil) )
      (save-restriction
        (narrow-to-region p1 p2)
        (xhm-emacs-to-windows-kbd-notation (point-min) (point-max) )

(goto-char (point-min))

        (replace-pairs-region (point-min) (point-max) replaceList)
        (replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["\+\\([^<]\\) \\(.\\) \\(.\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd> <kbd>\\3</kbd>"]
          ["\+\\([^<]\\) \\([A-Za-z0-0]\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd>"]
          ["\+\\([^<]\\)" "+<kbd>\\1</kbd>"]
          ])
        ) ) )

  ;; test cases
  ;; 【Ctrl+x a】
  ;; 【Ctrl+x a b】
  ;; 【Ctrl+x Ctrl+j】
  )

(defvar xhm-html-tag-input-history nil "for input history of `xhm-wrap-html-tag'")
(setq xhm-tag-input-history (list) )

(defvar xhm-class-input-history nil "for input history of `xhm-wrap-html-tag'")
(setq xhm-class-input-history (list) )

(defvar xhm-class-input-history nil "for input history of `xhm-wrap-html-tag'")
(setq xhm-class-input-history (list) )

(defun xhm-add-open/close-tag (tagName className p1 p2)
  "Add HTML open/close tags around region boundary p1 p2.
This function does not `save-excursion'.
"
  (let* (
        (classStr (if (or (equal className nil) (string= className "") ) "" (format " class=\"%s\"" className)))
        (insStrLeft (format "<%s%s>" tagName classStr) )
        (insStrRight (format "</%s>" tagName ) )
        )

      (goto-char p1)

      (if (xhm-tag-self-closing-p tagName)
          (progn (insert (format "<%s%s />" tagName classStr) ))
        (progn
          ;; (setq myText (buffer-substring-no-properties p1 p2)
          (insert insStrLeft )
          (goto-char (+ p2 (length insStrLeft)))
          (insert insStrRight )
          )
        )
      ) )

(defun xhm-wrap-html-tag (tagName &optional className)
  "Insert/wrap a HTML tags to text selection or current word/line/text-block.
When there's not text selection, the tag will be wrapped around current word/line/text-block, depending on the tag used.

If current line or word is empty, then insert open/end tags and place cursor between them.

If `universal-argument' is called first, then also prompt for a “class” attribute. Empty value means don't add the attribute.
"
  (interactive
   (list
    (ido-completing-read "HTML tag:" xhm-html5-tag-list "PREDICATE" "REQUIRE-MATCH" nil xhm-html-tag-input-history "span")
    (if current-prefix-arg
        (read-string "class:" nil xhm-class-input-history "")
      nil ) ) )
  (let (bds p1 p2
            lineWordBlock
            )
    (progn
      (setq lineWordBlock (xhm-get-tag-type tagName) )
      (setq bds
            (cond
             ((equal lineWordBlock "w") (get-selection-or-unit 'word))
             ((equal lineWordBlock "l") (get-selection-or-unit 'line))
             ((equal lineWordBlock "b") (get-selection-or-unit 'block))
             (t (get-selection-or-unit 'word))
             ))
      (setq p1 (elt bds 1) )
      (setq p2 (elt bds 2) )
      (xhm-add-open/close-tag tagName className p1 p2)

      (when ; put cursor between when input text is empty
          (and (equal p1 p2) (not (xhm-tag-self-closing-p tagName)))
          (progn (search-backward "</" ) )
         )
 ) ) )

(defun xhm-pre-source-code (&optional langCode)
  "Insert/wrap a <pre class=\"‹langCode›\"> tags to text selection or current text block.
"
  (interactive
   (list
    (ido-completing-read "lang code:" (mapcar (lambda (x) (car x)) xhm-lang-name-map) "PREDICATE" "REQUIRE-MATCH" nil xhm-html-tag-input-history "code")
    )
   )
  (let (bds p1 p2)
    (setq bds (get-selection-or-unit 'block))
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )
    (xhm-add-open/close-tag "pre" langCode p1 p2))
  )

(defun xhm-rename-html-inline-image (ξnewFilePath)
  "Replace current HTML inline image's file name.

When cursor is in HTML link file path, e.g.  <img src=\"gki/macosxlogo.png\" > and this command is called, it'll prompt user for a new name. The link path will be changed to the new name, the corresponding file will also be renamed. The operation is aborted if a name exists."

  (interactive
   (let (
         (defaultInput (expand-file-name
                        (elt (get-selection-or-unit 'filepath) 0)
                        (file-name-directory (or (buffer-file-name) default-directory )) )) )
     (list (read-string "New name: " defaultInput nil defaultInput )) ) )
  (let* (
         (bds (get-selection-or-unit 'filepath))
         (ξinputPath (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         (ξffp (local-url-to-file-path (expand-file-name ξinputPath (file-name-directory (or (buffer-file-name) default-directory )) ))) ;full path
         ;; (setq ξffp (windows-style-path-to-unix (local-url-to-file-path ξffp)))
         )

    (if (file-exists-p ξnewFilePath)
        (progn (error "file 「%s」 exist." ξnewFilePath ))
      (progn
        (rename-file ξffp ξnewFilePath )
        (message "rename to %s" ξnewFilePath)
        (delete-region p1 p2)
        (insert (xahsite-filepath-to-href-value ξnewFilePath (or (buffer-file-name) default-directory)))
        )
      )
    ))

(defun xhm-clean-whitespace ()
  "Delete redundant whitespace in HTML file.
Work on text selection or whole buffer.
This is heuristic based, does not remove ALL possible redundant whitespace."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'buffer))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         )
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "[ \t]+\n" nil "noerror")
            (replace-match "\n") ))

        (progn
          (goto-char (point-min))
          (while (search-forward-regexp " *<p>\n+" nil "noerror")
            (replace-match "<p>") ))

        )) ))



;; define the mode
(define-derived-mode xah-html-mode fundamental-mode
  "ξhtml"
  "A simple major mode for HTML5.
HTML5 keywords are colored.

see file header for currrent status.

\\{xhm-keymap}"

  (setq xhm-font-lock-keywords
        (let (
              (htmlElementNamesRegex (regexp-opt xhm-html5-tag-list 'words))
              (htmlAttributeNamesRegexp (regexp-opt xhm-attribute-names 'words))
              (cssPropertieNames (regexp-opt xhm-css-property-names 'words) )
              (cssValueNames (regexp-opt xhm-css-value-kwds 'words) )
              (cssColorNames (regexp-opt xhm-css-color-names 'words) )
              (cssUnitNames (regexp-opt xhm-css-unit-names 'words) )

;              (attriRegex " *= *\"\\([ -_a-z]*?\\)\"")
;              (attriRegex " +\\(?:[ =\"-_a-z]*?\\)") ; one or more attributes
              (attriRegex " +\\(?:[^\n<>]*?\\)") ; one or more attributes
;              (textNodeRegex "\\([ -_A-Za-z]+?\\)")
;              (textNodeRegex "\\([ [:graph:]]+?\\)")
              (textNodeRegex "\\([^\n<]+?\\)") ; ← hack, to avoid multi-line
              )
          `(

            (,htmlElementNamesRegex . font-lock-function-name-face)
            (,htmlAttributeNamesRegexp . font-lock-variable-name-face)
            (,cssPropertieNames . font-lock-type-face)
            (,cssValueNames . font-lock-keyword-face)
            (,cssColorNames . font-lock-preprocessor-face)
            (,cssUnitNames . font-lock-reference-face)

            ;; todo these multiline regex are bad. see elisp manual
            ("<!--\\|-->" . font-lock-comment-delimiter-face)
            (,(format "<!--%s-->" textNodeRegex) . (1 font-lock-comment-face))
            (,(format "“%s”" textNodeRegex) . (1 'xhm-curly“”-quoted-text-face))
            (,(format "‘%s’" textNodeRegex) . (1 'xhm-curly‘’-quoted-text-face))

            (,(format "<span%s>%s</span>" attriRegex textNodeRegex) . (1 "hi-pink"))
            (,(format "<mark%s>%s</mark>" attriRegex textNodeRegex) . (1 "hi-yellow"))
            (,(format "<b%s>%s</b>" attriRegex textNodeRegex) . (1 "bold"))
            (,(format "<h\\([1-6]\\)>%s</h\\1>" textNodeRegex) . (2 "bold"))
            (,(format "<title>%s</title>" textNodeRegex) . (1 "bold"))

            ) ) )

  (setq font-lock-defaults '((xhm-font-lock-keywords)))

  (set-syntax-table xhm-syntax-table)
  (use-local-map xhm-keymap)

  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")

  ;;  (setq mode-name "xah-html")
  (run-mode-hooks 'xah-html-mode-hook)
  )

(provide 'xah-html-mode)
