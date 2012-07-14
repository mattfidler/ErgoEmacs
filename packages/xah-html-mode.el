;;; xah-html-mode.el --- Major mode for editing pure html5. -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2012-05-12
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; Commentary:
;; Major mode for editing pure HTML5 files. Alpha stage.

;;; HISTORY
;; version 0.5, 2012-05-13 fixed sgml-skip-tag-forward sgml-skip-tag-backward. But sgml-delete-tag still doesn't work.
;; version 0.4, 2012-05-13 added sgml-delete-tag sgml-skip-tag-forward sgml-skip-tag-backward.
;; version 0.3, 2012-05-13 added comment handling. improved syntax coloring. Added keymap and syntax table."
;; version 0.2, 2012-05-12 first version

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")


;; syntax coloring related

(setq xhm-font-lock-keywords
(let (
(htmlElementNamesRegex (regexp-opt '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside" "audio" "b" "base" "basefont" "bdi" "bdo" "bgsound" "big" "blockquote" "body" "br" "button" "canvas" "caption" "center" "cite" "code" "col" "colgroup" "command" "datalist" "dd" "del" "details" "dfn" "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure" "font" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins" "kbd" "keygen" "label" "legend" "li" "link" "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object" "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span" "strike" "strong" "style" "sub" "summary" "sup" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "wbr" "xmp" "doctype") 'words))
(AttributeNamesRegexp (regexp-opt '( "id" "class" "style" "title" "href" "type" "rel" "http-equiv" "content" "charset" "alt" "src" "width" "height" "controls" "autoplay" "preload" ) 'words))
 )
`(
;; ("\"\\([^\"]+?\\)\"" . (1 font-lock-string-face))
("<!--\\|-->" . font-lock-comment-delimiter-face)
("<!--\\([^-]+?\\)-->" . (1 font-lock-comment-face))
("“\\([^”]+?\\)”" . (1 font-lock-string-face))
("「\\([^」]+\\)」" . (1 font-lock-string-face))

("<b>\\([- A-Za-z]+?\\)</b>" . (1 "bold"))
("<h[1-6]>\\([^<]+?\\)</h[1-6]>" . (1 "bold"))
("<title>\\([^<]+?\\)</title>" . (1 "bold"))
(,htmlElementNamesRegex . font-lock-function-name-face)
(,AttributeNamesRegexp . font-lock-keyword-face)
) ) )


;; keybinding


(defvar xhm-keymap nil "Keybinding for `xah-html-mode'")
(progn
  (setq xhm-keymap (make-sparse-keymap))
  (define-key xhm-keymap [remap comment-dwim] 'xhm-comment-dwim)
  (define-key xhm-keymap (kbd "C-c /") 'sgml-close-tag)
  (define-key xhm-keymap (kbd "C-c C-d") 'xhm-delete-tag)
  (define-key xhm-keymap (kbd "C-c <delete>") 'sgml-delete-tag)
  (define-key xhm-keymap (kbd "C-c C-r") 'xhm-skip-tag-forward)
  (define-key xhm-keymap (kbd "C-c C-g") 'xhm-skip-tag-backward)
)


;; syntax table
(defvar xhm-syntax-table nil "Syntax table for `xah-html-mode'.")
(setq xhm-syntax-table
      (let ((synTable (make-syntax-table)))

;; (progn                                  ; all on US keyboard

;; (modify-syntax-entry ?\" "\"" synTable)
;; (modify-syntax-entry ?' "w" synTable)

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
;; (modify-syntax-entry ?< "(" synTable)
;; (modify-syntax-entry ?> ")" synTable)

;; (modify-syntax-entry ?| "." synTable)
;; (modify-syntax-entry ?` "." synTable)
;; (modify-syntax-entry ?~ "." synTable)
;; )

        (modify-syntax-entry ?< "." synTable)
        (modify-syntax-entry ?> "." synTable)
        (modify-syntax-entry ?' "w" synTable)


;; (modify-syntax-entry ?“ "\"" synTable)
;; (modify-syntax-entry ?” "\"" synTable)


        synTable))



(require 'sgml-mode)

(defun xhm-delete-tag ()
  "Delete the tag under cursor.
Also delete the matching beginning/ending tag."
  (interactive)
(save-excursion 
  ;; search for current tag.
  ;; find left nearest >, and right nearest <
  ;; or left nearest <, and right nearest >
  ;; determine if it's <…> or >…<.

(let (
(p1-current (point))
p2-left<
p3-left>
p4-right<
p5-right>
cursor>…▮…<-p
cursor<…▮…>-p
 )
(goto-char p1-current)
(search-backward "<")
(setq p2-left< (point) )

(goto-char p1-current)
(search-backward ">")
(setq p3-left> (point) )

(goto-char p1-current)
(search-forward "<")
(setq p4-right< (point) )

(goto-char p1-current)
(search-forward ">")
(setq p5-right> (point) )

(when
    (and 
     (< p2-left< p3-left>)
     (< p4-right< p5-right>)
     )
    (setq cursor>…▮…<-p  t )
  )

(when
    (and 
     (< p3-left> p2-left< )
     (< p5-right> p4-right< )
     )
    (setq cursor<…▮…>-p  t )
  )

)
 

)
;  (sgml-delete-tag 1)

)

(defun xhm-skip-tag-forward ()
  "Move cursor to the closing tag."
  (interactive)
  (sgml-skip-tag-forward 1))

(defun xhm-skip-tag-backward ()
  "Move cursor to the beginning tag."
  (interactive)
  (sgml-skip-tag-backward 1))

(defun xhm-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "<!--") (comment-end "-->"))
     (comment-dwim arg)))



;; define the mode
(define-derived-mode xah-html-mode fundamental-mode
  "xah-html"
  "A simple major mode for HTML5.
HTML5 keywords are colored.
Basically that's it.

\\{xhm-keymap}"

  (setq font-lock-defaults '((xhm-font-lock-keywords)))

  (set-syntax-table xhm-syntax-table)
  (use-local-map xhm-keymap)

;;  (setq mode-name "xah-html")
  (run-mode-hooks 'xah-html-mode-hook)
)

(provide 'xah-html-mode)
