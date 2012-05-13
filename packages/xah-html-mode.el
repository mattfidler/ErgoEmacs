;;-*- coding: utf-8 -*-
;; xah-html-mode.el -- Major mode for editing pure html5.

;;; HISTORY
;; version 0.4, 2012-05-13 added sgml-delete-tag sgml-skip-tag-forward sgml-skip-tag-backward.
;; version 0.3, 2012-05-13 added comment handling. improved syntax coloring. Added keymap and syntax table."
;; version 0.2, 2012-05-12 first version

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")


;; syntax coloring related

(setq xhm-font-lock-keywords
(let (
(htmlElementNamesRegex (regexp-opt '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside" "audio" "b" "base" "basefont" "bdi" "bdo" "bgsound" "big" "blockquote" "body" "br" "button" "canvas" "caption" "center" "cite" "code" "col" "colgroup" "command" "datalist" "dd" "del" "details" "dfn" "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure" "font" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins" "kbd" "keygen" "label" "legend" "li" "link" "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object" "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span" "strike" "strong" "style" "sub" "summary" "sup" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "wbr" "xmp" "doctype") 'words))
(AttributeNamesRegexp (regexp-opt '( "id" "class" "style" "title" "href" "type" "rel" "http-equiv" "content" "charset" "alt" "src" "width" "height" ) 'words))
 )
`(
(,htmlElementNamesRegex . font-lock-function-name-face)
(,AttributeNamesRegexp . font-lock-keyword-face)
("“\\([^ ]+?\\)”" . (1 font-lock-string-face))
("「\\([^」]+\\)」" . (1 font-lock-string-face))
("<!--\\|-->" . font-lock-comment-delimiter-face)
("<!--\\([^-]+?\\)-->" . (1 font-lock-comment-face))
) ) )


;; keybinding


(defvar xhm-keymap nil "Keybinding for `xah-html-mode'")
(progn
  (setq xhm-keymap (make-sparse-keymap))
  (define-key xhm-keymap [remap comment-dwim] 'xhm-comment-dwim)
  (require 'sgml-mode)
  (define-key xhm-keymap (kbd "C-c C-d") 'sgml-delete-tag)
  (define-key xhm-keymap (kbd "C-c C-r") 'sgml-skip-tag-forward)
  (define-key xhm-keymap (kbd "C-c C-g") 'sgml-skip-tag-backward)
 )


;; syntax table
(defvar xhm-syntax-table nil "Syntax table for `xah-html-mode'.")
(setq xhm-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?< "." synTable)
        (modify-syntax-entry ?> "." synTable)
        synTable))



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
  "Major mode for HTML."

  (setq font-lock-defaults '((xhm-font-lock-keywords)))

  (set-syntax-table xhm-syntax-table)
  (use-local-map xhm-keymap)

;;  (setq mode-name "xah-html")
  (run-mode-hooks 'xah-html-mode-hook)
)

(provide 'xah-html-mode)
