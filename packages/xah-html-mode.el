;;-*- coding: utf-8 -*-
;; xah-html-mode.el -- Major mode for editing pure html5.

;;; HISTORY
;; version 0.1, 2011-07-29

(defvar xhm-mode-hook nil "Standard hook for `xhm-mode' (xah html mode)")

(defvar xhm-keywords nil "HTML keywords.")

(setq xhm-keywords
'("a" "abbr" "acronym" "address" "applet" "area" "article" "aside" "audio" "b" "base" "basefont" "bdi" "bdo" "bgsound" "big" "blockquote" "body" "br" "button" "canvas" "caption" "center" "cite" "code" "col" "colgroup" "command" "datalist" "dd" "del" "details" "dfn" "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure" "font" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins" "kbd" "keygen" "label" "legend" "li" "link" "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object" "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span" "strike" "strong" "style" "sub" "summary" "sup" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "wbr" "xmp" ))

(defvar xhm-keywords-regexp nil "regex for font locking")
(setq xhm-keywords-regexp (regexp-opt xhm-keywords 'words))

(setq xhm-keywords nil)                 ; no longer needed

(setq xhm-font-lock-keywords
  `(
    (,xhm-keywords-regexp . font-lock-keyword-face)
    ("〔\\([^ ]+?\\) " . (1 font-lock-function-name-face))
    ("“\\([^ ]+?\\)”" . (1 font-lock-string-face))
    ("「\\([^」]+\\)」" . (1 font-lock-variable-name-face))

))

;; define the mode
(define-derived-mode xah-html-mode fundamental-mode
  "xah-html"
  "Major mode for editing LSL (Linden Scripting Language)…"

  (setq font-lock-defaults '((xhm-font-lock-keywords)))

  ;; clear memory
  (setq xhm-keywords-regexp nil)
  (require 'sgml-mode )

  (setq mode-name "xah-html")
  (run-mode-hooks 'xhm-mode-hook)
)

(provide 'xah-html-mode)
