;;; xah-html-mode.el --- Major mode for editing pure html5. -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2012-05-12
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; Commentary:
;; Major mode for editing pure HTML5 files. Alpha stage.

;;; HISTORY
;; version 0.5.4, 2013-01-26 lots additions and changes. added xhm-wrap-html-tag xhm-wrap-p-tag xhm-lines-to-html-list xhm-make-html-table xhm-wikipedia-linkify xhm-wrap-url xhm-wikipedia-url-linkify xhm-source-url-linkify xhm-make-link-defunct xhm-make-citation xhm-update-title xhm-extract-url xhm-remove-html-tags xhm-remove-span-tag-region xhm-htmlize-keyboard-shortcut-notation
;; version 0.5.3, 2012-12-07 removed loading sgml-mode and all call to its functions. The sgml-mode seems to have bugs about keys. That is, global numberpad keys won't work. 
;; version 0.5.2, 2012-09-25 added a color for curly quoted text.
;; version 0.5, 2012-05-13 fixed sgml-skip-tag-forward sgml-skip-tag-backward. But sgml-delete-tag still doesn't work.
;; version 0.4, 2012-05-13 added sgml-delete-tag sgml-skip-tag-forward sgml-skip-tag-backward.
;; version 0.3, 2012-05-13 added comment handling. improved syntax coloring. Added keymap and syntax table."
;; version 0.2, 2012-05-12 first version

(require 'ido)
(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)

(defvar xah-html-mode-hook nil "Standard hook for `xah-html-mode'")


;; syntax coloring related

(defface xhm-curly-quoted-text-face
  '((((class color) (min-colors 88) (background light)) (:foreground "chartreuse4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "chartreuse2"))
    (((class color) (min-colors 16) (background light)) (:foreground "chartreuse4"))
    (((class color) (min-colors 16) (background dark)) (:foreground "chartreuse2"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face used for curly quoted text."
  :group 'languages)

(setq xhm-font-lock-keywords
(let (
(htmlElementNamesRegex (regexp-opt '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside" "audio" "b" "base" "basefont" "bdi" "bdo" "bgsound" "big" "blockquote" "body" "br" "button" "canvas" "caption" "center" "cite" "code" "col" "colgroup" "command" "datalist" "dd" "del" "details" "dfn" "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure" "font" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins" "kbd" "keygen" "label" "legend" "li" "link" "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object" "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span" "strike" "strong" "style" "sub" "summary" "sup" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "wbr" "xmp" "doctype") 'words))
(AttributeNamesRegexp (regexp-opt '( "id" "class" "style" "title" "href" "type" "rel" "http-equiv" "content" "charset" "alt" "src" "width" "height" "controls" "autoplay" "preload" ) 'words))
 )
`(

;; ("\"\\([^\"]+?\\)\"" . (1 font-lock-string-face))
("<!--\\|-->" . font-lock-comment-delimiter-face)
("<!--\\([^-]+?\\)-->" . (1 font-lock-comment-face))
("“\\([^”]+?\\)”" . (1 'xhm-curly-quoted-text-face))
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
  ;; (define-key xhm-keymap (kbd "C-c /") 'sgml-close-tag)
  (define-key xhm-keymap (kbd "C-c C-d") 'xhm-delete-tag)
  ;; (define-key xhm-keymap (kbd "C-c <delete>") 'sgml-delete-tag)
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



;; (require 'sgml-mode)

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
  ;; (sgml-skip-tag-forward 1)
  )

(defun xhm-skip-tag-backward ()
  "Move cursor to the beginning tag."
  (interactive)
  ;; (sgml-skip-tag-backward 1)
  )

(defun xhm-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "<!--") (comment-end "-->"))
     (comment-dwim arg)))

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
The argument ΞDELIMITER is a char used as the delimiter for columns.

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

When called interactively, use text selection as input, or current text block between empty lines. Output URLs in a buffer named 「*extract URL output*」.

If `universal-argument' 【Ctrl+u】 is called first, tries to convert relative URL to HTTP form.

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
        (mapc (lambda (ξx) (princ ξx) (terpri) ) urlList)
        )
      )
    urlList
    ))

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

If there's a text selection, use it for input, otherwise the input is a text block between empty lines."
  (interactive)
  (let (bds p1 p2 inputText myList ξtitle ξauthor ξdate ξurl )

    (setq bds (get-selection-or-unit 'block))
    (setq inputText (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (setq inputText (replace-regexp-in-string "^[[:space:]]*" "" inputText)) ; remove white space in front

    (setq myList (split-string inputText "[[:space:]]*\n[[:space:]]*" t) )

    (setq ξtitle (trim-string (elt myList 0)))
    (setq ξtitle (replace-regexp-in-string "^\"\\(.+\\)\"$" "\\1" ξtitle))
    (setq ξtitle (replace-pairs-in-string ξtitle '(["’" "'"] ["&" "＆"] )))

    (setq ξauthor (trim-string (elt myList 1)))
    (setq ξdate (trim-string (elt myList 2)))
    (setq ξurl (trim-string (elt myList 3)))

    (setq ξauthor (replace-regexp-in-string "\\. " " " ξauthor)) ; remove period in Initals
    (setq ξauthor (replace-regexp-in-string "By +" "" ξauthor))
    (setq ξauthor (upcase-initials (downcase ξauthor)))
    (setq ξdate (fix-timestamp ξdate))

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
          (search-backward "href=" (- (point) 90)) ; search boundary as extra guard for error
          (forward-char 6)
          (setq p1-url (point))
          (search-forward "\"" (+ p1-url 90))
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

(defun xhm-htmlize-keyboard-shortcut-notation ()
  "Wrap a “kbd” tag around keyboard keys on current text inside 【】, or text selection.
e.g.
 【ctrl+w】
becomes
 【<kbd>Ctrl</kbd>+<kbd>w</kbd>】
Same for Alt, Shift, Cmd, Win, Enter, Return, Home… and other strings."
  (interactive)

  (let (p1 p2 inputStr resultStr replaceList)
    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (save-excursion
        (progn
          (if (search-backward "【" nil t)
              (progn (forward-char)
                     (setq p1 (point) ) )
            (setq p1 (line-beginning-position) )
            )

          (if (search-forward "】" nil t)
              (progn (backward-char)
                     (setq p2 (point) ))
            (setq p2 (line-end-position) ) ) )) )
    (setq inputStr (buffer-substring-no-properties p1 p2))

    (setq replaceList [
 ;; case in find string shouldn't matter.
["Ctrl" "<kbd>Ctrl</kbd>"]
["AltGr" "<kbd>AltGr</kbd>"]
["Alt" "<kbd>Alt</kbd>"]
["Shift" "<kbd>⇧ Shift</kbd>"]
["command" "<kbd>⌘ Cmd</kbd>"]
["Cmd" "<kbd>⌘ Cmd</kbd>"]
["Option" "<kbd>⌥ Opt</kbd>"]
["Opt" "<kbd>⌥ Opt</kbd>"]
["Win" "<kbd>❖ Win</kbd>"]
["App" "<kbd>▤ Menu</kbd>"]
["Menu" "<kbd>▤ Menu</kbd>"]
["Meta" "<kbd>Meta</kbd>"]
["super" "<kbd>Super</kbd>"]

["Return" "<kbd>Return ↩</kbd>"]
["Enter" "<kbd>Enter ↵</kbd>"]
["Backspace" "<kbd>⌫ Backspace</kbd>"]
["Delete" "<kbd>⌦ Delete</kbd>"]
["Del" "<kbd>⌦ Delete</kbd>"]
["Space" "<kbd>Space</kbd>"]
["Caps Lock" "<kbd>Caps Lock</kbd>"]
["CapsLock" "<kbd>Caps Lock</kbd>"]
["F Lock" "<kbd>F Lock</kbd>"]
["Num Lock" "<kbd>Num Lock</kbd>"]
["Tab" "<kbd>Tab ↹</kbd>"]
["Esc" "<kbd>Esc</kbd>"]

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

["‹key›" "<kbd>‹key›</kbd>"]
                       ])

    (let ((case-fold-search t) (case-replace nil)
          )
      (setq resultStr (replace-pairs-in-string inputStr replaceList))
      )

    (setq resultStr (replace-regexp-pairs-in-string resultStr
 [
 ["\+\\([^<]\\) \\(.\\) \\(.\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd> <kbd>\\3</kbd>"]
 ["\+\\([^<]\\) \\([A-Za-z0-0]\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd>"]
 ["\+\\([^<]\\)" "+<kbd>\\1</kbd>"]
 ]))

    (delete-region p1 p2)
    (insert resultStr)
    )

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

(defun xhm-wrap-html-tag (tagName &optional className ξid)
  "Add/Insert a HTML tag to beginning and ending of current word or text selection.

If current line or word is empty, then insert the tag and move cursor into it.

The command will also prompt for a “class” and “id”. Empty value means don't add the attribute.

When called in lisp program, if className is nil or empty string, don't add the attribute. Same for ξid."
  (interactive
(let (
(html5tags '( "a" "abbr" "address" "area" "article" "aside" "audio" "b" "base" "bdi" "bdo" "blockquote" "body" "bq" "br" "button" "canvas" "caption" "cite" "class" "code" "col" "colgroup" "command" "datalist" "dd" "del" "details" "dfn" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "id" "iframe" "img" "input" "ins" "kbd" "keygen" "label" "legend" "li" "link" "mailto" "map" "mark" "menu" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span" "src" "strike" "strong" "style" "sub" "summary" "sup" "t" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "track" "u" "ul" "var" "video" "wbr"))
)
(list

(ido-completing-read "HTML tag:" html5tags "PREDICATE" "REQUIRE-MATCH" nil xhm-html-tag-input-history "span")
        ;; (read-string "Tag (p):" nil nil "p")
        (read-string "class:" nil xhm-class-input-history "")
        (read-string "id:" nil nil "") )
)
    )
  (let (bds p1 p2 inputText outputText
            (classStr (if (or (equal className nil) (string= className "") ) "" (format " class=\"%s\"" className)))
            (idStr (if (or (equal ξid nil) (string= ξid "") ) "" (format " id=\"%s\"" ξid)))
            )
    (setq bds (get-selection-or-unit 'word))
    (setq inputText (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (setq outputText (format "<%s%s%s>%s</%s>" tagName classStr idStr inputText tagName ) )

    (delete-region p1 p2)
    (goto-char p1)
    (insert outputText)
    (when                               ; put cursor between when input text is empty
        (string= inputText "" )
      (progn (search-backward "</" ) )
      )
 ) )

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
