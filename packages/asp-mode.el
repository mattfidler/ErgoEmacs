;;; asp-mode-el -- Major mode for editing ASP files
 
;; Author: Chris Done <chrisdone@gmail.com>
;; Created: 22 March 2009
;; Keywords: ASP major-mode
 
;; Based on file based on WPDL-Mode Tutorial by Scott Andrew Borton
 
;; Copyright (C) 2009 Chris Done <chrisdone@gmail.com>
;; Copyright (C) 2004 Christoph Kepper <kepper@diefirma.de>
;; Copyright (C) 2000, 2003 Scott Andrew Borton <scott@pp.htv.fi>
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
 
;;; COMMENTARY
;;
;; Based on a file which is in turn based on a file for demonstrating modes.
;;;; This mode is based on an example by Scott Andrew Borton used in a
;;;; tutorial about Emacs mode creation. The tutorial can be found here:
;;;; http://two-wugs.net/emacs/mode-tutorial.html
;;;;
;;;; This is my first Emacs-mode - please be patient, if things don't work
;;;; as planned
 
;;; INSTALLATION
;;
;; To automatically load files with a .asp extension, put the following
;; lines in your .emacs file:
;; (autoload 'asp-mode "asp-mode")
;; (setq auto-mode-alist
;; (cons '("\\.asp\\'" . asp-mode) auto-mode-alist))
 
;;; TODO
;;
;;
 
;;; CODE
(defvar asp-mode-hook nil)
(defvar asp-mode-map
  (let ((asp-mode-map (make-keymap)))
    (define-key asp-mode-map "\C-j" 'newline-and-indent)
    asp-mode-map)
  "Keymap for ASP major mode")
 
(add-to-list 'auto-mode-alist '("\\.asp\\'" . asp-mode))
 
(defconst asp-font-lock-keywords-1
; (insert (regexp-opt '("if" "sub" "function" "then" "else" "end" "while" "do" "for" "each" "in" "set" "wend" "next" "loop" "call" "option" "explicit" "redim" "preserve" "dim" "const" "to" "select" "case" "elseif" "exit" "continue" "class" "new" "execute" "not") t))
  (list
   '("\\<\\(c\\(?:a\\(?:ll\\|se\\)\\|lass\\|on\\(?:st\\|tinue\\)\\)\\|d\\(?:im\\|o\\)\\|e\\(?:ach\\|lse\\(?:if\\)?\\|nd\\|x\\(?:ecute\\|\\(?:plic\\)?it\\)\\)\\|f\\(?:or\\|unction\\)\\|i[fn]\\|loop\\|n\\(?:e\\(?:w\\|xt\\)\\|ot\\)\\|option\\|preserve\\|redim\\|s\\(?:e\\(?:\\(?:lec\\)?t\\)\\|ub\\)\\|t\\(?:hen\\|o\\)\\|w\\(?:end\\|hile\\)\\)\\>" . font-lock-keyword-face))
  "Keywords to highlight in ASP mode. (There aren't many.)")
 
(defconst asp-font-lock-keywords-2
; (insert (regexp-opt '("empty" "nothing" "null" "true" "false" "eof") t))
  (append asp-font-lock-keywords-1
   (list
   '("\\<\\(e\\(?:mpty\\|of\\)\\|false\\|n\\(?:othing\\|ull\\)\\|true\\)\\>" . font-lock-constant-face))))
 
(defconst asp-font-lock-keywords-3
; (insert (regexp-opt '("@codepage" "@enablesessionstate" "@language" "@lcid" "@transaction" "abandon" "session" "addheader" "response" "appendtolog" "response" "application" "application_onend" "application" "application_onstart" "application" "aspcode" "asperror" "aspdescription" "asperror" "asperror" "binaryread" "request" "binarywrite" "response" "buffer" "response" "cachecontrol" "response" "category" "asperror" "charset" "response" "clear" "response" "clientcertificate" "collection" "request" "codepage" "response" "codepage" "session" "column" "asperror" "contents" "collection" "application" "contents" "collection" "session" "contenttype" "response" "cookies" "collection" "request" "cookies" "collection" "response" "create" "server" "description" "asperror" "end" "response" "execute" "server" "expires" "response" "expiresabsolute" "response" "file" "asperror" "flush" "response" "form" "collection" "request" "getlasterror" "server" "htmlencode" "server" "isclientconnected" "response" "lcid" "response" "lcid" "session" "line" "asperror" "lock" "application" "mappath" "server" "number" "asperror" "context" "onendpage" "custom" "onstartpage" "custom" "ontransactionabort" "context" "ontransactioncommit" "context" "pics" "response" "querystring" "collection" "request" "redirect" "response" "remove" "application" "remove" "session" "removeall" "application" "removeall" "session" "request" "response" "scripttimeout" "server" "server" "servervariables" "collection" "request" "session" "session_onend" "session" "session_onstart" "session" "sessionid" "session" "setabort" "context" "setcomplete" "context" "source" "asperror" "statics" "collection" "application" "statics" "collection" "session" "status" "response" "timeout" "session" "totalbytes" "request" "transfer" "server" "unlock" "application" "urlencode" "server" "write") t))
  (append asp-font-lock-keywords-2
   (list
   '("\\<\\(@\\(?:codepage\\|enablesessionstate\\|l\\(?:anguage\\|cid\\)\\|transaction\\)\\|a\\(?:bandon\\|ddheader\\|pp\\(?:endtolog\\|lication\\(?:_on\\(?:end\\|start\\)\\)?\\)\\|sp\\(?:code\\|description\\|error\\)\\)\\|b\\(?:inary\\(?:read\\|write\\)\\|uffer\\)\\|c\\(?:a\\(?:checontrol\\|tegory\\)\\|harset\\|l\\(?:ear\\|ientcertificate\\)\\|o\\(?:depage\\|l\\(?:\\(?:lectio\\|um\\)n\\)\\|nte\\(?:nt\\(?:s\\|type\\)\\|xt\\)\\|okies\\)\\|reate\\|ustom\\)\\|description\\|e\\(?:nd\\|x\\(?:ecute\\|pires\\(?:absolute\\)?\\)\\)\\|f\\(?:ile\\|lush\\|orm\\)\\|getlasterror\\|htmlencode\\|isclientconnected\\|l\\(?:cid\\|ine\\|ock\\)\\|mappath\\|number\\|on\\(?:endpage\\|startpage\\|transaction\\(?:\\(?:abor\\|commi\\)t\\)\\)\\|pics\\|querystring\\|re\\(?:direct\\|move\\(?:all\\)?\\|quest\\|sponse\\)\\|s\\(?:cripttimeout\\|e\\(?:rver\\(?:variables\\)?\\|ssion\\(?:_on\\(?:end\\|start\\)\\|id\\)?\\|t\\(?:abort\\|complete\\)\\)\\|ource\\|tat\\(?:\\(?:ic\\|u\\)s\\)\\)\\|t\\(?:imeout\\|otalbytes\\|ransfer\\)\\|u\\(?:nlock\\|rlencode\\)\\|write\\)\\>" . font-lock-builtin-face))))
 
(defvar asp-font-lock-keywords asp-font-lock-keywords-3
  "Default highlighting expressions for ASP mode.")
 
(defun asp-indent-line ()
  "Indent current line as ASP code."
  (interactive)
  (beginning-of-line)
  ;; Regel 1
  (if (bobp)
   (indent-line-to 0)     ; First line is always non-indented
  (let ((not-indented t) cur-indent)
   (if (looking-at "^[ \t]*\\(end.*\\|next\\|loop\\|wend\\)") ; If the line we are looking at is the end of a block, then decrease the indentation
     (progn
      (save-excursion
       (forward-line -1)
       (setq cur-indent (- (current-indentation) default-tab-width)))
      (if (< cur-indent 0) ; We can't indent past the left margin
        (setq cur-indent 0)))
    (save-excursion
     (while not-indented ; Iterate backwards until we find an indentation hint
      (forward-line -1)
      (if (looking-at "^[ \t]*\\(end.*\\|next\\|loop\\|wend\\)") ; This hint indicates that we need to indent at the level of the END_ token
        (progn
         (setq cur-indent (current-indentation))
         (setq not-indented nil))
       (if (looking-at "^[ \t]*\\(if\\|sub\\|function\\|else\\|while\\|do\\|for\\)") ; This hint indicates that we need to indent an extra level
         (progn
          (setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
          (setq not-indented nil))
        (if (bobp)
          (setq not-indented nil)))))))
   (if cur-indent
     (indent-line-to cur-indent)
    (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation
 
 
(defvar asp-mode-syntax-table
  (let ((asp-mode-syntax-table (make-syntax-table)))
  
    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" asp-mode-syntax-table)   
 
  
    ;; Comment start with # and end with a newline
    (modify-syntax-entry ?\' "<")
    (modify-syntax-entry ?\n ">")
    
    asp-mode-syntax-table)
  "Syntax table for asp-mode")
  
(defun asp-mode ()
  "Major Mode for editing ASP files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map asp-mode-map)
  (set-syntax-table asp-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(asp-font-lock-keywords nil t))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'asp-indent-line)
  (set (make-local-variable 'default-tab-width) 4)
  (setq major-mode 'asp-mode)
  (setq mode-name "ASP")
  (run-hooks 'asp-mode-hook))
 
(provide 'asp-mode)
 
;;; asp-mode.el ends here