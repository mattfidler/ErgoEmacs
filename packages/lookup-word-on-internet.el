;-*- coding: utf-8 -*-
;; lookup-word-on-internet.el -- helpful commands for looking up the internet

;; Copyright © 2011 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; DESCRIPTION

;; this package provides some convenient commands for looking up the web.
;; For example: lookup-google, lookup-wikipedia, etc.

;;; REQUIREMENT

;; You need have 2 elisp util 〔xeu_elisp_util.el〕 and 〔xfrp_find_replace_pairs.el〕, available at
;; http://code.google.com/p/ergoemacs/source/browse/trunk/packages/xeu_elisp_util.el
;; http://code.google.com/p/ergoemacs/source/browse/trunk/packages/xfrp_find_replace_pairs.el

;;; INSTALL

;; To install, place this file 〔lookup-word-on-internet.el〕 in the directory 〔~/.emacs.d/〕.

;; also place the files 〔xeu_elisp_util.el〕 and 〔xfrp_find_replace_pairs.el〕 in 〔~/.emacs.d/〕, if you haven't done already.

;; Then, place the following code in your emacs init file

;; (autoload 'lookup-google "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-wikipedia "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-word-dict-org "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-word-definition "lookup-word-on-internet" "Lookup in browser" t)
;; (autoload 'lookup-wiktionary "lookup-word-on-internet" "Lookup word in browser" t)
;; (autoload 'lookup-php-ref "lookup-word-on-internet" "Lookup word in browser" t)

;; ;; Suggested keys
;; (global-set-key (kbd "C-h C-g") 'lookup-google)
;; (global-set-key (kbd "C-h C-w") 'lookup-wikipedia)
;; (global-set-key (kbd "C-h C-d") 'lookup-word-definition)

;;; DOCUMENTATION

;; just some simple useful commands
;; for detail, see http://xahlee.org/emacs/emacs_lookup_ref.html

;;; HISTORY
;; version 1.0, 2011-11-14 First released to public.

;;; Code:


;; § ----------------------------------------
(require 'xeu_elisp_util)

(defun lookup-word-on-internet (&optional input-word site-to-use)
  "Look up current word or text selection in a online dictionary.
This command launches/switchs you to default browser.

Optional argument INPUT-WORD and SITE-TO-USE can be given.
SITE-TO-USE a is URL string in this form: 「http://en.wiktionary.org/wiki/�」.
the 「�」 is a placeholder for the query string.

If SITE-TO-USE is nil, Google Search is used.

For a list of online reference sites, see:
 URL `http://xahlee.org/emacs/emacs_lookup_ref.html'"
  (interactive)
  (let (ξword refUrl myUrl)
    (setq ξword
          (if input-word
              input-word
            (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'symbol) )) )

    (setq ξword (replace-regexp-in-string " " "%20" (asciify-text ξword)))

    (setq refUrl
          (if site-to-use
              site-to-use
            "http://www.google.com/search?q=�" ) )

    (setq myUrl (replace-regexp-in-string "�" ξword refUrl t t))
    (cond
     ((string-equal system-type "windows-nt") ; any flavor of Windows
      (browse-url-default-windows-browser myUrl)
      )
     ((string-equal system-type "gnu/linux")
      (browse-url myUrl)
      )
     ((string-equal system-type "darwin") ; Mac
      (browse-url myUrl) ) ) ))

(defun lookup-google (&optional input-word)
  "See `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.google.com/search?q=�" ))
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-wikipedia (&optional input-word)
  "See `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://en.wikipedia.org/wiki/�" ))
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-word-dict-org (&optional input-word)
  "See `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=�" ))
    (lookup-word-on-internet input-word dictUrl)
    ) )

(defun lookup-word-definition (&optional input-word)
  "See `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://www.answers.com/main/ntquery?s=�" ))
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-wiktionary (&optional input-word)
  "See `lookup-word-on-internet'."
  (interactive)
  (let ((dictUrl "http://en.wiktionary.org/wiki/�" ))
    (lookup-word-on-internet input-word dictUrl) ) )

(defun lookup-php-ref ()
  "Look up current word in PHP's reference doc.
If a there is a text selection (a phrase), lookup that phrase.
Launches default browser and opens the doc's url."
  (interactive)
  (let (inputStr myUrl)
    (setq inputStr (elt (get-selection-or-unit 'word) 0) )
    (setq myUrl (concat "http://us.php.net/" inputStr))
    (browse-url myUrl)))

(provide 'lookup-word-on-internet)
