;;; insert-random-id.el --- commands to insert random ID. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Maintainer: Xah Lee
;; Created: 2013-04-19
;; Version: 0.1
;; Keywords: convenience

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; Commentary:
;; misc collection of commands to insert hex, digits, of fixed number of digits.

;;; Install:
;; (require 'insert-random-id)

;;; Todo:
;; code needs to be refactored, so that there are a bunch of functions that returns a value, then one interactive command wrapper to them. Also, the command need to accept universal-argument for number of digits to insert.

;;; Change Log:
;; • 0.1 2013-04-19 first version. Moved from my personal init.


(random t) ; set seed

(defun ξ-insert-random-hex ()
  "Insert a random 4-digit hexidecimal number."
  (interactive)
  (let* ((myCharset "1234567890abcdef" )
        (possibleCharsCount (length myCharset)))
    (dotimes (ii 4)
      (insert (elt myCharset (random possibleCharsCount))) ) )
  ;; (insert (format "%4x" (random 65535)) )
  )

(defun ξ-insert-random-string ()
  "Insert a random alphanumerics string of length 5.
The possible chars are 0 to 9, and a to z (lower case)."
  (interactive)
  (let (myCharset (possibleCharsCount 36))
    (setq myCharset "1234567890abcdefghijklmnopqrstuvwxyz" )
    (dotimes (ii 5)
      (insert (elt myCharset (random possibleCharsCount))) ) ) )

(defun ξ-insert-random-uuid ()
  "Insert a random universally unique identifier (UUID).

UUID is a 32 digits hexadecimal formatted in certain way with dash.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)) ) ) )

;; primarily Christopher Wellons. 2011-11-18
(defun ξ-insert-random-uuid-2 ()
  "Insert a UUID. This uses a simple hashing of variable data."
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (buffer-string)
                        (random)
                        (recent-keys)))))

    (insert (format "%s-%s-4%s-a%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))

(defun ξ-insert-random-number ()
  "Insert a random number of length 5."
  (interactive)
  (let (myCharset (possibleCharsCount 10))
    (setq myCharset "1234567890" )
    (dotimes (ii 5)
      (insert (elt myCharset (random possibleCharsCount))) ) ) )

(provide 'insert-random-id)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insert-random-id.el ends here
