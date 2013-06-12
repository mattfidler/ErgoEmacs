;;; ergoemacs-track.el --- Minor mode to track layout-based distances typed.
;; 
;; Filename: ergoemacs-track.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Jun 12 08:57:44 2013 (-0500)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar ergoemacs-track-hand
  '(0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      )
  "Based on ergoemcs-layouts, which hand is typing?
0 represents left, 1 represents right.")

(defvar ergoemacs-track-row
  '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
      3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
      4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
      3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
      4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)
  "Based on ergoemacs-layouts, what row is being used?
1 = 1st row/number row
2 = 2nd row

3 = 3rd row/home row
4 = 4th row")

(defvar ergoemacs-track-finger
  '(0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8
     0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8
     0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8
     0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8
     0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8
     0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8
     0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8
     0 0 0 1 2 3 4 4 5 5 6 7 8 8 8 8 8)
  "Track the finger based on the ergoemacs-layout.
0 = left pinky,
1 = left ring
3 = left middle
4 = left pointer
5 = right pointer
6 = right middle
7 = right ring
8 = right pinky
")

;; These are taken from http://www.colemak.com/wiki/index.php?title=Compare
(defvar ergoemacs-key-width 18
  "Assumption of key width (in px)")

(defvar ergoemacs-key-height 22
  "Assumption of key height (in px)")

(defvar ergoemacs-tab-key-width 28
  "Assumption of key width (in px)")

(defvar ergoemacs-lock-key-width 34
  "Assumption of lock key width (in px)")

(defvar ergoemacs-shift-key-width 26
  "Assumption of shift key width (in px)")

(defvar ergoemacs-return-key-width 36
  "Assumption of return key width (in px)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-track.el ends here
