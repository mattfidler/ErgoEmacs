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
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
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
  ' (0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
     0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
     0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
     0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
     0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
     0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
     0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
     0 0 0 1 2 3 3 4 4 5 6 7 7 7 7)
  "Track the finger based on the ergoemacs-layout.
0 = left pinky,
1 = left ring
2 = left middle
3 = left pointer
4 = right pointer
5 = right middle
6 = right ring
7 = right pinky
")

;; These are taken from http://www.colemak.com/wiki/index.php?title=Compare
(defvar ergoemacs-key-width 18.0
  "Assumption of key width (in px)")

(defvar ergoemacs-key-height 22.0
  "Assumption of key height (in px)")

(defvar ergoemacs-tab-key-width 28.0
  "Assumption of key width (in px)")

(defvar ergoemacs-lock-key-width 34.0
  "Assumption of lock key width (in px)")

(defvar ergoemacs-shift-key-width 26.0
  "Assumption of shift key width (in px)")

(defvar ergoemacs-return-key-width 36.0
  "Assumption of return key width (in px)")

(defvar ergoemacs-tab-key-width 28.0
  "Assumption of tab key width (in px)")

(defvar ergoemacs-key-width-m 0.010
  "Default key width (in m)")

(defvar ergoemacs-keyboard-coordinates-x nil
  "Keyboard x-coordinates (in m)")

(defvar ergoemacs-keyboard-coordinates-y nil
  "Keyboard y-coordinates (in m)")

(defun ergoemacs-calculate-keyboard-coordinates ()
  "Calculates `ergoemacs-keyboard-coordinates-x' and
`ergoemacs-keyboard-coordintes-y'"
  (setq ergoemacs-keyboard-coordinates-x
        (let ((i 0)
              (last 0)
              curr)
          (mapcar
           (lambda(x)
             (setq i (+ i 1))
             (setq curr (+ last (/ ergoemacs-tab-key-width 2)))
             (cond
              ((or (= 17 i) (= 58 i))
               (setq last ergoemacs-tab-key-width))
              ((or (= 34 i) (= 75 i))
               (setq last ergoemacs-lock-key-width))
              ((or (= 41 i) (= 92 i))
               (setq last ergoemacs-shift-key-width))
              (t
               (setq last (+ last ergoemacs-key-width))))
             (* (/ ergoemacs-key-width-m ergoemacs-key-width) curr))
           ergoemacs-track-finger)))
  
  (setq ergoemacs-keyboard-coordinates-y
        (let ((i 0)
              (last 0)
              curr)
          (mapcar
           (lambda(x)
             (setq i (+ i 1))
             (setq curr (+ last (/ ergoemacs-tab-key-width 2)))
             (cond
              ((= 58 i)
               (setq last 0))
              ((or (= 17 i) (= 34 i) (= 75 i)(= 41 i) (= 92 i))
               (setq last (+ last ergoemacs-tab-key-width))))
             (* (/ ergoemacs-key-width-m ergoemacs-key-width) curr))
           ergoemacs-track-finger))))

(ergoemacs-calculate-keyboard-coordinates)

(defun ergoemacs-key-properties (key layout &optional curr-i)
  "Key the KEY properties based on ergoemacs LAYOUT"
  (let ((i 0)
        (lay (intern-soft (format "ergoemacs-layout-%s" layout)))
        ret)
    (when lay
      (if curr-i
          (setq wi curr-i)
        (mapc
         (lambda(x)
           (when (string= key x)
             (setq wi i))
           (setq i (+ i 1)))
         (symbol-value lay)))
      (setq i wi)
     (setq ret
           `(:x ,(nth i ergoemacs-keyboard-coordinates-x)

                :y ,(nth i ergoemacs-keyboard-coordinates-y)

                :x-home ,(nth (if (<= (nth i ergoemacs-track-finger) 3)
                                  (+ 32 (nth i ergoemacs-track-finger))
                                (+ 38 (- (nth i ergoemacs-track-finger) 4)))
                              ergoemacs-keyboard-coordinates-x)

                :y-home ,(nth (if (<= (nth i ergoemacs-track-finger) 3)
                                  (+ 32 (nth i ergoemacs-track-finger))
                                (+ 38 (- (nth i ergoemacs-track-finger) 4)))
                              ergoemacs-keyboard-coordinates-y)
                
                :hand ,(if (= 0 (nth i ergoemacs-track-hand))
                           'left
                         'right)

                :finger ,(cond
                          ((or (= 0 (nth i ergoemacs-track-finger))
                               (= 7 (nth i ergoemacs-track-finger)))
                           'pinky)
                          ((or (= 1 (nth i ergoemacs-track-finger))
                               (= 6 (nth i ergoemacs-track-finger)))
                           'ring)
                          ((or (= 2 (nth i ergoemacs-track-finger))
                               (= 5 (nth i ergoemacs-track-finger)))
                           'middle)
                          (t
                           'pointer))

                :finger-n ,(nth i ergoemacs-track-finger)

                :row-n ,(nth i ergoemacs-track-row)

                :row ,(cond
                       ((= 1 (nth i ergoemacs-track-row))
                        'number)
                       ((= 2 (nth i ergoemacs-track-row))
                        'top)
                       ((= 3 (nth i  ergoemacs-track-row))
                        'home)
                       ((= 4 (nth i ergoemacs-track-row))
                        'bottom))))
     (symbol-value 'ret))))

(defvar ergoemacs-key-hash nil
  "Key hash")

(setq ergoemacs-key-hash (make-hash-table :test 'equal))

(mapc
 (lambda(layout)
   (let ((lay (intern-soft (format "ergoemacs-layout-%s" layout))))
     (when lay
       (mapc
        (lambda(key)
          (unless (string= key "")
            (puthash (cons layout key)
                     (ergoemacs-key-properties key layout)
                     ergoemacs-key-hash)))
        (symbol-value lay)))))
 (ergoemacs-get-layouts t))

(defun ergoemacs-key-distance (key1 key2 layout)
  "Gets the key distance based on the layout."
  (let ((kp1 (gethash (cons layout key1) ergoemacs-key-hash))
        (kp2 (gethash (cons layout key2) ergoemacs-key-hash))
        dx dy d dh
        (ret 0))
    (cond
     ((eq (plist-get kp1 :finger-n) (plist-get kp2 :finger-n))
      (setq dx (- (plist-get kp1 :x) (plist-get kp2 :x)))
      (setq dy (- (plist-get kp1 :y) (plist-get kp2 :y)))
      (setq d (sqrt (+ (* dx dx) (* dy dy))))
      (setq dx (- (plist-get kp1 :x-home) (plist-get kp2 :x)))
      (setq dy (- (plist-get kp1 :y-home) (plist-get kp2 :y)))
      (setq dh (sqrt (+(* dx dx) (* dy dy))))
      (message "%s %s" d dh))
     (t
      ;; different fingers.
      (setq dx (- (plist-get kp1 :x) (plist-get kp1 :x-home)))
      (setq dy (- (plist-get kp1 :y) (plist-get kp1 :y-home)))
      (setq d (* 2 (sqrt (+ (* dx dx) (* dy dy)))))
      (setq dx (- (plist-get kp2 :x) (plist-get kp2 :x-home)))
      (setq dy (- (plist-get kp2 :y) (plist-get kp2 :y-home)))
      (setq dh (sqrt (+ (* dx dx) (* dy dy))))
      (seq d (+ dh d))
      (message "%s; %s" d dh)
      ))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-track.el ends here
