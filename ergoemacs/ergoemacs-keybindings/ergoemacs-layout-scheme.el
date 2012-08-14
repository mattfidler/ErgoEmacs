;-*- coding: utf-8 -*-

;; 2012-08-14 ignore this file. this is work in progress.

(ergoemacs-get-key "dv" 35 ['meta 'shift 'control])
Should return the return value of 「(kbd "M-e")」

(defun ergoemacs-get-key (languageCode, keyCode)
  "DOCSTRING

 1  2 [ 3  4  5  6  7] [ 8  9 10 11 12] 13 14 15
16 17 [18 19 20 21 22] [23 24 25 26 27] 28 29 30
31 32 [33 34 35 36 37] [38 39 40 41 42] 43 44 45
46 47 [48 49 50 51 52] [53 54 55 56 57] 58 59 60

"
  (interactive)
  (let ()

    ;; read the file
    ;; split by "\n\n"
    ;; for each text block, split by "\n"
    ;; for each line, ignore those starting with #
    ;; then, the first line should start with 「layout:」
    ;; second line should start with 「modifier:」

  ))





[
["1" "2" "3" "4" "5"  "6" "7" "8" "9" "0"]
["q" "w" "e" "r" "t"  "y" "u" "i" "o" "l"]
["a" "s" "d" "f" "g"  "h" "j" "k" "l" ";"]
["z" "x" "c" "v" "b"  "n" "m" "," "." "/"]
]

