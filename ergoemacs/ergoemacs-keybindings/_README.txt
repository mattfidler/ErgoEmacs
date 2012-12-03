-*- coding: utf-8; mode: org -*-

* Layouts
All the layouts in ergoemacs are easy to generate.  To add your own
personal layout you just need to match the keybindings for your in a
layout variable from =ergoemacs-layout-XXX=.  For the US and UK
layouts, the defining variable adds the layout:

#+BEGIN_SRC emacs-lisp
  (defvar ergoemacs-layout-us
    '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
      "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"
      "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "" ""
      "" ""  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
      ;; Shifted
      "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
      "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"
      "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"" "" ""
      "" ""  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
    "US Engilsh QWERTY Keyboard")
  
  (defvar ergoemacs-layout-gb
    '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
      "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" ""
      "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "#" ""
      "" "\\"  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
      ;; Shifted
      "" "¬" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
      "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" ""
      "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "@" "~" ""
      "" "|"  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
    "UK QWERTY")
#+END_SRC

This lists the keyboard positions from left to right for the unshifted
and shifted states of he keyboard.  After listing the keyboard
descriptions it provides a description of the layout which is used for
the customization variable `ergoemacs-keyboard-layout'.  By simply
defining your layout before ergoemacs-mode is loaded, you add it to
the ergoemacs-keyboard-layout variable with its description. 
* Customizing/Saving the variables
You can customiae the ergoemacs keybindings by typing M-x
customize-group ergoemacs-keybindings

* Extras
To generate keyboard binding diagrams and scripts that allow you to
use ergoemacs elsewhewe, please type M-x ergoemacs-extra.  These
scripts will be stored under the extras directory.

Note that if you use an alternative layout on a QWERTY keyboard (such
as colemak), and use the portable colemak layout, to use ergoemacs
keys use the us layout not the colemak layout.  However, if you
installed the colemak keyboard layout to your system, use the colemak
not the US layout variant.

