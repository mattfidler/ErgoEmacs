;-*- coding: utf-8 -*-

;; Ergohotkey, global editing shortcuts for Autohotkey:
;; giving the power of ErgoEmacs to notepad.exe
;;
;;   Copyright (C) 2009 Milan Santosi
;;   This program is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global ergonomic editing command shortcuts for 
;; use with autohotkey <http://www.autohotkey.com/>
;; shortcut layout taken from ErgoEmacs
;; <http://code.google.com/p/ergoemacs/>
;; Further details:
;; <http://xahlee.org/emacs/ergonomic_emacs_keybinding.html/>
;; Version 0.2 for us-dvorak layout,
;; created and tested on Windows 7 evaluation build 7100
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changelog:
;; Version 0.3: 
;; - added a #SingleInstance directive, by Xah Lee
;; Version 0.2: 
;; - 'Fixed' malfunctioning kill-line-backwards by remapping it to
;;   something without a shift modifier. Not very happy about it.
;; - Replaced Send with SendInput
;; - Replaced occurences of DEL with C-x to 'kill' to the clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't run multiple instance of this script
#SingleInstance force

;; try to avoid redundancy  with ErgoEmacs
#IfWinNotActive ahk_class Emacs

;; navigation keys should be on the homerow
!c::
  SendInput {Up}
  return
!h::
  SendInput {Left}
  return
!t::
  SendInput {Down}
  return
!n::
  SendInput {Right}
  return

;; move by word
!g::
  SendInput {Ctrl down}
  SendInput {Left}
  SendInput {Ctrl up}
  return
!r::
  SendInput {Ctrl down}
  SendInput {Right}
  SendInput {Ctrl up}
  return

;; move to home/end
!d::
  SendInput {Home}
  return
+!d::
  SendInput {End}
  return

;; delete char forwards/backwards (no yank needed)
!u::
  SendInput {Delete} 
  return
!e::
  SendInput {Backspace}
  return

;; kill rest of line forwards
!i::				  
  SendInput {Shift down}
  SendInput {End}
  SendInput {Shift up}
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return

;; kill line backwards (shift-home key combo mapped to a !+-key causes trouble)
!x::
  SendInput {Shift down}
  SendInput {Home}
  SendInput {Shift up}
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return 

;; kill word backwards
!.::
  SendInput {Ctrl down}
  SendInput {Shift down}
  SendInput {Left}
  SendInput {Ctrl up}
  SendInput {Shift up}
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return

;; delete word forwards
!p::
  SendInput {Ctrl down}
  SendInput {Shift down}
  SendInput {Right}
  SendInput {Ctrl up}
  SendInput {Shift up}
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return

;; cut copy paste
!q::
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return
!j::
  SendInput {Ctrl down}
  SendInput {c}
  SendInput {Ctrl up}
  return
!k::
  SendInput {Ctrl down}
  SendInput {v}
  SendInput {Ctrl up}
  return

;; undo/redo
!`;::
  SendInput {Ctrl down}
  SendInput {z}
  SendInput {Ctrl up}
  return
+!`;::
  SendInput {Ctrl down}
  SendInput {y}
  SendInput {Ctrl up}
  return


;;Not quite ergo-emacs but still useful to have globally:

;; put parens around a word
!b::
  SendInput {Ctrl down}
  SendInput {Left}
  SendInput {Ctrl up}
  SendInput {(}
  SendInput {Ctrl down}
  SendInput {Right}
  SendInput {Ctrl up}
  SendInput {)}
  return

;; deletes first and last char of a word (removing parens)
!+b::
  SendInput {Ctrl down}
  SendInput {Left}
  SendInput {Ctrl up}
  SendInput {Del}
  SendInput {Ctrl down}
  SendInput {Right}
  SendInput {Ctrl up}
  SendInput {BS}
  return

;; copy rest of line (broken because of '!+' again)
; !+i::				  
;   SendInput {Shift down}
;   SendInput {End}
;   SendInput {Shift up}
;   SendInput {Ctrl down}
;   SendInput {x}
;   SendInput {Ctrl up}
;   return