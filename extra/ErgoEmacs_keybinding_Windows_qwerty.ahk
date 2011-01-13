;-*- coding: utf-8 -*-

;; Ergohotkey for QWERTY layout
;; A AutoHotkey script for system-wide ErgoEmacs keybinding
;;
;;   Copyright © 2009 Milan Santosi
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
;;   along with this program.  If not, see http://www.gnu.org/licenses/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global ergonomic editing command shortcuts for 
;; use with autohotkey http://www.autohotkey.com/
;; hotkey layout taken from http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
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

;; Don't activate when in ErgoEmacs (because ErgoEmacs already defines them)
#IfWinNotActive ahk_class Emacs

;; make arrow keys on righthand home position
!i:
  SendInput {Up}
  return
!j::
  SendInput {Left}
  return
!k::
  SendInput {Down}
  return
!l::
  SendInput {Right}
  return

;; move by word
!u::
  SendInput {Ctrl down}
  SendInput {Left}
  SendInput {Ctrl up}
  return
!o::
  SendInput {Ctrl down}
  SendInput {Right}
  SendInput {Ctrl up}
  return

;; move to home/end
!h::
  SendInput {Home}
  return
+!h::
  SendInput {End}
  return

;; delete char forwards/backwards (no yank needed)
!f::
  SendInput {Delete} 
  return
!d::
  SendInput {Backspace}
  return

;; kill rest of line forwards
!g::				  
  SendInput {Shift down}
  SendInput {End}
  SendInput {Shift up}
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return

;; kill line backwards (shift-home key combo mapped to a !+-key causes trouble)
!b::
  SendInput {Shift down}
  SendInput {Home}
  SendInput {Shift up}
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return 

;; kill word backwards
!e::
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
!r::
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
!x::
  SendInput {Ctrl down}
  SendInput {x}
  SendInput {Ctrl up}
  return
!c::
  SendInput {Ctrl down}
  SendInput {c}
  SendInput {Ctrl up}
  return
!v::
  SendInput {Ctrl down}
  SendInput {v}
  SendInput {Ctrl up}
  return

;; undo/redo
!z::
  SendInput {Ctrl down}
  SendInput {z}
  SendInput {Ctrl up}
  return
!+z::
  SendInput {Ctrl down}
  SendInput {y}
  SendInput {Ctrl up}
  return



;;Not quite ergo-emacs but still useful to have globally:

;; put parens around a word
!n::
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
!+n::
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
; !+g::				  
;   SendInput {Shift down}
;   SendInput {End}
;   SendInput {Shift up}
;   SendInput {Ctrl down}
;   SendInput {x}
;   SendInput {Ctrl up}
;   return