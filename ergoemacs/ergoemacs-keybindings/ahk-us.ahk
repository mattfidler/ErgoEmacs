;-*- coding: utf-8 -*-
;; Ergohotkey
;; A AutoHotkey script for system-wide ErgoEmacs keybinding
;;
;;   Copyright © 2009 Milan Santosi
;;   Copyright © 2013 Matthew Fidler
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
;; Version 0.6:
;; - Unified Script, fixed kill-line-backwards
;; Version 0.5:
;; - Made this generated inside of ergoemacs.  Malfunctioning kill-line-backwards re-included.
;; Version 0.4: 
;; - Fixed a missing colon, that prevents Alt+i to work. Xah Lee
;; Version 0.3:
;; - added a #SingleInstance directive. Xah Lee
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
#Persistent  ; Keep the script running until the user exits it.


LayLst=
VarLst=
CareL = 0
CareV = 0
CareLV = 0

IniRead CurrLayout, ergoemacs.ini, Curr, Layout
If (CurrLayout == "ERROR"){
  CurrLayout=us
}
IniRead CurrVariant, ergoemacs.ini, Curr, Variant
If (CurrVariant == "ERROR"){
  CurrVariant=Standard
} 


CurrLayVar= % "[" . CurrLayout . "-" . CurrVariant . "]"
Loop, Read, ergoemacs.ini
{
  If (A_LoopReadLine == "[Layouts]"){
     CareL = 1
     CareV = 0
  } Else If (A_LoopReadLine == "[Variants]"){
     CareV = 1
     CareL = 0
  } Else If (CareL == 1 || CareV == 1){
    tmp = %A_LoopReadLine%
    NextSec := InStr(tmp,"[")
    If (NextSec == 1){
      CareL = 0
      CareV = 0
    } else {
      NextSec := InStr(tmp,"=")
      If (NextSec != 0){
          NextSec := SubStr(tmp,1,NextSec-1)
          If (CareL == 1){
            LayLst  = %LayLst%%NextSec%`n
          } else {
            VarLst  = %VarLst%%NextSec%`n
          }          
      }
    }
  } Else If (A_LoopReadLine == CurrLayVar){
    CareLV = 1
  } Else If (CareLV == 1){
    tmp = %A_LoopReadLine%
    NextSec := InStr(tmp,"[")
    If (NextSec == 1){
      CareLV = 0
    } else {
      NextSec := InStr(tmp,"=")
      If (NextSec != 0){
        fn := SubStr(tmp,1,NextSec - 1)
        NextSec := SubStr(tmp,NextSec + 1)
        HotKey, %NextSec%, %fn%
      }
    }
  }
}


; Create Menu

Loop, parse, LayLst, `n 
{
        If (A_LoopField != ""){
           Menu, MenuKey, add, %A_LoopField%, MenuKeyHandler
           If (A_LoopField == CurrLayout){
              Menu, MenuKey, Check, %A_LoopField%
           } else {
             Menu, MenuKey, UnCheck, %A_LoopField%
           }
        }
}

Loop, parse, VarLst, `n
{
        If (A_LoopField != ""){
           Menu, VariantKey, add, %A_LoopField%, VariantKeyHandler
           
           If (A_LoopField == CurrVariant){
              Menu, VariantKey, Check, %A_LoopField%
           } else {
             Menu, VariantKey, UnCheck, %A_LoopField%
           }

        }
}

Menu, Tray, DeleteAll
Menu, Tray, NoStandard
Menu, tray, add, Keyboard Layouts, :MenuKey
Menu, tray, add, Variants, :VariantKey
Menu, tray, add, Exit, Exit
return

VariantKeyHandler:
IniWrite,%A_ThisMenuItem%,ergoemacs.ini,Curr,Variant
Reload
return

MenuKeyHandler:
IniWrite,%A_ThisMenuItem%,ergoemacs.ini,Curr,Layout
Reload
return

Exit:
ExitApp
return