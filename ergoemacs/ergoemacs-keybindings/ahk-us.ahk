;-*- coding: utf-8 -*-
;; Ergohotkey
;; A AutopairHotkey script for system-wide ErgoEmacs keybinding
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
;; Version 0.7:
;; - Added Caps lock to Menu in emacs.
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
#Persistent  ; Keep the script running until the user exits it.
IniRead CurrCaps, ergoemacs.ini, Caps, App

LayLst=
VarLst=
CareL = 0
CareV = 0
CareLV = 0

SendNonEmacs(key){
 If WinActive("ahk_class Emacs") {
   pressedKey := SubStr(A_ThisHotkey,0)
   modifiers := GetModifiers()
   Suspend On
   SendInput % modifiers pressedKey
   Suspend Off 
 } Else {
  SendInput %key%
 }   
}

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


HotKey,Capslock,capslock-handle
;; HotKey,(,autopair-paren


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
Menu, Tray, add, Caps to Menu in Emacs, ToggleCaps
If (CurrCaps == "1"){
  Menu, Tray, Check, Caps to Menu in Emacs
}
Menu, tray, add, Exit, Exit
return

ToggleCaps:
If (CurrCaps == "1"){
   IniWrite,0,ergoemacs.ini,Caps,App
} Else {
   IniWrite,1,ergoemacs.ini,Caps,App
}
Reload
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


autopair-paren:
  ClipSave := ClipboardAll
  Clipboard := ; Clear the Clipboard
  SendInput {Ctrl down}{c}{Ctrl up}
  ClipWait
  if (Clipboard = "") {
     ;; Nothing copied
     SendInput (){Left}
  } else {
    SendInput {Ctrl down}{x}{Ctrl up}({Ctrl down}{v}{Control up}){Left}
    Clipboard := ClipSave
  }
  ClipSaved = ; Free memory in case the clipboard was large
  return 


capslock-handle:
  If (WinActive("ahk_class Emacs") && CurrCaps == "1") {
     SendInput {AppsKey}
  } else {
    Suspend On
    SendInput {Capslock}
    Suspend Off
  }
  return



previous-line:

  SendNonEmacs("{Up}")
  return


next-line:

  SendNonEmacs("{Down}")
  return


backward-char:

 SendNonEmacs("{Left}")
 return


forward-char:

 SendNonEmacs("{Right}")
 return


backward-word:

 SendNonEmacs("{Ctrl down}{Left}{Ctrl up}")
  return


forward-word:

  SendNonEmacs("{Ctrl down}{Right}{Ctrl up}")
  return


move-beginning-of-line:

  SendNonEmacs("{Home}")
  return


move-end-of-line:

 SendNonEmacs("{End}")
 return


delete-backward-char:

 SendNonEmacs("{Backspace}")
  return


delete-char:

 SendNonEmacs("{Delete}")
 return


scroll-down:

 SendNonEmacs("{PgUp}")
 return


scroll-up:

 SendNonEmacs("{PgDn}")
 return


isearch-forward:

  SendNonEmacs("{Ctrl down}{f}{Ctrl Up}")
 return


query-replace:

  SendNonEmacs("{Ctrl down}{h}{Ctrl Up}")
 return


backward-kill-word:
 SendNonEmacs("{Shift down}{Ctrl down}{Left}{Ctrl up}{Shift up}{Ctrl down}{x}{Ctrl up}")
  return


kill-word:

  SendNonEmacs("{Ctrl down}{Shift down}{Right}{Ctrl up}{Shift up}{Ctrl down}{x}{Ctrl up}")
  return


kill-line:

 SendNonEmacs("{Shift down}{End}{Shift up}{Ctrl down}{x}{Ctrl up}")
  return


ergoemacs-kill-line-backward:

  SendNonEmacs("{Shift down}{Home}{Shift up}{Ctrl down}{x}{Ctrl up}")
  return


ergoemacs-cut-line-or-region:

 SendNonEmacs("{Ctrl down}{x}{Ctrl up}")
  return


ergoemacs-copy-line-or-region:

 SendNonEmacs("{Ctrl down}{c}{Ctrl up}")
  return


yank:

 SendNonEmacs("{Ctrl down}{v}{Ctrl up}")
  return


undo:

  SendNonEmacs("{Ctrl down}{z}{Ctrl up}")
  return


redo:

 SendNonEmacs("{Ctrl down}{y}{Ctrl up}")
  return


;; Get Modifiers taken from https://github.com/benhansenslc/BigCtrl/blob/master/BigCtrl.ahk
; Return the hotkey symbols (ie !, #, ^ and +) for the modifiers that
; are currently activated
GetModifiers()
{
  Modifiers =
  GetKeyState, state1, LWin
  GetKeyState, state2, RWin
  state = %state1%%state2%
  if state <> UU  ; At least one Windows key is down.
    Modifiers = %Modifiers%# 
  GetKeyState, state1, Alt
  if state1 = D
    Modifiers = %Modifiers%!
  GetKeyState, state1, Control
  if state1 = D
    Modifiers = %Modifiers%^
  GetKeyState, state1, Alt
  GetKeyState, state1, Shift
  if state1 = D
    Modifiers = %Modifiers%+
  Return Modifiers
}

;; Copyright information for BiGCtl

; Copyright (c) 2012 Benjamin Hansen
;
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.