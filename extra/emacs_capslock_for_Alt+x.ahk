; 2011-01-10
;  Xah ∑ http://xahlee.org/ ☄ 

SetCapsLockState, off
CapsLock::Return

;; emacs hotkeys
#IfWinActive ahk_class Emacs

CapsLock::Send !x ; call execute-extended-command
