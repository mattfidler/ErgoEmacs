; The expected directories layout is:
;
; <root>
;   |
;   +- ErgoEmacs-trunk/
;   |  |
;   |  +- init.el ...
;   |  |
;   |  +- setup/
;   |     |
;   |     +- ErgoEmacs.iss
;   |     +- ErgoEmacs.exe
;   |
;   +- emacs-23.1/
;      |
;      +- bin/
;      +- etc/
;      +- info/
;      +- leim/
;      +- lisp/
;      +- site-lisp/
;      +- ...
;

[Setup]
AppName=ErgoEmacs
AppVerName=ErgoEmacs 23.1.1
AppPublisherURL=http://code.google.com/p/emacs2010/
AppSupportURL=http://code.google.com/p/emacs2010/
AppUpdatesURL=http://code.google.com/p/emacs2010/
DefaultDirName={pf}\ErgoEmacs
DefaultGroupName=ErgoEmacs
AllowNoIcons=yes
LicenseFile=..\..\emacs-23.1\COPYING
OutputDir=.
OutputBaseFilename=ErgoEmacs 23.1.1 Setup
SetupIconFile=..\..\emacs-23.1\etc\icons\emacs.ico
;Compression=none
Compression=lzma
SolidCompression=yes
VersionInfoVersion=23.1.1

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\..\emacs-23.1\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\*"; Excludes: "setup\*,setup\Redist"; DestDir: "{app}\ergoemacs"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "ErgoEmacs.exe"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\ErgoEmacs"; Filename: "{app}\ErgoEmacs.exe"
Name: "{commondesktop}\ErgoEmacs"; Filename: "{app}\ErgoEmacs.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\ErgoEmacs.exe"; Description: "{cm:LaunchProgram,ErgoEmacs}"; Flags: nowait postinstall skipifsilent

