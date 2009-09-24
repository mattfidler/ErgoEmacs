; ErgoEmacs Installer for Windows
;
; To compile this .iss with Inno Setup, the expected layout of directories is:
;
; <root>
;   |
;   |- ErgoEmacs-trunk/
;   |- emacs-23.1/
;   |- msys/
;   `- hunspell/
;
; More information here:
; http://code.google.com/p/ergoemacs/wiki/CreatingErgoEmacsWindowsInstaller
; 

[Setup]
AppName=ErgoEmacs
AppVerName=ErgoEmacs 1.6
AppPublisherURL=http://code.google.com/p/ergoemacs/
AppSupportURL=http://code.google.com/p/ergoemacs/
AppUpdatesURL=http://code.google.com/p/ergoemacs/
DefaultDirName={pf}\ErgoEmacs
DefaultGroupName=ErgoEmacs
AllowNoIcons=yes
LicenseFile=..\..\emacs-23.1\COPYING
OutputDir=.
OutputBaseFilename=ErgoEmacs 1.6 Setup
SetupIconFile=..\..\emacs-23.1\etc\icons\emacs.ico
;Compression=none
Compression=lzma
SolidCompression=yes
VersionInfoVersion=1.6

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\..\emacs-23.1\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\..\msys\*"; DestDir: "{app}\msys"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\..\hunspell\*"; DestDir: "{app}\hunspell"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\packages\*"; Excludes: "*~,#*#"; DestDir: "{app}\packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\ergoemacs\*"; Excludes: "*~,#*#"; DestDir: "{app}\ergoemacs"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "ErgoEmacs.exe"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\ErgoEmacs"; Filename: "{app}\ErgoEmacs.exe"
Name: "{commondesktop}\ErgoEmacs"; Filename: "{app}\ErgoEmacs.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\ErgoEmacs.exe"; Description: "{cm:LaunchProgram,ErgoEmacs}"; Flags: nowait postinstall skipifsilent

