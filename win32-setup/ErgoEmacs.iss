; ErgoEmacs Installer for Windows
;
; To compile this .iss with Inno Setup, the expected layout of directories is:
;
; <root>
;   |
;   |- ErgoEmacs-trunk/
;   |- emacs-23.2/
;   |- msys/
;   `- hunspell/
;
; More information here:
; http://code.google.com/p/ergoemacs/wiki/CreatingErgoEmacsWindowsInstaller
; 

[Setup]
AppName=ErgoEmacs
AppVerName=ErgoEmacs 1.8
AppPublisherURL=http://ergoemacs.org/
AppSupportURL=http://ergoemacs.org/
AppUpdatesURL=http://ergoemacs.org/
DefaultDirName={pf}\ErgoEmacs
DefaultGroupName=ErgoEmacs
AllowNoIcons=yes
LicenseFile=..\..\emacs-23.2\COPYING
OutputDir=.
OutputBaseFilename=ErgoEmacs 1.8 Setup
SetupIconFile=..\..\emacs-23.2\etc\icons\emacs.ico
;Compression=none
Compression=lzma
SolidCompression=yes
VersionInfoVersion=1.8
ChangesAssociations=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: associate; Description: "&Associate .el files with ErgoEmacs"; GroupDescription: "File Associations:"; Flags: unchecked

[Files]
Source: "..\..\emacs-23.2\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\..\msys\*"; DestDir: "{app}\msys"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\..\hunspell\*"; DestDir: "{app}\hunspell"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\packages\*"; Excludes: "*~,#*#"; DestDir: "{app}\packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\ergoemacs\*"; Excludes: "*~,#*#"; DestDir: "{app}\ergoemacs"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\site-lisp\*"; Excludes: "*~,#*#"; DestDir: "{app}\site-lisp"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "ErgoEmacs.exe"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\ErgoEmacs"; Filename: "{app}\ErgoEmacs.exe"
Name: "{commondesktop}\ErgoEmacs"; Filename: "{app}\ErgoEmacs.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\ErgoEmacs.exe"; Description: "{cm:LaunchProgram,ErgoEmacs}"; Flags: nowait postinstall skipifsilent

[Registry]
Root: HKCR; Subkey: ".el"; ValueType: string; ValueName: ""; ValueData: "ErgoEmacsFile"; Flags: uninsdeletevalue; Tasks: associate
Root: HKCR; Subkey: "ErgoEmacsFile"; ValueType: string; ValueName: ""; ValueData: "ErgoEmacs File"; Flags: uninsdeletekey; Tasks: associate
Root: HKCR; Subkey: "ErgoEmacsFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\ErgoEmacs.exe,0"; Tasks: associate
Root: HKCR; Subkey: "ErgoEmacsFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\ErgoEmacs.exe"" ""%1"""; Tasks: associate

