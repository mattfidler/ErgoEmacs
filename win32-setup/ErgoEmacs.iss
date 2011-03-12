; ErgoEmacs Installer for Windows
;
; To compile this .iss with Inno Setup, the expected layout of directories is:
;
; <root>
;   |
;   |- ErgoEmacs-trunk/
;   |- emacs-23.3/
;   |- msys/
;   `- hunspell/
;
; More information here:
; http://code.google.com/p/ergoemacs/wiki/CreatingErgoEmacsWindowsInstaller
; 

#define AppVersion    "1.9.3"
#define EmacsDir      "..\..\emacs-23.3"
#define MsysDir       "..\..\msys"
#define HunspellDir   "..\..\hunspell"

[Setup]
AppName=ErgoEmacs
AppVerName=ErgoEmacs {#AppVersion}
AppPublisherURL=http://ergoemacs.org/
AppSupportURL=http://ergoemacs.org/
AppUpdatesURL=http://ergoemacs.org/
DefaultDirName={pf}\ErgoEmacs
DefaultGroupName=ErgoEmacs
AllowNoIcons=yes
LicenseFile={#EmacsDir}\COPYING
OutputDir=.
OutputBaseFilename=ErgoEmacs {#AppVersion} Setup
SetupIconFile={#EmacsDir}\etc\icons\emacs.ico
;Compression=none
Compression=lzma
SolidCompression=yes
VersionInfoVersion={#AppVersion}
ChangesAssociations=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: associate; Description: "&Associate .el files with ErgoEmacs"; GroupDescription: "File Associations:"; Flags: unchecked

[Files]
Source: "{#MsysDir}\*"; DestDir: "{app}\msys"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#HunspellDir}\*"; DestDir: "{app}\hunspell"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#EmacsDir}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\packages\*"; Excludes: "*~,#*#"; DestDir: "{app}\packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\ergoemacs\*"; Excludes: "*~,#*#"; DestDir: "{app}\ergoemacs"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\site-lisp\*"; Excludes: "*~,#*#"; DestDir: "{app}\site-lisp"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\extra\*"; Excludes: "*~,#*#"; DestDir: "{app}\extra"; Flags: ignoreversion recursesubdirs createallsubdirs
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
