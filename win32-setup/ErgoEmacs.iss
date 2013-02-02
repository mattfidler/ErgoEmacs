; ErgoEmacs Installer for Windows
;
; To compile this .iss with Inno Setup, the expected layout of directories is:
;
; <root>
;   |
;   |- bin-extra/
;   |- emacs-24.2/
;   |- ergoemacs/            <-- Git clone
;   |- hunspell/
;   `- msys/
;
; More information here:
; http://code.google.com/p/ergoemacs/wiki/CreatingErgoEmacsWindowsInstaller
; 

#define AppVersion    "2.0.0"
#define EmacsDir      "..\..\emacs-24.2"
#define MsysDir       "..\..\msys"
#define HunspellDir   "..\..\hunspell"
#define BinExtraDir   "..\..\bin-extra"

[Setup]
AppId=ErgoEmacs
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
Source: "{#BinExtraDir}\*"; DestDir: "{app}\bin"; Flags: ignoreversion recursesubdirs createallsubdirs
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

[Code]
// The code to uninstall the previous version was taken from: http://stackoverflow.com/a/2099805/408239
function GetUninstallString(): String;
var
  sUnInstPath: String;
  sUnInstallString: String;
begin
  sUnInstPath := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\{#emit SetupSetting("AppId")}_is1');
  sUnInstallString := '';
  if not RegQueryStringValue(HKLM, sUnInstPath, 'UninstallString', sUnInstallString) then
    RegQueryStringValue(HKCU, sUnInstPath, 'UninstallString', sUnInstallString);
  Result := sUnInstallString;
end;

function IsUpgrade(): Boolean;
begin
  Result := (GetUninstallString() <> '');
end;

function UnInstallOldVersion(): Integer;
var
  sUnInstallString: String;
  iResultCode: Integer;
begin
// Return Values:
// 1 - uninstall string is empty
// 2 - error executing the UnInstallString
// 3 - successfully executed the UnInstallString

  // default return value
  Result := 0;

  // get the uninstall string of the old app
  sUnInstallString := GetUninstallString();
  if sUnInstallString <> '' then begin
    sUnInstallString := RemoveQuotes(sUnInstallString);
    if Exec(sUnInstallString, '/SILENT /NORESTART /SUPPRESSMSGBOXES','', SW_HIDE, ewWaitUntilTerminated, iResultCode) then
      Result := 3
    else
      Result := 2;
  end else
    Result := 1;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if (CurStep=ssInstall) then
  begin
    if (IsUpgrade()) then
    begin
      UnInstallOldVersion();
    end;
  end;
end;
