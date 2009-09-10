; ErgoEmacs Installer for Windows
;
; To compile this .iss with Inno Setup, the expected layout of directories is:
;
; <root>
;   |
;   +- ErgoEmacs-trunk/
;   |  +- ergoemacs/
;   |  |  +- ergoemacs-keybindings/
;   |  |  +- init.el ...
;   |  +- packages/
;   |  |  +- command-frecuency.el
;   |  |  +- ...
;   |  +- win32-setup/
;   |     +- ErgoEmacs.iss
;   |     +- ErgoEmacs.exe
;   |     +- ...
;   |
;   +- emacs-23.1/
;   |  +- bin/
;   |  +- etc/
;   |  +- info/
;   |  +- leim/
;   |  +- lisp/
;   |  +- site-lisp/
;   |  +- ...
;   |
;   +- msys/
;   |  +- bin/
;   |  +- ...
;   |
;   +- hunspell/
;      +- hunspell.exe
;      +- en_US.dic
;      +- en_US.aff
;      +- ...
;
; The GNU Emacs 23.1 (emacs-23.1 folder) can be found here:
; - emacs-23.1-bin-i386.zip
;   http://ftp.gnu.org/pub/gnu/emacs/windows/emacs-23.1-bin-i386.zip
;
; The content of msys/ folder is the content of the following files
; uncompressed:
; - msysCORE-1.0.11-bin.tar.gz
;   https://sourceforge.net/projects/mingw/files/MSYS%20Base%20System/Current%20Release_%20MSYS-1.0.11/msysCORE-1.0.11-bin.tar.gz/download
; - coreutils-5.97-MSYS-1.0.11-snapshot.tar.bz2
;   https://sourceforge.net/projects/mingw/files/MSYS%20Base%20System/Current%20Release_%20MSYS-1.0.11/coreutils-5.97-MSYS-1.0.11-snapshot.tar.bz2/download
;   (all the content inside "coreutils-5.97" folder goes to msys folder)
; - make-3.81-MSYS-1.0.11-2.tar.bz2
;   https://sourceforge.net/projects/mingw/files/MSYS%20Base%20System/Current%20Release_%20MSYS-1.0.11/make-3.81-MSYS-1.0.11-2.tar.bz2/download
;
; Hunspell can be found here:
; - hunspell-1.2.8-win32.zip
;   https://sourceforge.net/projects/hunspell/files/Hunspell/1.2.8/hunspell-1.2.8-win32.zip/download
; - Dictionaries can be found here:
;   http://wiki.services.openoffice.org/wiki/Dictionaries
; 

[Setup]
AppName=ErgoEmacs
AppVerName=ErgoEmacs 1.5.1
AppPublisherURL=http://code.google.com/p/emacs2010/
AppSupportURL=http://code.google.com/p/emacs2010/
AppUpdatesURL=http://code.google.com/p/emacs2010/
DefaultDirName={pf}\ErgoEmacs
DefaultGroupName=ErgoEmacs
AllowNoIcons=yes
LicenseFile=..\..\emacs-23.1\COPYING
OutputDir=.
OutputBaseFilename=ErgoEmacs 1.5.1 Setup
SetupIconFile=..\..\emacs-23.1\etc\icons\emacs.ico
;Compression=none
Compression=lzma
SolidCompression=yes
VersionInfoVersion=1.5.1

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

