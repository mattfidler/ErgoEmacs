/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
     Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/*
  Main ErgoEmacs program to start Emacs with its console window hidden,
  by David Capello.

  Based on code of runemacs.exe.
*/

#include <windows.h>
#include <string.h>
#include <malloc.h>
#include <stdio.h>
#include <shlobj.h>
#include "string_utils.h"

static const struct {
  DWORD kbdcode;
  const char *file;
  const char *desc;
} kb_layout[] = {
   { 0x00401, "a1",	"Arabic (101)" },
   { 0x10401, "a2",	"Arabic (102)" },
   { 0x20401, "a3",	"Arabic (102) AZERTY" },
   { 0x00402, "bu",	"Bulgarian" },
   { 0x00404, "us",	"Chinese (Traditional) - US Keyboard" },
   { 0x00405, "cz",	"Czech" },
   { 0x10405, "cz1",	"Czech (QWERTY)" },
   { 0x20405, "cz2",	"Czech Programmers" },
   { 0x00406, "da",	"Danish" },
   { 0x00407, "gr",	"German" },
   { 0x10407, "gr1",	"German (IBM)" },
   { 0x00408, "he",	"Greek" },
   { 0x10408, "he220",	"Greek (220)" },
   { 0x20408, "he319",	"Greek (319)" },
   { 0x30408, "hela2",	"Greek (220) Latin" },
   { 0x40408, "hela3",	"Greek (319) Latin" },
   { 0x50408, "gkl",	"Greek Latin" },
   { 0x60408, "hept",	"Greek Polytonic" },
   { 0x00409, "us",	"US English" },
   { 0x10409, "dv",	"US-Dvorak" },
   { 0x20409, "usx",	"US-International" },
   { 0x30409, "usl",	"US-Dvorak for left hand" },
   { 0x40409, "usr",	"US-Dvorak for right hand" },
   { 0x50409, "usa",	"US English (IBM Arabic 238_L)" },
   { 0x0040a, "sp",	"Spanish" },
   { 0x1040a, "es",	"Spanish Variation" },
   { 0x0040b, "fi",	"Finnish" },
   { 0x0040c, "fr",	"French" },
   { 0x0040d, "heb",	"Hebrew" },
   { 0x0040e, "hu",	"Hungarian" },
   { 0x1040e, "hu1",	"Hungarian 101-key" },
   { 0x0040f, "ic",	"Icelandic" },
   { 0x00410, "it",	"Italian" },
   { 0x10410, "it142",	"Italian (142)" },
   { 0x00411, "jpn",	"Japanese" },
   { 0x00412, "ko",	"Korean" },
   { 0x00413, "ne",	"Dutch" },
   { 0x00414, "no",	"Norwegian" },
   { 0x00415, "pl1",	"Polish (Programmers)" },
   { 0x10415, "pl",	"Polish (214)" },
   { 0x00416, "br",	"Portuguese (Brazilian ABNT)" },
   { 0x00418, "ro",	"Romanian" },
   { 0x00419, "ru",	"Russian" },
   { 0x10419, "ru1",	"Russian (Typewriter)" },
   { 0x0041a, "cr",	"Croatian" },
   { 0x0041b, "sl",	"Slovak" },
   { 0x1041b, "sl1",	"Slovak (QWERTY)" },
   { 0x0041c, "al",	"Albanian" },
   { 0x0041d, "sw",	"Swedish" },
   { 0x0041e, "th0",	"Thai Kedmanee" },
   { 0x1041e, "th1",	"Thai Pattachote" },
   { 0x2041e, "th2",	"Thai Kedmanee (non-ShiftLock)" },
   { 0x3041e, "th3",	"Thai Pattachote (non-ShiftLock)" },
   { 0x0041f, "tuq",	"Turkish Q" },
   { 0x1041f, "tuf",	"Turkish F" },
   { 0x00422, "ur",	"Ukrainian" },
   { 0x00423, "blr",	"Belarusian" },
   { 0x00424, "cr",	"Slovenian" },
   { 0x00425, "est",	"Estonian" },
   { 0x00426, "lv",	"Latvian" },
   { 0x10426, "lv1",	"Latvian (QWERTY)" },
   { 0x00427, "lt",	"Lithuanian IBM" },
   { 0x10427, "lt1",	"Lithuanian" },
   { 0x00429, "fa",	"Farsi" },
   { 0x0042a, "vntc",	"Vietnamese" },
   { 0x0042b, "arme",	"Armenian Eastern" },
   { 0x1042b, "armw",	"Armenian Western" },
   { 0x0042c, "azel",	"Azeri Latin" },
   { 0x0042f, "mac",	"Macedonian (FYROM)" },
   { 0x00437, "geo",	"Georgian" },
   { 0x00438, "fo",	"Faeroese" },
   { 0x00439, "indev",	"Devanagari - INSCRIPT" },
   { 0x10439, "inhin",	"Hindi Traditional" },
   { 0x0043f, "kaz",	"Kazakh" },
   { 0x00444, "tat",	"Tatar" },
   { 0x00449, "intam",	"Tamil" },
   { 0x0044e, "inmar",	"Marathi" },
   { 0x00804, "us",	"Chinese (Simplified) - US Keyboard" },
   { 0x00807, "sg",	"Swiss German" },
   { 0x00809, "uk",	"United Kingdom" },
   { 0x0080a, "la",	"Latin American" },
   { 0x0080c, "be",	"Belgian French" },
   { 0x1080c, "bene",	"Belgian (Comma)" },
   { 0x00813, "be",	"Belgian Dutch" },
   { 0x00816, "po",	"Portuguese" },
   { 0x0081a, "ycl",	"Serbian (Latin)" },
   { 0x0082c, "aze",	"Azeri Cyrillic" },
   { 0x00843, "uzb",	"Uzbek Cyrillic" },
   { 0x00c0c, "fc",	"Canadian French (Legacy)" },
   { 0x10c0c, "can",	"Canadian Multilingual Standard" },
   { 0x00c1a, "ycc",	"Serbian (Cyrillic)" },
   { 0x10c1a, "ycl",	"Serbian (Latin)" },
   { 0x01009, "ca",	"Canadian French" },
   { 0x11009, "can",	"Canadian Multilingual Standard" },
   { 0x0100c, "sf",	"Swiss French" },
   { 0x01809, "ir",	"Irish" },
   { 0x11809, "gae",	"Gaelic" },
   { 0x00000, NULL,	NULL }
};

static char emacs_dir[MAX_PATH];

static int get_emacs_dir ();
static void set_home_dir ();
static int run_client (LPSTR cmdline);
static int run_server (LPSTR cmdline);
static int exe_cmdline (LPSTR cmdline, int wait_for_child, DWORD priority_class);
static void prepend_path (String* s, const char* emacs_dir, const char* subdir);

int WINAPI
WinMain (HINSTANCE hSelf, HINSTANCE hPrev, LPSTR cmdline, int nShow)
{
  int code;

  code = get_emacs_dir ();
  if (code == 0)
    {
      set_home_dir ();

      code = run_client (cmdline);
      if (code != 0)
	{
	  code = run_server (cmdline);
	}
    }

  if (code == -1)
    {
      MessageBox (NULL, "Could not start ErgoEmacs.", "Error", MB_ICONSTOP);
      code = 1;
    }

  return code;
}

static int get_emacs_dir()
{
  char *p;

  if (!GetModuleFileName (NULL, emacs_dir, MAX_PATH))
    return -1;

  if ((p = strrchr (emacs_dir, '\\')) == NULL)
    return -1;

  *p = 0;
  return 0;
}

static void set_home_dir ()
{
  char home[MAX_PATH];

  /* If HOME is not set, set it as "C:\Documents and Settings\username" */
  if (!GetEnvironmentVariable ("HOME", home, MAX_PATH) || !*home)
    {
      HRESULT hr = SHGetFolderPath (NULL, CSIDL_PROFILE, NULL, 0, home);
      if (SUCCEEDED (hr))
	{
	  SetEnvironmentVariable ("HOME", home);
	}
    }
}

/* Run emacsclient.exe to connect to some current server to open the
   file specified in the command line.  */
static int run_client (LPSTR cmdline)
{
  char home[MAX_PATH];
  DWORD ret_code = 0;
  String *new_cmdline = string_create ("");

  /* Quote executable name in case of spaces in the path. */
  string_append (new_cmdline, "\"");
  string_append (new_cmdline, emacs_dir);
  string_append (new_cmdline, "\\bin\\emacsclient.exe\" ");

  string_append (new_cmdline, " ");
  string_append (new_cmdline, " --no-wait ");
  string_append (new_cmdline, " --server-file \"");
  GetEnvironmentVariable ("HOME", home, MAX_PATH);
  string_append (new_cmdline, home);
  string_append (new_cmdline, "\\.emacs.d\\server\\server\" ");
  string_append (new_cmdline, cmdline);

  ret_code = exe_cmdline (new_cmdline->text, TRUE, NORMAL_PRIORITY_CLASS);

  string_destroy (new_cmdline);
  return ret_code;
}

/* Runs emacs.exe to initialize a new server.  */
static int run_server (LPSTR cmdline)
{
  int wait_for_child = FALSE;
  DWORD priority_class = NORMAL_PRIORITY_CLASS;
  String *new_cmdline = string_create ("");
  int ret_code;
  char *p;

  /* Quote executable name in case of spaces in the path. */
  string_append (new_cmdline, "\"");
  string_append (new_cmdline, emacs_dir);
  string_append (new_cmdline, "\\bin\\emacs.exe\" ");

  /* Put ErgoEmacs as the window caption.  */
  string_append (new_cmdline, " --title ErgoEmacs");

  /* Append original arguments if any; first look for arguments we
     recognise (-wait, -high, and -low), and apply them ourselves.  */
  while (cmdline[0] == '-' || cmdline[0] == '/')
    {
      if (strncmp (cmdline+1, "wait", 4) == 0)
  	{
  	  wait_for_child = TRUE;
  	  cmdline += 5;
  	}
      else if (strncmp (cmdline+1, "high", 4) == 0)
  	{
  	  priority_class = HIGH_PRIORITY_CLASS;
  	  cmdline += 5;
  	}
      else if (strncmp (cmdline+1, "low", 3) == 0)
  	{
  	  priority_class = IDLE_PRIORITY_CLASS;
  	  cmdline += 4;
  	}
      else
  	break;
      /* Look for next argument.  */
      while (*++cmdline == ' ');
    }

  /* Add the original arguments specified by the user (maybe a file to open).  */
  string_append (new_cmdline, " ");
  string_append (new_cmdline, cmdline);

  /* ErgoEmacs: Setup PATH enviroment variable.  */
  {
    int required_length = 256;
    String *path_var = string_create ("");

    /* Get PATH enviroment variable */
    do
      {
	string_reserve (path_var, required_length);

	required_length = GetEnvironmentVariable ("PATH", path_var->text, required_length);
      } while (required_length >= path_var->size);
    path_var->length = strlen(path_var->text);
      
    /* Add to PATH:
       "C:\Program Files\ErgoEmacs\bin"
       "C:\Program Files\ErgoEmacs\msys\bin"
       "C:\Program Files\ErgoEmacs\hunspell"
    */
    prepend_path (path_var, emacs_dir, "bin");
    prepend_path (path_var, emacs_dir, "msys\\bin");
    prepend_path (path_var, emacs_dir, "hunspell");
    SetEnvironmentVariable ("PATH", path_var->text);

    string_destroy (path_var);
  }

  /* ErgoEmacs: Setup keyboard layout.  */
  {
    char buf[256];

    /* If ERGOEMACS_KEYBOARD_LAYOUT is not set.  */
    if (!GetEnvironmentVariable ("ERGOEMACS_KEYBOARD_LAYOUT", buf, sizeof (buf)) || !*buf)
      {
	const char* ergoemacs_layout = "";
	DWORD kbdcode;
	int i;

	GetKeyboardLayoutName (buf);
	kbdcode = strtol (buf, NULL, 16) & 0xfffff;

	for (i = 0; kb_layout[i].file; ++i)
	  {
	    if (kb_layout[i].kbdcode == kbdcode)
	      {
		ergoemacs_layout = kb_layout[i].file;
		break;
	      }
	  }

	SetEnvironmentVariable ("WIN32_KEYBOARD_LAYOUT", buf);
	SetEnvironmentVariable ("ERGOEMACS_KEYBOARD_LAYOUT", ergoemacs_layout);
      }
  }

  /* Set emacs_dir variable.  */
  for (p = emacs_dir; *p; p++)
    if (*p == '\\') 
      *p = '/';
  SetEnvironmentVariable ("emacs_dir", emacs_dir);

  ret_code = exe_cmdline (new_cmdline->text, wait_for_child, priority_class);

  string_destroy (new_cmdline);
  return ret_code;
}

static int exe_cmdline (LPSTR cmdline, int wait_for_child, DWORD priority_class)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  PROCESS_INFORMATION child;
  DWORD ret_code = 0;
  char *p;

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  start.dwFlags = STARTF_USESHOWWINDOW | STARTF_USECOUNTCHARS;
  start.wShowWindow = SW_HIDE;
  /* Ensure that we don't waste memory if the user has specified a huge
     default screen buffer for command windows.  */
  start.dwXCountChars = 80;
  start.dwYCountChars = 25;

  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL;
  sec_attrs.bInheritHandle = FALSE;

  if (CreateProcess (NULL, cmdline, &sec_attrs, NULL, TRUE, priority_class,
		     NULL, NULL, &start, &child))
    {
      if (wait_for_child)
	{
	  WaitForSingleObject (child.hProcess, INFINITE);
	  GetExitCodeProcess (child.hProcess, &ret_code);
	}
      CloseHandle (child.hThread);
      CloseHandle (child.hProcess);
    }
  else
    return -1;

  return (int) ret_code;
}

static void prepend_path (String* s, const char* emacs_dir, const char* subdir)
{
  string_prepend (s, ";");
  string_prepend (s, subdir);
  string_prepend (s, "\\");
  string_prepend (s, emacs_dir);
}
