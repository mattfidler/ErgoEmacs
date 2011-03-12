# How to release a new ErgoEmacs distribution?
#
# Just run "make update-version VERSION=x.x.x"
# And then "make compile"

CURRENT_VERSION = $(shell cat ergoemacs/init_version.el \
			| grep "defconst ergoemacs-version " \
			| sed -e "s/^.*ergoemacs-version \"*\([0-9.]*\).*/\1/")

RC_VERSION = $(shell echo $(VERSION) | sed -e "s/\\./,/g" -e "s/$$/,0/")
RC_VERSION2 = $(shell echo $(RC_VERSION) | sed -e "s/,/, /g")

ifndef EMACS
EMACS = emacs
endif

RM = rm
ELC_FILES = $(wildcard *.elc) $(wildcard */*.elc) $(wildcard */*/*.elc)

PACKAGE_AUTOCOMPLETE = auto-complete-1.3.1
PACKAGE_BOOKMARKPLUS = bookmarkplus
PACKAGE_DICTIONARY = dictionary-1.8.7
PACKAGE_ERLANG = erlang
PACKAGE_HASKELL = haskell-mode-2.7.0
PACKAGE_POV = pov-mode-3.2
PACKAGE_HUNSPELL = rw-hunspell
PACKAGE_SCALA = scala-mode
PACKAGE_TUAREG = tuareg-mode-1.45.7
PACKAGE_YASNIPPET = yasnippet-0.6.1c

all:
	@echo "Usage:"
	@echo "  make compile"
	@echo "    Byte-compiles all .el files with the specified emacs in EMACS variable."
	@echo "    E.g. You can use it as:"
	@echo "      make compile EMACS=../emacs-23.3/bin/emacs.exe"
	@echo
	@echo "  make clean"
	@echo "    Removes all .elc files."
	@echo
	@echo "  make show-version"
	@echo "    Shows the current ErgoEmacs version."
	@echo
	@echo "  make update-version VERSION=x.x.x"
	@echo "    Changes the ErgoEmacs version from all files."
	@echo

# Byte-compiles all .el files
compile:
	-$(RM) -f $(ELC_FILES)
	$(EMACS) -L packages \
		 -L packages/$(PACKAGE_AUTOCOMPLETE) \
		 -L packages/$(PACKAGE_BOOKMARKPLUS) \
		 -L packages/$(PACKAGE_ERLANG) \
		 -L packages/$(PACKAGE_SCALA) \
		 -L packages/$(PACKAGE_YASNIPPET) \
		 -batch -f batch-byte-compile ergoemacs/*.el
	-$(EMACS) -batch -f batch-byte-compile ergoemacs/ergoemacs-keybindings/*.el
	-$(EMACS) -batch -f batch-byte-compile packages/*.el
	$(EMACS) -L packages/$(PACKAGE_AUTOCOMPLETE) -batch -f batch-byte-compile packages/$(PACKAGE_AUTOCOMPLETE)/*.el
	$(EMACS) -L packages/$(PACKAGE_BOOKMARKPLUS) -batch -f batch-byte-compile packages/$(PACKAGE_BOOKMARKPLUS)/*.el
	$(EMACS) -L packages/$(PACKAGE_DICTIONARY) -batch -f batch-byte-compile packages/$(PACKAGE_DICTIONARY)/*.el
	$(EMACS) -L packages/$(PACKAGE_ERLANG) -batch -f batch-byte-compile packages/$(PACKAGE_ERLANG)/*.el
	$(EMACS) -L packages/$(PACKAGE_HASKELL) -batch -f batch-byte-compile packages/$(PACKAGE_HASKELL)/*.el
	$(EMACS) -L packages/$(PACKAGE_POV) -batch -f batch-byte-compile packages/$(PACKAGE_POV)/*.el
	$(EMACS) -L packages/$(PACKAGE_HUNSPELL) -batch -f batch-byte-compile packages/$(PACKAGE_HUNSPELL)/*.el
	$(EMACS) -L packages/$(PACKAGE_SCALA) -batch -f batch-byte-compile packages/$(PACKAGE_SCALA)/*.el
	$(EMACS) -L packages/$(PACKAGE_TUAREG) -batch -f batch-byte-compile packages/$(PACKAGE_TUAREG)/*.el
	$(EMACS) -L packages/$(PACKAGE_YASNIPPET) -batch -f batch-byte-compile packages/$(PACKAGE_YASNIPPET)/*.el
	$(EMACS) -batch -f batch-byte-compile site-lisp/*.el

# Removes all .elc files
clean:
	-$(RM) -f $(ELC_FILES)

# Displays the current ErgoEmacs version
show-version:
	@echo $(CURRENT_VERSION)

# Changes the ErgoEmacs version
update-version:
	cat ergoemacs/init_version.el \
		| sed -e "s/defconst ergoemacs-version \"[0-9.]*\"/defconst ergoemacs-version \"$(VERSION)\"/" \
		> tmp
	mv tmp ergoemacs/init_version.el
	cat win32-setup/ErgoEmacs.iss \
		| sed -e "s/\(#define AppVersion *\)\"[0-9.]*\"/\1\"$(VERSION)\"/" \
		> tmp
	mv tmp win32-setup/ErgoEmacs.iss
	cat win32-setup/ErgoEmacs.rc \
		| sed -e "s/ FILEVERSION [0-9, ]*/ FILEVERSION $(RC_VERSION)/" \
		      -e "s/ PRODUCTVERSION [0-9, ]*/ PRODUCTVERSION $(RC_VERSION)/" \
		      -e "s/VALUE \"FileVersion\",.*/VALUE \"FileVersion\", \"$(RC_VERSION2)\\\\0\"/" \
		      -e "s/VALUE \"ProductVersion\",.*/VALUE \"ProductVersion\", \"$(RC_VERSION2)\\\\0\"/" \
		> tmp
	mv tmp win32-setup/ErgoEmacs.rc
