ifndef EMACS
EMACS = emacs
endif

RM = rm
ELC_FILES = $(wildcard *.elc) $(wildcard */*.elc) $(wildcard */*/*.elc)
EL_FILES = $(wildcard *.el) $(wildcard */*.el) $(wildcard */*/*.el)

all:
	@echo "Usage:"
	@echo "  make compile"
	@echo "    Byte-compiles all .el files with the specified emacs in EMACS variable."
	@echo "    E.g. You can use it as:"
	@echo "      make compile EMACS=../emacs-23.1/bin/emacs.exe"
	@echo
	@echo "  make clean"
	@echo "    Removes all .elc files."
	@echo

# Byte-compiles all .el files
compile:
	-$(RM) -f $(ELC_FILES)
	-$(EMACS) -batch -f batch-byte-compile $(EL_FILES)

# Removes all .elc files
clean:
	-$(RM) -f $(ELC_FILES)
