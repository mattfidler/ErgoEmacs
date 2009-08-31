ifndef EMACS
EMACS = emacs
endif

RM = rm
ELC_FILES = $(wildcard *.elc) $(wildcard */*.elc)
EL_FILES = $(wildcard *.el) $(wildcard */*.el)

all:
	-$(RM) $(ELC_FILES)
	-$(EMACS) -batch -f batch-byte-compile $(EL_FILES)
