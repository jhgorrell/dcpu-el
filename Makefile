#
# ~/projects/games/0x10c/dcpu-el/Makefile ---
#
# $Id: Makefile,v 1.3 2012/04/10 20:04:09 harley Exp $
#

_default: _all

EL_FILES:=$(wildcard *.el)
ELC_FILES=${EL_FILES:%.el=%.elc}

%.elc: %.el
	emacs --batch  -L . -f batch-byte-compile ${<}

_elc: ${ELC_FILES}

clean:
	rm *.elc *~

_cli_1:
	./dcpu-cli ../programs/test-1.bin

_all: _elc
