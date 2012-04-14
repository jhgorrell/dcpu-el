#
# ~/projects/games/0x10c/dcpu-el/Makefile ---
#
# $Id: Makefile,v 1.5 2012/04/14 04:58:59 harley Exp $
#

# export JHG_CLOAD_ENABLE=0

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


##########

# git setup
# git remote add origin git@github.com:jhgorrell/dcpu-el.git

_github_push:
	git push -u origin master
