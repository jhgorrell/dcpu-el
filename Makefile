#
# ~/projects/games/0x10c/dcpu-el/Makefile ---
#
# $Id: Makefile,v 1.7 2012/04/17 04:27:06 harley Exp $
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

# gem install org-ruby
README.html: README.org
	org-ruby README.org > ${@}

_README.html: README.html
	open README.html

##########

# git setup
# git remote add origin git@github.com:jhgorrell/dcpu-el.git

_github_push:
	git push -u origin master

##########

A16_EXE:=dcpu16.git/a16

dcpu16.git:
	git clone https://github.com/swetland/dcpu16.git ${@}

${A16_EXE}: | dcpu16.git
	cd dcpu16.git && make
#####

%.bin: %.dasm | ${A16_EXE}
	${A16_EXE} -o ${@} -O binary $<
%.hex: %.dasm | ${A16_EXE}
	${A16_EXE} -o ${@} -O hex $<
%.pretty: %.dasm | ${A16_EXE}
	${A16_EXE} -o ${@} -O pretty $<

##########

