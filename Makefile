#
# ~/projects/games/0x10c/dcpu-el/Makefile ---
#
# $Id: Makefile,v 1.13 2012/05/04 08:14:35 harley Exp $
#

# export JHG_CLOAD_ENABLED=0

_default: _elc

##########

# stuff for just me.
include $(wildcard Makefile.jhg)

##########

EL_FILES:=$(wildcard dasm*.el dcpu*.el elex*.el)
ELC_FILES=${EL_FILES:%.el=%.elc}

dcpu-autoloads.el: $(filter-out dcpu-autoloads.el,${EL_FILES}) Makefile
	-rm -f ${@}
	emacs --batch -L . -l 'dcpu.el' -f 'dcpu:generate-autoloads'

%.elc: %.el
	emacs --batch  -L . -f 'batch-byte-compile' ${<}

_elc: ${ELC_FILES}

clean:
	-rm -f $(wildcard *.elc *~ dcpu-autoloads.el test-*.dasm.hex)

_cli_1:
	./dcpu-cli ../programs/test-1.bin

_all: dcpu-autoloads.el _elc

##########

# gem install org-ruby
README.html: README.org
	org-ruby README.org > ${@}

_README.html: README.html
	open README.html

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

_line_cnt:
	wc *.el
