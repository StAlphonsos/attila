# -*- makefile -*-
#

include settings.mk

SUBDIRS?=bin lib dotfiles emacs

default: all

all install uninstall clean distclean:
	@(for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) $(MFLAGS) S=$(S) $@); done)

xinstall:
	@(cd dotfiles; $(MAKE) $(MFLAGS) S=$(S) $@)
