# -*- makefile -*-
#

.include "bsdmake/settings.mk"

SUBDIRS?=bin lib dotfiles emacs

default: all

all install uninstall clean distclean:
	@(for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) $(MFLAGS) S=$(S) MM=$(MM) $@); done)

xinstall:
	@(cd dotfiles; $(MAKE) $(MFLAGS) S=$(S) MM=$(MM) $@)
