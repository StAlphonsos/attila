# -*- makefile -*-

S?=..
include $(S)/settings.mk
include $(S)/commands.mk

default: all

all uninstall clean distclean:

install: $(EMACSDIR)
	@($(FAKE) $(TAR_CF) - lisp) | (cd $(EMACSDIR); $(FAKE) $(TAR_XF) -)

$(EMACSDIR):
	$(FAKE) $(INSTALL_D) $(EMACSDIR)
