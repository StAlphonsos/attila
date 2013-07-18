# -*- makefile -*-

S?=..
include $(S)/settings.mk
include $(S)/commands.mk

TEMPLATES?=template.*
FILES?=*.au *.png Utils.pm
LIBPERL?=perl/*.p[lm]

default: all

all clean distclean:

install: all $(LIBDIR)
	@(for f in $(TEMPLATES) $(FILES) $(LIBPERL); do $(FAKE) $(INSTALL) $$f $(LIBDIR)/$$f; done)

uninstall: $(LIBDIR)
	@echo No uninstall for dotfiles

$(LIBDIR): $(LIBDIR)
	$(FAKE) $(INSTALL_D) $(LIBDIR)
	$(FAKE) $(INSTALL_D) $(LIBDIR)/perl
