# -*- makefile -*-

S?=..
include $(S)/settings.mk
include $(S)/commands.mk

DOTFILES?=bashrc emacs profile rc screenrc zshaliases zshenv zshrc
XFILES?=Xdefaults xsessiorc

default: all

all clean distclean:

install: all
	@(for f in $(DOTFILES); do $(FAKE) $(INSTALL) $$f $(DOTDIR)/.$$f; done)

xinstall:
	@(for f in $(XFILES); do $(FAKE) $(INSTALL) $$f $(DOTDIR)/.$$f; done)

uninstall:
	@echo No uninstall for dotfiles

