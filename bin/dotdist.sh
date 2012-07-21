#!/bin/sh
##
# Distribute dot.files to various oft-used machines, e.g.
#
#     $ dotdist.sh zshrc
#
# will send my ~/.zshrc to all the machines I care about.
##
if [ x"${DOTDIST}" != x ]; then
  hosts="${DOTDIST}"
else
  if [ -f $HOME/.dotdist ]; then
    hosts="`cat $HOME/.dotdist`"
  else
    hosts="mu intelligence woodshed itzamna wmd"
  fi
fi
for f in $* /dev/null; do
  if [ x"$f" != x/dev/null ]; then
    if [ -f "$HOME/.$f" -a "$f" != "emacs" ]; then
      f=".$f"
    fi
    if [ ! -f "$HOME/$f" -a ! -d "$HOME/$f" ]; then
      echo $0: $f is invalid relative to $HOME ...
    else
      echo Copying $f to: $hosts
      for h in $hosts; do
# This way deals properly with e.g. emacs/lisp/hacking.el (or just emacs)
        (cd $HOME; tar cf - $f) | ssh $h tar xvf - | sed -e "s/^/$h: /"
# This way does not:
#       scp $HOME/$f $h:
      done
    fi
  fi
done
