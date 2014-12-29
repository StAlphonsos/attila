#!/bin/sh
##
say () {
	echo '****************'
	echo '['`date`'] '"$*"
	echo '****************'
}
LOGFILE=${LOGFILE-/home/attila/logs/userland.log}
start=`date`
say logging to $LOGFILE
exec >$LOGFILE 2>&1
say cleaning obj
rm -rf /usr/obj/*
cd /usr/src
say making obj
make obj
say making distrib-dirs
cd /usr/src/etc && env DESTDIR=/ make distrib-dirs
say building userland
cd /usr/src
make build
say done - started at $start
