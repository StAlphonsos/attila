#!/bin/sh
##
say () {
	echo '****************'
	echo '['`date`'] '"$*"
	echo '****************'
}
LOGFILE=${LOGFILE-/home/attila/logs/kernel.log}
start=`date`
say logging to $LOGFILE
exec >$LOGFILE 2>&1
say configuring
cd /usr/src/sys/arch/`arch -s`/conf
config GENERIC
cd ../compile/GENERIC
say cleaning
make clean
say compiling and installing
make && make install
say done - started at $start
