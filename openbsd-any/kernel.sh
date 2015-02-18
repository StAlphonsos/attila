#!/bin/sh
##
say () {
	echo '****************'
	echo '['`date`'] '"$*"
	echo '****************'
}
LOGFILE=${LOGFILE-${HOME}/logs/kernel.log}
KERNEL=${KERNEL-GENERIC.MP}
start=`date`
say building $KERNEL logging to $LOGFILE
exec >$LOGFILE 2>&1
say configuring $KERNEL
cd /usr/src/sys/arch/`arch -s`/conf
config $KERNEL
say building $KERNEL
cd ../compile/$KERNEL
say cleaning
make clean
say compiling and installing
make && make install
say $KERNEL done - started at $start
