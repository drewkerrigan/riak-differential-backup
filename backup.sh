#!/bin/bash

#Rotate log file, TODO:convert to function.
logfile=$1
if [ ! -f $logfile ]; then
  echo "log file not found $logfile"
  exit 1
fi
timestamp=`date +%Y%m%d`
newlogfile=$logfile.$timestamp
cp $logfile $newlogfile
cat /dev/null > $logfile
gzip -f -9 $newlogfile