#!/bin/bash

#ssh to each of the clients and rotate then grab the keyfile.log

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

# Merge and dedup the files

sort keyfile.log | uniq | grep -v delete | sed -e s/store,//g >> bucketKeyNameFile.txt

# Run the data migrator

