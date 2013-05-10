#!/bin/bash

#Node to query for backups
RIAK_IP=$1
RIAK_PB_PORT=8087
RIAK_HTTP_PORT=8098

#Other Settings
LOCAL_RDM_LOCATION=~/src/riak-data-migrator/target/riak-data-migrator-0.2.4
REMOTE_LOG_LOCATION=/var/log/riak
LOG_FILE_NAME=keyfile.log
DATA_DIRECTORY_PREFIX=output

TIMESTAMP=`date +%Y%m%d`
LOGFILE="$REMOTE_LOG_LOCATION/$LOG_FILE_NAME"
NEWLOGFILE="$LOGFILE.$TIMESTAMP"
FILECOUNTER=1
PROCESSED_KEYFILE=$LOCAL_RDM_LOCATION/bucketKeyNameFile.$TIMESTAMP.txt
BACKUP_DIRECTORY=$DATA_DIRECTORY_PREFIX-$TIMESTAMP

echo "Starting Backup"

# Loop over each of the ip addresses and grab keyfile.log
HOSTCOUNTER=1
for HOST in "$@"
do
	echo "Rotating and copying $LOG_FILE_NAME from $HOST"
    ssh $HOST "cp $LOGFILE $NEWLOGFILE ; cat /dev/null > $LOGFILE"
    scp $HOST:$NEWLOGFILE $LOCAL_RDM_LOCATION/$LOG_FILE_NAME.$TIMESTAMP.$HOSTCOUNTER

    HOSTCOUNTER=$((HOSTCOUNTER + 1))
done


# Process log files
rm $PROCESSED_KEYFILE
HOSTCOUNTER=1
for HOST in "$@"
do
	echo "Processing $LOG_FILE_NAME.$TIMESTAMP.$HOSTCOUNTER"
	ORIG=$LOCAL_RDM_LOCATION/$LOG_FILE_NAME.$TIMESTAMP.$HOSTCOUNTER
	sort $ORIG | uniq | grep -v delete | grep -v '^$' | sed -e s/store,//g >> $PROCESSED_KEYFILE

	HOSTCOUNTER=$((HOSTCOUNTER + 1))
done

# Perform backup
cd $LOCAL_RDM_LOCATION
mkdir $BACKUP_DIRECTORY
echo "Backing up $PROCESSED_KEYFILE to $DATA_DIRECTORY_PREFIX-$TIMESTAMP"
java -jar riak-data-migrator-0.2.4.jar -d -K $PROCESSED_KEYFILE -r $BACKUP_DIRECTORY -h $RIAK_IP -p $RIAK_PB_PORT -H $RIAK_HTTP_PORT