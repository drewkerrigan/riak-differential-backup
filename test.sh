#!/bin/bash
RIAK_BIN_LOCATION=~/src/riak-1.2.1/rel/riak/bin
RIAK_DATA_LOCATION=~/src/riak-1.2.1/rel/riak/data
KEYLOG_LOCATION=~/src/riak-1.2.1/rel/riak/log
CODE_LOCATION=`pwd`
BEAMS_LOCATION=/tmp/beams
RDM_LOCATION=~/src/riak-data-migrator/target/riak-data-migrator-0.2.4
BB_LOCATION=~/src/basho_bench
RIAK_IP=127.0.0.1
RIAK_HTTP_PORT=8098
RIAK_PB_PORT=8087

function init() {
	echo "Initializing"
	cd $RDM_LOCATION
	rm -rf output_1
	rm -rf output_2
	mkdir output_1
	mkdir output_2

	cp $CODE_LOCATION/basho_bench/diffload1.config $BB_LOCATION/
	cp $CODE_LOCATION/basho_bench/diffload2.config $BB_LOCATION/
	cp $CODE_LOCATION/basho_bench/diffcheck2.config $BB_LOCATION/
}

function clear_riak() {
	echo "Clear Riak data"
	mkdir $BEAMS_LOCATION
	cd $CODE_LOCATION
	erlc log_key.erl ; rm $BEAMS_LOCATION/log_key.beam ; cp log_key.beam $BEAMS_LOCATION/log_key.beam

	cd $RIAK_BIN_LOCATION
	./riak stop
	rm -rf $RIAK_DATA_LOCATION/*
	rm -rf $KEYLOG_LOCATION/*
	cd $RIAK_BIN_LOCATION
	./riak start

	# Add the precommit hook to the desired bucket:
	curl -XPUT -H "Content-Type: application/json" \
	http://$RIAK_IP:$RIAK_HTTP_PORT/buckets/test/props    \
	-d '{"props":{"precommit":[{"mod": "log_key", "fun": "log"}]}}'
}

function run_bb() {
	echo "Running basho bench load 1"
	cd $BB_LOCATION
	./basho_bench $1
}

function rotate_log() {
	echo "Rotate log"
	cd $KEYLOG_LOCATION
	logfile=keyfile.log
	if [ ! -f $logfile ]; then
	  echo "log file not found $logfile"
	  exit 1
	fi
	timestamp=`date +%Y%m%d`
	newlogfile=$logfile.$timestamp
	cp $logfile $newlogfile
	cat /dev/null > $logfile

	echo "Dedup, merge, backup"
	sort $KEYLOG_LOCATION/$newlogfile | uniq | grep -v delete | sed -e s/store,//g >> bucketKeyNameFile.txt
	mv $KEYLOG_LOCATION/bucketKeyNameFile.txt $RDM_LOCATION/
}

function run_backup() {
	echo "Running Backup"
	cd $RDM_LOCATION
	java -jar riak-data-migrator-0.2.4.jar -d -K bucketKeyNameFile.txt -r $1 -h $RIAK_IP -p $RIAK_PB_PORT -H $RIAK_HTTP_PORT
}

function load_from_backup() {
	echo "Reloading from Backup"
	cd $RDM_LOCATION
	java -jar riak-data-migrator-0.2.4.jar -l -r $1 -a -h $RIAK_IP -p $RIAK_PB_PORT -H $RIAK_HTTP_PORT
}

init

clear_riak
run_bb diffload1.config
rotate_log
run_backup output_1

run_bb diffload2.config
rotate_log
run_backup output_2

clear_riak
load_from_backup output_1
load_from_backup output_2
run_bb diffcheck2.config