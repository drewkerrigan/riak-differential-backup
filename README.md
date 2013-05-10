Riak Differential Backup
========================

This is a guide on how to perform differential backups using the [riak-data-migrator tool](https://github.com/basho/riak-data-migrator).

The goal is to back up only the keys that were modified during a particular time period to avoid backing up the entire Riak cluster.

#Workflow

1. Add a precommit hook to one or more buckets which logs the bucket / key combination on store and delete operations on each node of the cluster.
2. Collect and merge the logs at regular intervals using a cron job from a coordinating machine.
3. Use the [riak-data-migrator tool](https://github.com/basho/riak-data-migrator) to backup the resulting bucket / key list.

#Configuration

####Complete the following steps on each of the nodes in the cluster:

1. Modify [log_key.erl](https://github.com/drewkerrigan/riak-differential-backup/blob/master/log_key.erl) as necessary to change the log location.

	```
	%% ...
	file:write_file("log/keyfile.log", 
	%% ...
	```
2. Compile [log_key.erl](https://github.com/drewkerrigan/riak-differential-backup/blob/master/log_key.erl)

	```
	erlc log_key.erl
	```
3. Move the resulting `log_key.beam` to `/tmp/beams` or another code directory.

	```
	mv log_key.beam /tmp/beams/log_key.beam
	```
4. Create an add_paths in the riak_kv section of app.config (replace `/tmp/beams/` with another code directory if desired).

	```
	{riak_kv, [
	  %% ...
	  {add_paths, ["/tmp/beams/"]},
	  %% ...
	```
5. Restart Riak.

	```
	riak restart
	```

####Complete the following steps on a single node in the cluster:
1. Add the precommit hook to the desired bucket (replace `test` with the bucket(s) to be backed up).

	```
	curl -XPUT -H "Content-Type: application/json" \
	http://127.0.0.1:8098/buckets/test/props    \
	-d '{"props":{"precommit":[{"mod": "log_key", "fun": "log"}]}}'
	```
2. Verify the precommit setting is present.

	```
	curl http://127.0.0.1:8098/buckets/test/props
	```

#Backup

####Complete the following from a backup / coordinating machine

Note: The [backup.sh](https://github.com/drewkerrigan/riak-differential-backup/blob/master/backup.sh) included in this guide is meant to be an example. Operational procedures for backup may differ from the methods used in this file.

Gather, process, and merge the log files from each node in the cluster from the backup machine using [backup.sh](https://github.com/drewkerrigan/riak-differential-backup/blob/master/backup.sh) (automate with cron)

```
./backup.sh <riak_node_1> [<riak_node_2> …]
```

####Alternatively, complete the steps manually as follows

1. Rotate and copy keyfile.log from each of the nodes in the cluster
2. De-duplicate keynames and remove deletes from each of the `keyfile.log` files.

	```
	sort keyfile.log | uniq | grep -v delete | sed -e s/store,//g >> bucketKeyNameFile.txt
	```
3. Run the [riak-data-migrator tool](https://github.com/basho/riak-data-migrator) with the --loadkeys option to backup the keys in `bucketKeyNameFile.txt` (replace `output-20130510` with the backup data directory)

	```
	mkdir output-20130510
	java -jar riak-data-migrator-0.2.4.jar -d --loadkeys bucketKeyNameFile.txt -r output-20130510 -h 127.0.0.1 -p 8087 -H 8098
	```

#Restore

Iterate through each of the generated backup directories and run the [riak-data-migrator tool](https://github.com/basho/riak-data-migrator) load functionality

```
java -jar riak-data-migrator-0.2.4.jar -l -r output-20130510 -a -h 127.0.0.1 -p 8087 -H 8098
```

#Testing

Modify the following lines in [test.sh](https://github.com/drewkerrigan/riak-differential-backup/blob/master/test.sh) to complete a single node test

```
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
```

Run the following from the directory that you've downloaded this repository to:

```
./test.sh
```