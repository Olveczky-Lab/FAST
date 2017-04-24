param($hostname,$sfname,$l2fname,$firstspike,$nspikes,$nlevels,$nchans)
if (!$hostname) {
	echo "hostname is necessary"
}
else {
	netsh advfirewall firewall add rule name=ZMQMapReduce dir=in action=allow protocol=TCP localport=5000
	$cmd="C:\Titanic\Clusterer\TitanSpikeClusterer.exe"
	$prc="C:\Titanic\ProcessRunnerClient\ProcessRunnerClient.exe"
	$serverendpoint="tcp://*:5000"

	$workerendpoint="tcp://192.168.0.201:5000"
	$workernodes=@("192.168.0.201:8000","192.168.0.202:8000")
	for ($j=0;$j -lt $workernodes.length;$j++) {
		$workernode = $workernodes[$j]
		for ($i=0;$i -lt 1;$i++) {
			iex "& '$prc' http://$workernode Administrator olabmstr!1 $cmd 'Level2Worker $hostname $sfname $l2fname $nchans $nlevels $workerendpoint $j-$i' C:\Titanic\Clusterer"
		}
	}
	$sw = [Diagnostics.Stopwatch]::StartNew()
	iex "& '$cmd' Level2Server $firstspike $nspikes $nlevels $serverendpoint"
	$sw.Stop()
	$time = $sw.Elapsed.TotalSeconds
	echo "Took $time  seconds"
	netsh advfirewall firewall delete rule name=ZMQMapReduce
}