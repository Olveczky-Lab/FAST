param($cmdname,$hostname,$fpath,$nchans,$l2clustemp,$maxtempnum,$numblocks)
	if (!$hostname) {
		echo "hostname is necessary"
	}
	else {
	netsh advfirewall firewall add rule name=ZMQMapReduce dir=in action=allow protocol=TCP localport=5000
	$cmd="C:\Titanic\Clusterer\TitanSpikeClusterer.exe"
	$prc="C:\Titanic\ProcessRunnerClient\ProcessRunnerClient.exe"
	$serverendpoint="tcp://*:5000"

	$workerendpoint="tcp://192.168.0.201:5000"
	$workernodes=@("192.168.0.201:8000","192.168.0.202:8000","192.168.0.203:8000","192.168.0.204:8000","192.168.0.205:8000")
	for ($j=0;$j -lt $workernodes.length;$j++) {
		$workernode = $workernodes[$j]
		for ($i=0;$i -lt 8;$i++) {
			iex "& '$prc' http://$workernode username password $cmd '$($cmdname)Worker $hostname $fpath $nchans $l2clustemp $maxtempnum $workerendpoint $j-$i' C:\Titanic\Clusterer"
		}
	}
	$sw = [Diagnostics.Stopwatch]::StartNew()
	iex "& '$cmd' $($cmdname)Server $hostname $fpath $nchans $l2clustemp $maxtempnum $numblocks $serverendpoint"
	$sw.Stop()
	$time = $sw.Elapsed.TotalSeconds
	echo "Took $time  seconds"
	netsh advfirewall firewall delete rule name=ZMQMapReduce
}
