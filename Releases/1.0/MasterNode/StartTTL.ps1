param($hostname,$outpath,$numsamples)
if (!$outpath) {
	echo "OutPath is necessary"
}
else {	
	if (!$numsamples) {
		echo "numsamples is necessary"
	}
	else {
		if (!$hostname) {
			echo "hostname is necessary"
		}
		else {
			$datafile="$outpath/TTLIns"
			$serverendpoint="tcp://*:5000"
			$cmd="C:\Titanic\Snippeter\SnippetMaster.exe"
			$prc="C:\Titanic\ProcessRunnerClient\ProcessRunnerClient.exe"
			netsh advfirewall firewall add rule name=ZMQMapReduce dir=in action=allow protocol=TCP localport=5000

			$workerendpoint="tcp://127.0.0.1:5000"
			$workernodes=@("127.0.0.1:8000")
			for ($j=0;$j -lt $workernodes.length;$j++) {
				$workernode = $workernodes[$j]
				for ($i=0;$i -lt 4;$i++) {
					iex "& '$prc' http://$workernode username password $cmd 'TTLWorker $hostname $workerendpoint $datafile $outpath $j-$i' C:\Titanic\Snippeter"
				}
			}

			$sw = [Diagnostics.Stopwatch]::StartNew()
			iex "& '$cmd' TTLServer $hostname $serverendpoint $numsamples"
			$sw.Stop()
			$time = $sw.Elapsed.TotalSeconds
			echo "Took $time  seconds"
			netsh advfirewall firewall delete rule name=ZMQMapReduce
		}
	}
}
