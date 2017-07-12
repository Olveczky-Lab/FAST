param($hostname,$datafile,$outpath)
if (!$outpath) {
	echo "OutPath is necessary"
}
else {	
	if (!$datafile) {
		echo "datafile is necessary"
	}
	else {
		if (!$hostname) {
			echo "hostname is necessary"
		}
		else {
			$settingsfile="$outpath/SnippeterSettings.xml"
			$serverendpoint="tcp://*:5000"
			$cmd="C:\Titanic\Snippeter\SnippetMaster.exe"
			$prc="C:\Titanic\ProcessRunnerClient\ProcessRunnerClient.exe"
			netsh advfirewall firewall add rule name=ZMQMapReduce dir=in action=allow protocol=TCP localport=5000

			$workerendpoint="tcp://192.168.0.201:5000"
			$workernodes=@("192.168.0.201:8000","192.168.0.202:8000","192.168.0.203:8000","192.168.0.204:8000","192.168.0.205:8000")
			for ($j=0;$j -lt $workernodes.length;$j++) {
				$workernode = $workernodes[$j]
				for ($i=0;$i -lt 8;$i++) {
					iex "& '$prc' http://$workernode username password $cmd 'SnippetWorkerOld $hostname $settingsfile $workerendpoint $datafile $outpath $j-$i' C:\Titanic\Snippeter"
				}
			}

			$sw = [Diagnostics.Stopwatch]::StartNew()
			iex "& '$cmd' SnippetServer $hostname $settingsfile $serverendpoint"
			$sw.Stop()
			$time = $sw.Elapsed.TotalSeconds
			echo "Took $time  seconds"
			netsh advfirewall firewall delete rule name=ZMQMapReduce
		}
	}
}
