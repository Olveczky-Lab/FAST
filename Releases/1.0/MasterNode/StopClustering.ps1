$prc="C:\Titanic\ProcessRunnerClient\ProcessRunnerClient.exe"
$workernodes=@("192.168.0.201:8000","192.168.0.202:8000","192.168.0.203:8000","192.168.0.204:8000","192.168.0.205:8000")
for ($j=0;$j -lt $workernodes.length;$j++) {
	$workernode = $workernodes[$j]
	iex "& '$prc' http://$workernode username password 'C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe' 'Stop-Process -ProcessName TitanSpikeClusterer' C:\Titanic\Clusterer"
}
Start-Sleep -Seconds 2
