$prc="\\ipaddress\Rajesh\Ephys\ProcessRunnerService\ProcessRunnerClient\ProcessRunnerClient\bin\Release\ProcessRunnerClient.exe"
for ($j=1;$j -le 5;$j++) {
	iex "& '$prc' http://192.168.0.20$j`:8000 Administrator olabmstr!1 net.exe 'use \\ipaddress\asheshdhawale password /user:asheshdhawale' C:\Titanic\Snippeter"
}
