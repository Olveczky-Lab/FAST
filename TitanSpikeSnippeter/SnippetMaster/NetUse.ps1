$prc="\\140.247.178.88\Rajesh\Ephys\ProcessRunnerService\ProcessRunnerClient\ProcessRunnerClient\bin\Release\ProcessRunnerClient.exe"
for ($j=1;$j -le 5;$j++) {
	iex "& '$prc' http://192.168.0.20$j`:8000 Administrator olabmstr!1 net.exe 'use \\192.168.0.10\asheshdhawale elawahdhsehsa! /user:asheshdhawale' C:\Titanic\Snippeter"
}
