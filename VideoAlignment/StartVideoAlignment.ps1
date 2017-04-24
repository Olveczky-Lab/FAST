$datapath="\\192.168.0.10\asheshdhawale\Data\Desh"
$ephysfnum="635315292020667843"
$videopath="$datapath\PGCamL"
$videofnum="635315976150105852"
$syncch="5"
$tapch="1"
$prc="\\140.247.178.88\Rajesh\Ephys\ProcessRunnerService\ProcessRunnerClient\ProcessRunnerClient\bin\Release\ProcessRunnerClient.exe"
$cmd="C:\Titanic\VideoAlignment\VideoAlignment.exe"
$n=0
for ($j=1;$j -le 5;$j++) {
	for ($i=0;$i -lt 3;$i++) {
		$n++
		iex "& '$prc' http:\\192.168.0.20$j`:8000 Administrator olabmstr!1 '$cmd' '$datapath $ephysfnum $videopath $videofnum $syncch $tapch $n' C:\Titanic\Snippeter"
	}
}
