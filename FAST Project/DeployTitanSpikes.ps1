#Copy the latest release from the svn server to a local directory. Change $ReleasePath to that
$ReleasePath = "D:\Rajesh\Ephys\Data Analysis\FSharp\Releases\1.0\WorkerNodes"
#Change ProcessRunnerService.exe.config to use an unused port in the machine like 8001 and the Username to a valid one (RatControl for the RatControl-PCs)
#Change $port below to the same value

$Cred = Get-Credential

for ($index = 9; $index -le 9; $index++) {
	$ComputerName = "RatControl-PC$index";    
    net use \\$ComputerName /USER:RatControl lortnoCtaR!
    Copy-Item $ReleasePath \\$ComputerName\C$\Titanic -Recurse
    net use \\$ComputerName /delete
    icm $ComputerName -Credential $Cred -ScriptBlock {
        $port = 8001
        C:\Titanic\InstallUtil.exe C:\Titanic\ProcessRunnerService\ProcessRunnerService.exe
        netsh advfirewall firewall add rule name=ProcessRunnerService dir=in action=allow protocol=TCP localport=$port
        Start-Service ProcessRunnerService
    }
}