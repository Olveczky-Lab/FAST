#Copy the latest release from the github server to a local directory. Change $ReleasePath to that
$ReleasePath = ".\WorkerNodes"
#Change ProcessRunnerService.exe.config to use an unused port in the machine like 8000 and the Username/password to a valid one
#Change $port below to the same value

$Cred = Get-Credential

$computerNames=@("192.168.0.201","192.168.0.202","192.168.0.203","192.168.0.204","192.168.0.205")

for ($index = 0; $index -lt $computerNames.length; $index++) {
	$computerName = $computerNames[$index];    
    net use \\$computerName /USER:Administrator password
    Copy-Item $ReleasePath \\$computerName\C$\Titanic -Recurse
    net use \\$computerName /delete
    icm $computerName -Credential $Cred -ScriptBlock {
        $port = 8000
        C:\Titanic\InstallUtil.exe C:\Titanic\ProcessRunnerService\ProcessRunnerService.exe
        netsh advfirewall firewall add rule name=ProcessRunnerService dir=in action=allow protocol=TCP localport=$port
        Start-Service ProcessRunnerService
    }
}
