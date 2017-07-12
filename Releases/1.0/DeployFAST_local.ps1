# Copy the contents of the WorkerNodes folder in the latest FAST GitHub Release to a 'C:\Titanic\' directory on the worker node.
# Change ProcessRunnerService.exe.config to use an unused port in the machine like 8000 (line 16) and also update the username of the PC (line 33).
# Change $port below to the same value.
# Run this script or execute the following commands one by one in a powershell window on the worker node (as Administrator)

$port = 8000
C:\Titanic\InstallUtil.exe C:\Titanic\ProcessRunnerService\ProcessRunnerService.exe
netsh advfirewall firewall add rule name=ProcessRunnerService dir=in action=allow protocol=TCP localport=$port
Start-Service ProcessRunnerService