. \\ipaddress\Rajesh\RatControlOp\PowerShell\ImportExport-PSCredential.ps1
$Cred = Import-PSCredential C:\Titanic\PSCred.enc.xml
for ($i=1;$i -le 5;$i++) {
	Invoke-Command -ComputerName "192.168.0.20$i" -Authentication Credssp -Credential $Cred -ScriptBlock {
		net use \\ipaddress\Rajesh password /user:rpoddar
		Remove-Item 'C:\Titanic\Clusterer' -Force -Recurse
		Copy-Item '\\ipaddress\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeClusterer\TitanSpikeClusterer\bin\Release' 'C:\Titanic\Clusterer' -Force -Recurse -Container
		net use /delete \\ipaddress\Rajesh /yes
	}
}
