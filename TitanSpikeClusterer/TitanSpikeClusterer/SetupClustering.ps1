. \\140.247.178.88\Rajesh\RatControlOp\PowerShell\ImportExport-PSCredential.ps1
$Cred = Import-PSCredential C:\Titanic\PSCred.enc.xml
for ($i=1;$i -le 5;$i++) {
	Invoke-Command -ComputerName "192.168.0.20$i" -Authentication Credssp -Credential $Cred -ScriptBlock {
		net use \\140.247.178.88\Rajesh olabmstrpwdvrysecure /user:rpoddar
		Remove-Item 'C:\Titanic\Clusterer' -Force -Recurse
		Copy-Item '\\140.247.178.88\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeClusterer\TitanSpikeClusterer\bin\Release' 'C:\Titanic\Clusterer' -Force -Recurse -Container
		net use /delete \\140.247.178.88\Rajesh /yes
	}
}
