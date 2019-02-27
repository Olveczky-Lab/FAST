# Specify inpaths and outpaths for the data files to be snippeted
# Comment/uncomment lines 20/21 depending on whether you want to snippet AMP or RHD files

param($hostname)
if (!$hostname) {
	echo "hostname is necessary"
}
else {
	$inpaths = @(
'X:\Data\Rat1\data_file_1',
'X:\Data\Rat1\data_file_2'
)
	$outpaths = @(
'X:\Data\Rat1\data_file_1',
'X:\Data\Rat1\data_file_2'
)
For($I=0;$I -lt $inpaths.count;$I++) {
		$inpath = $inpaths[$I]
		$outpath = $outpaths[$I]
		iex "& 'C:\Titanic\Snippeter\StartSnippeting.ps1' $hostname $inpath.rhd $outpath" # RHD
#		iex "& 'C:\Titanic\Snippeter\StartSnippetingAMP.ps1' $hostname $inpath.amp $outpath" # AMP
	}
}
