param($hostname)
if (!$hostname) {
	echo "hostname is necessary"
}
else {
	$inpaths = @(

'/root/data/rpoddar/ashesh60/Data/Hindol/635575742205053556',
'/root/data/rpoddar/ashesh60/Data/Hindol/635577855613334605'
)
	$outpaths = @(
'/root/data/asheshdhawale/Data/Hindol/635575742205053556',
'/root/data/asheshdhawale/Data/Hindol/635577855613334605'
)
For($I=0;$I -lt $inpaths.count;$I++) {
		$inpath = $inpaths[$I]
		$outpath = $outpaths[$I]
		iex "& '\\140.247.178.88\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\StartSnippeting.ps1' $hostname $inpath.rhd $outpath"
	}
}
