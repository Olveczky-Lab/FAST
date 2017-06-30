param($hostname)
if (!$hostname) {
	echo "hostname is necessary"
}
else {
	$inpaths = @(
'/root/data/rpoddar/steffen170/Rat16/635576303263290371',
'/root/data/rpoddar/steffen170/Rat16/635592472184060130',
'/root/data/rpoddar/steffen170/Rat16/635577203829834332',
'/root/data/rpoddar/steffen170/Rat16/635583269298629093',
'/root/data/rpoddar/steffen170/Rat16/635582382239865694',
'/root/data/rpoddar/steffen170/Rat16/635575434654244197',
'/root/data/rpoddar/steffen170/Rat16/635574580021168374',
'/root/data/rpoddar/steffen170/Rat16/635584103832992750',
'/root/data/rpoddar/steffen170/Rat16/635581523887631330',
'/root/data/rpoddar/steffen170/Rat16/635578199676594554',
'/root/data/rpoddar/steffen208/Rat20/635576302725783628',
'/root/data/rpoddar/steffen208/Rat20/635592473569345608',
'/root/data/rpoddar/steffen208/Rat20/635577203443800113',
'/root/data/rpoddar/steffen208/Rat20/635583269216699425',
'/root/data/rpoddar/steffen208/Rat20/635582381977257437',
'/root/data/rpoddar/steffen208/Rat20/635584103739584054',
'/root/data/rpoddar/steffen208/Rat20/635581523849016069',
'/root/data/rpoddar/steffen208/Rat20/635578199456302779',
'/root/data/rpoddar/steffen208_2/Rat20/635575434568831018',
'/root/data/rpoddar/steffen208_2/Rat20/635574580540834935'
)
	$outpaths = @(
'/root/data/steffenwolff/RAT16/635576303263290371',
'/root/data/steffenwolff/RAT16/635592472184060130',
'/root/data/steffenwolff/RAT16/635577203829834332',
'/root/data/steffenwolff/RAT16/635583269298629093',
'/root/data/steffenwolff/RAT16/635582382239865694',
'/root/data/steffenwolff/RAT16/635575434654244197',
'/root/data/steffenwolff/RAT16/635574580021168374',
'/root/data/steffenwolff/RAT16/635584103832992750',
'/root/data/steffenwolff/RAT16/635581523887631330',
'/root/data/steffenwolff/RAT16/635578199676594554',
'/root/data/steffenwolff/RAT20/635576302725783628',
'/root/data/steffenwolff/RAT20/635592473569345608',
'/root/data/steffenwolff/RAT20/635577203443800113',
'/root/data/steffenwolff/RAT20/635583269216699425',
'/root/data/steffenwolff/RAT20/635582381977257437',
'/root/data/steffenwolff/RAT20/635584103739584054',
'/root/data/steffenwolff/RAT20/635581523849016069',
'/root/data/steffenwolff/RAT20/635578199456302779',
'/root/data/steffenwolff/RAT20/635575434568831018',
'/root/data/steffenwolff/RAT20/635574580540834935'
)
For($I=0;$I -lt $inpaths.count;$I++) {
		$inpath = $inpaths[$I]
		$outpath = $outpaths[$I]
		iex "& '\\140.247.178.88\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\StartSnippeting.ps1' $hostname $inpath.rhd $outpath"
	}
}
