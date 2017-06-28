#if COMPILED
module PlotRenTec
#else
#load "SVGWriter.fs"
#load "Subject.fs"
#load "ClusterViewerHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open Helper
open SVGWriter
open MathHelper
open PlotHelper
open RP.Controls
open System.Windows.Media
open ClusterTreeHelper
open ClusterViewerHelper
open ClusterHelper
open RunCluster
open Cluster
open System.IO
open Nessos.FsPickler
open MathNet.Numerics.Statistics
open BehaviorHelper
open TwoTapHelper
let getAlignedTimes (dtbefore,dtafter) taligns spiketimes = 
    taligns
    |> Array.Parallel.map (fun talign ->
        spiketimes |> Array.filter (fun x -> x > talign+(dtbefore*30000.|>int64) && x < talign+(dtafter*30000.|>int64))
        |> Array.map (fun t -> (t-talign|>float)/30000.))
let plotaligned xranges xs =
    let wnd,(xrange,xranges),yrange,sps = 
        plotspikerasters xranges 0.25 0.1 10. xs
    let plots =
        sps |> Array.map (Array.map (getSVGSubPlot 600. (500.*(xs|>Array.length|>float)))) |> Array.concat
    for xrange in xranges do
        addSubPlot2 wnd xrange yrange xrange yrange 
            [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList] |> ignore  
    wnd.Width <- 600.
    wnd.Height <- 600.
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    plots

let plotraw () = 
    let hostname = Remote "140.247.178.94:5900"
    let path = @"/root/data/asheshdhawale/Data/Jaunpuri/635512225348432484"
    let fpath =  sprintf @"%s/%s" path

    let start = 2625480UL
    let rawdata =
        let n = 30000 
        let chnums = [|4;5;6;7|]
        let b = getArray<byte> hostname (sprintf "%s.rhd" path) (176UL*start) (176*n)
        chnums |> Array.map (fun ch ->
            let d_s = scaleData (32768.0) b ch
            d_s |> filtfilt spikes_bandpass
        )

    let plotraw  =
        let wnd = new D3DPlot2DWindow()
        wnd.Show()
        let xrange = Range(0.,1.)
        let yrangepos,yrange = 
            rawdata |> Array.map (fun xs -> xs |> Array.min,xs |> Array.max) |> Array.scan (fun (acc,_) (t0,t1) ->
                (acc+t1-t0,Some ((acc,t1-t0),(t0,t1)))
            ) (0.,None) |> fun xs -> xs|> Array.choose snd, Range(0.,xs.[Array.length xs-1]|>fst)
        let yranges = aggRange wnd yrange yrangepos
        addXAxis wnd xrange Colors.Black "Time(s)" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        Array.zip yranges rawdata |> Array.map (fun (yrange,rawdata) ->
            let l = getLinexy2 Colors.Black (rawdata |> Array.mapi (fun i x -> (i|>float)/30000.,x))
            let lt = [|(0., 250.),(1.,250.),Colors.Red;(0., -250.),(1.,-250.),Colors.Red|] |> getLineList
            addSubPlot2 wnd xrange yrange xrange yrange [l;lt]
            |> getSVGSubPlot 900. 600. 
        )

    let spikes =
        let sfname,stfname = 
            fpath @"ChGroup_1/Spikes",fpath @"ChGroup_1/SpikeTimes"
        let first,last = getSubSetindx hostname stfname (start,start+30000UL)
        (getSpikeSubsets hostname sfname stfname 256 [|[|first..last|]|]).[0]
        |> Array.Parallel.map (fun (st,spk) -> spk |> Array.map (fun x -> (x|>float)*0.195))

    let plotspikes =
        let yrange = Range(-300.,175.)
        let (wnd,(sp,grd)),plotfun = plotshapes 64 4 yrange
        plotfun 
            [spikes,Colors.Gray
             spikes|>Array.filter (fun spk -> spk.[31*4+2] < -200.),Colors.Red;
             spikes|>Array.filter (fun spk -> spk.[31*4+0] > 75.),Colors.Blue;]
        [|sp;grd|] |> Array.map (getSVGSubPlot 900. 300.)
    ()

let plotclusters () =
    let hostname = Remote "140.247.178.16:5900"
    let path = @"/root/data/rpoddar/arches/635267094066850917"
    let fpath =  sprintf @"%s/%s" path
    let chgroup,nchans,spikelen = "0",4,64
    let fname = sprintf "%s/ChGroup_%s/%s" path chgroup
    let numperblock,firstblock,numblocks = 1000,45236,500

    let scaled_spikes,spikesraw = 
        let sfname,stfname,clusfname = 
            fname @"Spikes", fname @"SpikeTimes", fname @"Clusters1.indx"
        let spikestoload = getClustersIndx hostname clusfname numperblock firstblock numblocks
        let spikes = getSpikeSubsets hostname sfname stfname (spikelen*nchans) [|spikestoload|] |> Array.concat
        let scaled_spikes = 
            spikes |> Array.map (fun (st,spk) -> 
                (st|>float)/30000.,spk |> Array.map (fun x -> 0.195*(x|>float)) |> scalespike nchans)
        let spikesraw = spikes |> Array.map snd
        scaled_spikes,spikesraw

    let plotscatter =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..30000-1|] |> Array.map (Array.get scaled_spikes) |> Array.map (fun (st,spk) -> 
                (spk.[31*nchans+0],spk.[31*nchans + 1]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        [|addSubPlot2 wnd xrange yrange xrange yrange 
            [getLinexy2 Colors.Red [-48.75,-48.75;-48.75,48.75;48.75,48.75;48.75,-48.75;-48.75,-48.75] ] 
            |> getSVGSubPlot 800. 800.;
         getSVGPointList 800. 800. xrange yrange 1. Colors.Black toplot|]

    let plotspikeamp =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..10..500000-1|] |> Array.map (Array.get scaled_spikes) |> Array.map (fun (st,spk) -> 
                (st,spk.[31*nchans+1]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        [|addSubPlot2 wnd xrange yrange xrange yrange 
            [getLinexy2 Colors.Red [xrange.Min,-48.75;xrange.Max,-48.75];getLinexy2 Colors.Red [xrange.Min,48.75;xrange.Max,48.75]] 
            |> getSVGSubPlot 800. 800.;
         getSVGPointList 800. 800. xrange yrange 1. Colors.Black toplot|]

    let t0 = (firstblock|>uint64)*(numperblock|>uint64)
    let ns = 
        [|1..5|] |> Array.map (fun i ->
            let first,last = getSubSetindx hostname (fname (sprintf @"Clusters%d.indx" i)) (t0,t0+(numperblock*numblocks|>uint64)-1UL)
            last-first+1UL|>int
        )
    let first,last = getSubSetindx hostname (fname @"Level2/SpikeTimes") (t0,t0+(numperblock*numblocks|>uint64)-1UL)
    let l2scaled_spikes = 
        let sfname,stfname = 
            fname @"Level2/Spikes", fname @"Level2/SpikeTimesRaw"
        let spikes = getSpikeSubsets hostname sfname stfname (spikelen*nchans) [|[|first..last|]|] |> Array.concat
        let scaled_spikes = 
            spikes |> Array.map (fun (st,spk) -> 
                (st|>float)/30000.,spk |> Array.map (fun x -> 0.195*(x|>float)) |> scalespike nchans)
        let x = 
            getSpikeTimes hostname (fname @"Level2/ClusterSpikeNums.off") first (last-first|>int) 
            |> Seq.pairwise |> Seq.map (fun (a,b) -> (b-a)/8UL|>int) |> Seq.toArray 
        scaled_spikes

    let _,mergertree = 
        use ms = new MemoryStream(getArray<byte> hostname (sprintf @"%s/allmergers" path) 0UL -1)
        FsPickler().Deserialize<string * (string * Tree<Set<string> * (int * float) option> []) []>(ms)
    let sorting,unsorted =
        mergertree |> Array.find (fst>>(=)chgroup) |> snd |>Array.map (fun (Node((a,_),_)) -> a |> Set.toArray)
        |> Array.mapi (fun i x -> 
            let st = getSpikeTimes hostname (fname (sprintf @"MergedChains/%d.l2snums" i)) 0UL -1
            if st.[0] <= last && st.[Array.length st-1] >= first then
                Some (st |> Array.choose (fun x -> if x >= first && x <= last then Some (x-first|>int) else None))
            else None
        ) |> Array.choose id |> Array.sortBy Array.length |> Array.rev 
        |> fun x -> x.[0..15]
        |> fun x -> x, Set.difference ([0..(last-first|>int)]|>Set.ofList) (x|>Array.concat|>Set.ofArray)|>Set.toArray
    
    let plotscatterl2 =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..Array.length l2scaled_spikes-1|] |> Array.map (Array.get l2scaled_spikes) |> Array.map (fun (st,spk) -> 
                (spk.[31*nchans+0],spk.[31*nchans + 1]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        [|addSubPlot2 wnd xrange yrange xrange yrange 
            [getLinexy2 Colors.Red [-48.75,-48.75;-48.75,48.75;48.75,48.75;48.75,-48.75;-48.75,-48.75] ] 
            |> getSVGSubPlot 800. 800.;
         getSVGPointList 800. 800. xrange yrange 1. Colors.Black toplot.[200..];
         getSVGPointList 800. 800. xrange yrange 2. Colors.Red toplot.[0..199]|]

    let plotscatterl2sorting =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..Array.length l2scaled_spikes-1|] |> Array.map (Array.get l2scaled_spikes) |> Array.map (fun (st,spk) -> 
                (spk.[31*nchans+0],spk.[31*nchans + 1]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        Array.append
            [|addSubPlot2 wnd xrange yrange xrange yrange 
                [getLinexy2 Colors.Red [-48.75,-48.75;-48.75,48.75;48.75,48.75;48.75,-48.75;-48.75,-48.75] ] 
                |> getSVGSubPlot 800. 800.;
            getSVGPointList 800. 800. xrange yrange 1. Colors.Gray (unsorted|>Array.map (Array.get toplot));|]
            (sorting |> Array.mapi (fun i x -> 
                x|>Array.map (Array.get toplot)|>getSVGPointList 800. 800. xrange yrange 1. brightColors.[i%Array.length brightColors]))

    let plotspikeampl2 =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..Array.length l2scaled_spikes-1|] |> Array.map (Array.get l2scaled_spikes) |> Array.map (fun (st,spk) -> 
                (st,spk.[31*nchans+2]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        [|addSubPlot2 wnd xrange yrange xrange yrange 
            [getLinexy2 Colors.Red [xrange.Min,-48.75;xrange.Max,-48.75];getLinexy2 Colors.Red [xrange.Min,48.75;xrange.Max,48.75]] 
            |> getSVGSubPlot 800. 800.;
         getSVGPointList 800. 800. xrange yrange (1.5) Colors.Black toplot|]

    let plotspikeampl2sorting =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..Array.length l2scaled_spikes-1|] |> Array.map (Array.get l2scaled_spikes) |> Array.map (fun (st,spk) -> 
                (st,spk.[31*nchans+3]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        Array.append
            [|addSubPlot2 wnd xrange yrange xrange yrange 
                [getLinexy2 Colors.Red [xrange.Min,-48.75;xrange.Max,-48.75];getLinexy2 Colors.Red [xrange.Min,48.75;xrange.Max,48.75]] 
                |> getSVGSubPlot 800. 800.;
             getSVGPointList 800. 800. xrange yrange 1.5 Colors.Gray (unsorted|>Array.map (Array.get toplot))|]
            (sorting |> Array.mapi (fun i x -> 
                x|>Array.map (Array.get toplot)|>getSVGPointList 800. 800. xrange yrange 1.5 brightColors.[i%Array.length brightColors]))

    let clusterSpikes spikestocluster =
        let yrange = Range(-950.,350.)
        let mintemp =  0.01
        let tempstep = mintemp
        let maxtemp =  0.05
        let nspikes = Array.length spikestocluster
        let features = 
            spikestocluster |> Array.map (fun i -> spikesraw.[i]|>scalespike nchans)
        let clus = wave_clus mintemp maxtemp tempstep 300 11 0 
                    @"C:\temp\tempclus" features false
        let ntemp = clus.[0]|>Array.length
        let xrange = Range(-0.5,0.5)
        let clusterblocks =
            let stab x = (x.Stability|>float)/(x.Items|>Array.length|>float)
            let clusterblock = 
                let xspikes = Array.zip spikestocluster clus
                let shapes = spikestocluster |> Array.map (fun i -> scaled_spikes.[i]|>snd)
                getStableTree shapes xspikes
                |> mapTree (fun x _ -> Visible,x)
                |> fun x -> 
                    {StableClusters=x;XPos=(-0.25,0.5)}
            [|clusterblock|]
        let clustertrees = clusterblocks|>Array.map (fun x -> x.StableClusters |> mapTree (fun (_,x) _ -> x))
        let plotspikes =
            let yrange = Range(-300.,175.)
            let (wnd,(sp,grd)),plotfun = plotshapes 64 4 yrange
            
            plotfun 
                [clustertrees.[0] |> fun (Node(x,_)) -> x.Items |> Array.map (fun i -> scaled_spikes.[i]|>snd),Colors.Gray]
            [|sp;grd|] |> Array.map (getSVGSubPlot 800. 800.)
        
        let (wnd,sps),xranges,yranges,cluchanged,_ = plotStableClusters (spikelen,nchans,ntemp) xrange yrange clusterblocks    
    //    let spikeamps = (scaled_spikes|>Array.map (fun (st,spk) -> 
    //            st,Array.init nchans (fun ch -> Array.get spk (31*nchans+ch))))
    //    let wndspikes = 
    //        plotClusterSpikeAmps xrange [|scaled_spikes|>Array.minBy fst|>fst,scaled_spikes|>Array.maxBy fst|>fst|] 
    //            (clusterblocks|>Array.map (fun x -> x.StableClusters|> mapTree (fun (_,clu) _ -> clu.Items),x.XPos)) 
    //            yrange spikeamps cluchanged     
    //    plotClusterSpikeShapes spikelen nchans ntemp yrange clustertrees (scaled_spikes|>Array.map snd) cluchanged
        sps |> Array.map (fun (a,b) -> Array.append b a) |> Array.concat

    let plotclusters = clusterSpikes [|0..999|]
    ()

let plotexamplechangingwaveform () = 
    let hostname = Remote "140.247.178.16:5900"
    let path = @"/root/data/rpoddar/badlands/635276396558304920"
    let fpath =  sprintf @"%s/%s" path
    let chgroup,nchans,spikelen = "0",4,64
    let fname = sprintf "%s/ChGroup_%s/%s" path chgroup
    let mergertree =         
        use ms = new MemoryStream(getArray<byte> hostname (fpath @"allmergers" ) 0UL -1)
        FsPickler().Deserialize<string * (string * Tree<Set<string> * (int * float) option> []) []>(ms)
        |> snd
    let units = [|1177,0;1199,-3|]
    let scaled_spikes = 
        let sfname,stfname = 
            fname @"Spikes", fname @"SpikeTimes"
        units |> Array.map (fun (unit,init_t) ->
            let tree = mergertree |> Array.find (fst>>(=)chgroup)|>snd |> fun x -> x.[unit]
            let (Node(chains,_)) = 
                tree |> mapTree (fun (a,b) sts ->
                    match b,sts with
                    | _,[||] -> a |> Set.toArray |> Array.map (fun x -> x,init_t)
                    | Some (dt,_),[|Node(left,_);Node(right,_)|] -> 
                        Array.append left (right |> Array.map (fun (x,t) -> x,t+dt))
                    | _ -> failwith "Not a binary tree"
                )
            chains |> Array.map (fun (chain,t) ->
                let snums = getSpikeTimes hostname (fname (sprintf @"AutoSort/%s.snums" chain)) 0UL -1
                let spikes = getSpikeSubsets hostname sfname stfname (spikelen*nchans) [|snums|] |> Array.concat
                spikes |> Array.map (fun (st,spk) -> 
                    (st|>float)/30000.,
                    (translate nchans 10 (spk |> Array.map (fun x -> 0.195*(x|>float))) t)//.[16*nchans..48*nchans-1]
                ) 
            ) |> Array.concat 
        ) |> Array.concat |> Array.sortBy fst
        |> fun x ->
            let n = Array.length x
            Array.init (n/100) (fun i -> 
                x.[i*100+50]|>fst,
                x.[i*100..(i+1)*100-1]|>Array.map (fun (_,x) -> 1.,x) |> avgShape
            )
    let spikestoplot = 
        [100,124;650,674;1000,1024;1600,1624;2500,2524]
    let plotspikeamp =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..3300-1|] |> Array.map (Array.get scaled_spikes) |> Array.map (fun (st,spk) -> 
                (st,spk.[31*nchans+2]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        [|addSubPlot2 wnd xrange yrange xrange yrange 
            (getLinexy2 Colors.Red [xrange.Min,-48.75;xrange.Max,-48.75]::getLinexy2 Colors.Red [xrange.Min,48.75;xrange.Max,48.75]::
             (spikestoplot |> List.mapi (fun i (t1,t2) -> 
                let t1 = scaled_spikes.[t1]|>fst
                let t2 = scaled_spikes.[t2]|>fst
                [getLinexy2 brightColors.[i] [t1,yrange.Min;t1,yrange.Max];getLinexy2 brightColors.[i] [t2,yrange.Min;t2,yrange.Max]])|>List.concat) |> Seq.cast)
            |> getSVGSubPlot 1600. 800.;
         getSVGPointList 1600. 800. xrange yrange 1.5 Colors.Black toplot|]
    let plotspikes =
        let yrange = Range(-200.,150.)
        let (wnd,(sp,grd)),plotfun = plotshapes 32 4 yrange
        let a = 
            spikestoplot |> List.mapi (fun i (t1,t2) ->
                scaled_spikes.[t1..t2]|>Array.map (snd>>fun x -> x.[16*nchans..48*nchans-1]),
                brightColors.[i]
            ) 
        plotfun a
        [|sp;grd|] |> Array.map (getSVGSubPlot 1600. 600.)
        
    ()
    
let plotexamplebigwaveform () = 
    let hostname = Remote "140.247.178.16:5900"
    let path = @"/root/data/rpoddar/arches/635267094066850917"
    let fpath =  sprintf @"%s/%s" path
    let chgroup,nchans,spikelen = "0",4,64
    let fname = sprintf "%s/ChGroup_%s/%s" path chgroup
    let scaled_spikes = 
        let sfname,stfname = 
            fname @"Level2/Spikes", fname @"Level2/SpikeTimesRaw"
        let snums = getSpikeTimes hostname (fname (sprintf @"MergedChains/%s.l2snums" "8")) 0UL -1
        let spikes = getSpikeSubsets hostname sfname stfname (spikelen*nchans) [|[|0..1..Array.length snums-1|]|>Array.map (Array.get snums)|] |> Array.concat
        spikes |> Array.map (fun (st,spk) -> 
            (st|>float)/30000.,
            (spk |> Array.map (fun x -> 0.195*(x|>float)))//.[16*nchans..48*nchans-1]
        ) 
    
    let spikestoplot = 
        [1000,1024;20000,20024;34000,34024]
    let plotspikeamp =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..50000-1|] |> Array.map (Array.get scaled_spikes) |> Array.map (fun (st,spk) -> 
                (st,spk.[31*nchans+3]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        [|addSubPlot2 wnd xrange yrange xrange yrange 
            (getLinexy2 Colors.Red [xrange.Min,-48.75;xrange.Max,-48.75]::getLinexy2 Colors.Red [xrange.Min,48.75;xrange.Max,48.75]::
             (spikestoplot |> List.mapi (fun i (t1,t2) -> 
                let t1 = scaled_spikes.[t1]|>fst
                let t2 = scaled_spikes.[t2]|>fst
                [getLinexy2 brightColors.[i] [t1,yrange.Min;t1,yrange.Max];getLinexy2 brightColors.[i] [t2,yrange.Min;t2,yrange.Max]])|>List.concat) |> Seq.cast)
            |> getSVGSubPlot 1600. 800.;
         getSVGPointList 1600. 800. xrange yrange 1.5 Colors.Black toplot|]
    let plotspikes =
        let yrange = Range(-900.,150.)
        let (wnd,(sp,grd)),plotfun = plotshapes 32 4 yrange
        let a = 
            spikestoplot |> List.mapi (fun i (t1,t2) ->
                scaled_spikes.[t1..t2]|>Array.map (snd>>fun x -> x.[16*nchans..48*nchans-1]),
                brightColors.[i]
            ) 
        plotfun a
        [|sp;grd|] |> Array.map (getSVGSubPlot 1600. 600.)
        
    ()
    
let plotexamplelongwaveform () = 
    let hostname = Remote "140.247.178.94:5900"
    let ratpath = sprintf @"/root/data/asheshdhawale/Data/Gandhar/%s"
    let chgroup,nchans,spikelen = "1",4,64
    let cells = 
        [|1940;2099;2078;2083;2029;2652;2602;2396;2393;2253;2391;2369;2344;2156;2159;2161;2750;2195;2155;2195;2805;2792|]
    let fnums = 
      [|635315177878133140L; 635321438594938418L; 635322210101867531L; 635323209220610003L;
        635330705373882279L; 635331796778663473L; 635334227062367922L;
        635348390634222721L; 635354189728481093L; 635355048019445858L;
        635356075870355949L; 635356935917584898L; 635358608001473262L;
        635360126629009723L; 635361059805403998L; 635361981590114691L;
        635363745181074152L; 635364676575794173L; 635368345668632516L;
        635381851614475718L; 635385150755642455L; 635386970581102692L;
        635388753469302338L|] |> Array.map (fun x -> sprintf "%d" x)
    let cellnums =
        let map = 
            let mergertree = 
                fnums |> Array.map (fun fnum ->
                    use ms = new MemoryStream(getArray<byte> hostname (ratpath (sprintf @"%s/allmergers" fnum)) 0UL -1)
                    FsPickler().Deserialize<string * (string * Tree<Set<string> * (int * float) option> []) []>(ms)
                )
            let allmergers =
                mergertree |> Array.map (fun (fnum,xss) ->
                    fnum,(xss |> Array.map (fun (chgroup,xs) -> chgroup,xs|>Array.map (fun (Node((a,_),_)) -> a |> Set.toArray)) |> Map.ofArray)
                ) |> Map.ofArray
            mergertree |> Seq.groupBy fst |> Seq.map (fun (fnum,_) ->
                let n = allmergers.[fnum].[chgroup]|>Array.length
                [|0..n-1|] |> Array.map (fun i -> fnum,i)
            ) |> Array.concat |> Array.mapi (fun i x -> i,x) |> Map.ofArray
        cells |> Array.map (fun x -> Map.find x map)
    let sms =
        use ms = new MemoryStream(getArray<byte> hostname (ratpath "OpConHRTwoTapSMTimes") 0UL -1)
        FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms) |> Map.ofArray
    let offsets = 
        let start = fnums.[0] |> uint64
        fnums |> Array.map (fun fnum ->
            ((fnum |> uint64) - start)*3UL/1000UL
        ) |> Array.zip fnums |> Map.ofArray

    let scaled_spikes,spiketimes = 
        cellnums |> Array.map (fun (fnum,cell) ->
            let offset = offsets.[fnum]
            let fname = sprintf "%s/ChGroup_%s/%s" (ratpath fnum) chgroup
            let sfname,stfname = 
                fname @"Level2/Spikes", fname @"Level2/SpikeTimesRaw"
            let snums = getSpikeTimes hostname (fname (sprintf @"MergedChains/%d.l2snums" cell)) 0UL -1
            let stimes = 
                getSpikeTimes hostname (fname (sprintf @"MergedChains/%d.stimes" cell)) 0UL -1
                |> Array.map (fun st -> (st+offset|>float)/30000.)
            let spikes = getSpikeSubsets hostname sfname stfname (spikelen*nchans) [|[|0..1..Array.length snums-1|]|>Array.map (Array.get snums)|] |> Array.concat
            spikes |> Array.map (fun (st,spk) -> 
                (st+offset|>float)/30000.,
                (spk |> Array.map (fun x -> 0.195*(x|>float)))
            ),stimes
        ) |> fun xs -> xs |> Array.map fst |> Array.concat |> Array.sortBy fst, xs |> Array.map snd |> Array.concat |> Array.sort
    
    let frate =
        let t0,t1 = spiketimes.[0],spiketimes.[Array.length spiketimes-1]
        let dt = 60.*10.
        let nb = 1+((t1-t0)/dt|>int)
        let h = Histogram(spiketimes.[1..],nb,t0,t0+dt*(nb|>float))
        let frates = Array.init (h.BucketCount) (fun i -> (h.[i].LowerBound+h.[i].UpperBound)/2.,h.[i].Count/dt)
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let xrange = Range(t0,t1)
        let yrange = Range(0.0,frates |> Array.maxBy snd |> snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        addSubPlot2 wnd xrange yrange xrange yrange [getLinexy2 Colors.Black frates]
        |> getSVGSubPlot 1600. 500.
    
    showSortedSpikes spikelen nchans [|scaled_spikes|>Array.mapi (fun i (_,x) -> (i|>float),x),(Colors.Black,[||])|]
    |> ignore

    let spikestoplot = 
        [3.8902;6.6422;9.0991;15.6997] |> List.map (fun x -> x*3600.|>int|>fun y -> y,y+24)
    let plotspikeamp =
        let wnd = new D3DPlot2DWindow();
        wnd.Show()
        let toplot = 
            [|0..2..Array.length scaled_spikes-1|] |> Array.map (Array.get scaled_spikes) |> Array.map (fun (st,spk) -> 
                (st,spk.[31*nchans+3]))
        let xrange = Range(toplot|>Array.minBy fst|>fst,toplot|>Array.maxBy fst|>fst)
        let yrange = Range(toplot|>Array.minBy snd|>snd,toplot|>Array.maxBy snd|>snd)
        addXAxis wnd xrange Colors.Black "" |> ignore
        addYAxis wnd yrange Colors.Black "" |> ignore
        let l = 
            toplot |> Array.map (fun x ->
                x,(xrange.Interval()/400.,yrange.Interval()/400.),Colors.Black
            ) |> getMarkerPoints
        addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
        [|addSubPlot2 wnd xrange yrange xrange yrange 
            (getLinexy2 Colors.Red [xrange.Min,-48.75;xrange.Max,-48.75]::getLinexy2 Colors.Red [xrange.Min,48.75;xrange.Max,48.75]::
             (spikestoplot |> List.mapi (fun i (t1,t2) -> 
                let t1 = scaled_spikes.[t1]|>fst
                let t2 = scaled_spikes.[t2]|>fst
                [getLinexy2 brightColors.[i] [t1,yrange.Min;t1,yrange.Max];getLinexy2 brightColors.[i] [t2,yrange.Min;t2,yrange.Max]])|>List.concat) |> Seq.cast)
            |> getSVGSubPlot 1600. 800.;
         getSVGPointList 1600. 800. xrange yrange 1.5 Colors.Black toplot|]
    let plotspikes =
        let yrange = Range(-900.,150.)
        let (wnd,(sp,grd)),plotfun = plotshapes 32 4 yrange
        let a = 
            spikestoplot |> List.mapi (fun i (t1,t2) ->
                scaled_spikes.[t1..t2]|>Array.map (snd>>fun x -> x.[16*nchans..48*nchans-1]),
                brightColors.[i]
            ) 
        plotfun a
        [|sp;grd|] |> Array.map (getSVGSubPlot 1600. 600.)
        
    ()

let plotrasters () =
    let hostname = Remote "140.247.178.94:5900"
    let ratpath = sprintf @"/root/data/asheshdhawale/Data/Gandhar/%s"
    let chgroup,nchans,spikelen = "1",4,64
    let cells = 
        [|1940;2099;2078;2083;2029;2652;2602;2396;2393;2253;2391;2369;2344;2156;2159;2161;2750;2195;2155;2195;2805;2792|]
    let fnums = 
      [|635315177878133140L; 635321438594938418L; 635322210101867531L; 635323209220610003L;
        635330705373882279L; 635331796778663473L; 635334227062367922L;
        635348390634222721L; 635354189728481093L; 635355048019445858L;
        635356075870355949L; 635356935917584898L; 635358608001473262L;
        635360126629009723L; 635361059805403998L; 635361981590114691L;
        635363745181074152L; 635364676575794173L; 635368345668632516L;
        635381851614475718L; 635385150755642455L; 635386970581102692L;
        635388753469302338L|] |> Array.map (fun x -> sprintf "%d" x)
    let cellnums =
        let map = 
            let mergertree = 
                fnums |> Array.map (fun fnum ->
                    use ms = new MemoryStream(getArray<byte> hostname (ratpath (sprintf @"%s/allmergers" fnum)) 0UL -1)
                    FsPickler().Deserialize<string * (string * Tree<Set<string> * (int * float) option> []) []>(ms)
                )
            let allmergers =
                mergertree |> Array.map (fun (fnum,xss) ->
                    fnum,(xss |> Array.map (fun (chgroup,xs) -> chgroup,xs|>Array.map (fun (Node((a,_),_)) -> a |> Set.toArray)) |> Map.ofArray)
                ) |> Map.ofArray
            mergertree |> Seq.groupBy fst |> Seq.map (fun (fnum,_) ->
                let n = allmergers.[fnum].[chgroup]|>Array.length
                [|0..n-1|] |> Array.map (fun i -> fnum,i)
            ) |> Array.concat |> Array.mapi (fun i x -> i,x) |> Map.ofArray
        cells |> Array.map (fun x -> Map.find x map)
    let sms =
        use ms = new MemoryStream(getArray<byte> hostname (ratpath "OpConHRTwoTapSMTimes") 0UL -1)
        FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms) |> Map.ofArray
    let (exptid,sm) as expt = (79,40)
    let xrange = (-3.,5.)
    let alignedtimes =
        let fnum = 
            let fnum,_ = sms.[expt]
            fnum |> sprintf "%d"
        let spiketimes = 
            cellnums |> Array.filter (fun (f,_) -> f = fnum) |> Array.map (fun (_,cell) -> 
                let fname = sprintf "%s/ChGroup_%s/%s" (ratpath fnum) chgroup
                getSpikeTimes hostname (fname (sprintf @"MergedChains/%d.stimes" cell)) 0UL -1
            )|> Array.concat |> Array.sort |> Array.map int64
        let hrtimes = 
            use ms = new MemoryStream(getArray<byte> hostname (ratpath (sprintf @"%s/Behavior/79LeverPressOpCon.tmap" fnum)) 0UL -1)
            FsPickler().Deserialize<Map<int,(int64*int64)>>(ms)
        let trials =
            use ms = new MemoryStream(getArray<byte> hostname (ratpath "OpConTwoTapTrials") 0UL -1)
            FsPickler().Deserialize<((int * int) * (Trial [] * Map<int,int64>) option) []>(ms)
            |> Array.choose (fun (a,b) -> b |> Option.map (fun x -> a,x)) |> Map.ofArray
        let taligns = 
            trials.[expt] |> fst |> Seq.pairwise |> Seq.choose (fun (t1,t2) -> 
                match (t1.Reward,t2.Reward) with
                | Some _, Some _ -> Some (hrtimes.[t2.Taps.[0]|>fst]|>fst)
                |_ -> None
            ) |> Seq.toArray
        getAlignedTimes xrange taligns spiketimes
    let rasters =
        plotaligned [|xrange|] [|[|alignedtimes|],Colors.Blue|]
    (getSVG 600. 500. rasters).Save(@"D:\Rajesh\Lab Meetings\RenTec Interview\rasters.svg")
    ()


let hostname = Remote "140.247.178.16:5900"
let path = @"/root/data/rpoddar/arches/635267094066850917"
let fpath =  sprintf @"%s/%s" path
let chgroup,nchans,spikelen = "0",4,64
let fname = sprintf "%s/ChGroup_%s/%s" path chgroup
