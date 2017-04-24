#if COMPILED
module ComputePSTHs
#else
#load "SnippetHelper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open SegmentationFusion
open Helper
open RP.HDF5.H5TypeProviderRuntime
open ClusterHelper
open PlotHelper
open RP.Controls
open System.Windows
open System.Windows.Media
open System.IO

let filterclusters (start,dur) clusters =
    clusters |> List.map (List.filter (fun (_,t,_) -> start <= t && t <= (start+dur)))
    |> List.filter (fun xs -> List.length xs > 0)

let getClusterDuration cluster =
    let f c = c |> List.head |> fun (_,t:float,_) -> t
    (cluster |> List.rev |> f) - (cluster |> f)

let numperclustering = 1000

let clusters chgroup = 
    let times = 
        use fs = new FileStream(sprintf @"Z:\badlands\BlockTimes\635276396558304920_ChGroup_%s.bt" chgroup,FileMode.Open)
        use br = new BinaryReader(fs)
        Array.init (br.ReadInt32()) (fun i -> (br.ReadInt64()|>float)/30000.0)
    deserialize (sprintf @"Z:\badlands\SegFused75L2\635276396558304920_ChGroup_%s.sf" chgroup)
    |> List.map (List.map (fun (i,clu) -> (i,times.[i],clu)))

let all = 
    Array.init 16 (fun chnum -> (clusters (sprintf "%d" chnum)))

open Helper

let getSpikeTimes chgroup clu =
    let numspikes =     
        let fnames = 
            let fname = sprintf @"Z:\badlands\635276396558304920_%d%s.h5"
            let spikesfname i = fname i ""
            let clusfname i = fname i "_Clusters"
            [|0..9|] |> Array.map (fun i -> (spikesfname i,clusfname i))
        loadnumspikes fnames chgroup numperclustering
    let first = clu|>List.head fst
    let last = clu|>List.rev|>List.head|>fst
    let clus,spiketimes = getSpikeTimesAndClusters numspikes first (last-first+1) chgroup 1000 10



let sessionClusters =
    let smtimes =
        let lines = File.ReadAllLines(@"Z:\badlands\TrainingSMs.txt")
        [|for i in 1..2..lines.Length-1 -> 
            lines.[i].Split('\t') |> Array.map (fun x -> (x|>float)/30000.0) |> fun x -> (x.[0],x.[1]),lines.[i-1]|]
    let all = 
        Array.init 16 (fun chnum -> 
        let c = (clusters (sprintf "%d" chnum))         
        smtimes |> Array.map (fun (t,_) -> filterclusters t c |> List.toArray |> Array.map (fun x -> chnum,x)))
    smtimes |> Array.mapi (fun i t -> t, Array.init 16 (fun chnum -> all.[chnum].[i]) |> Array.concat)

sessionClusters |> Seq.iter (fun (((_,dur),smdetails),xs) -> 
    printfn "%s" smdetails
    xs |> Array.map (fun (chnum,clu) -> (chnum,clu),(getClusterDuration clu))
    |> Array.sortBy snd |> Array.rev
    |> Array.iter (fun ((chnum,clu),d) ->
        if (d/dur > 0.25) then
            printfn "%2d\t%4.0f\t%+5.1f" chnum d (clu|>List.averageBy (fun (_,_,c) -> c.Amplitude))
            //dispchain (clu|>List.map (fun (i,_,c) ->(i,c)))
    )
)

let r = 
    let smtimes =
        let lines = File.ReadAllLines(@"Z:\badlands\TrainingSMs.txt")
        [|for i in 1..2..lines.Length-1 -> lines.[i].Split('\t') |> Array.map (fun x -> (x|>float)/30000.0) |> fun x -> x.[0],x.[1]|]
    //Plotting
    let xrange = Range(0.,3600.*24.*15.)

    let wnd = new D3DPlot2DWindow()
    wnd.Plot.SelectionBoxEnabled <- true
    wnd.Show()
    addTimeXAxis wnd xrange |> ignore

    // Plot Session Times
    smtimes |> Array.map (fun (t,dt) ->
        [|getLinexy (Colors.Gray) [|t,-1.;t,1.|];getLinexy (Colors.Gray) [|t+dt,-1.;t+dt,1.|]|]
    ) |> Array.concat |> Seq.cast |> addSubPlot wnd xrange (Range(-1.,1.)) |> ignore

    let yrange = Range(0.0,15.0)
    addYAxis wnd yrange (Colors.Black) ("Number of units") |> ignore

    let f c = c |> List.head |> fun (_,t:float,xs) -> t
    let a = all |> Array.toList |> List.concat |> List.map (fun c -> f c, f (c|>List.rev))
    Array.init (60*24*14) (fun i -> 
        let t = (i|>float)*60. + 1800.
        t,a |> List.filter (fun (t1,t2) -> t >= t1 && t <= t2) |> List.length |> float
    )
    |> getLinexy Colors.Black
    |> fun l -> addSubPlot wnd xrange yrange [l]


