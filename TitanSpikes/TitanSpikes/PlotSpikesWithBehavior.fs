#if COMPILED
module PlotSpikesWithBehavior
#else
#load "SVGWriter.fs"
#time
fsi.ShowDeclarationValues <- false
#endif

open Helper
open BehaviorHelper
open TwoTapHelper
open PlotHelper
open RP.Controls
open System.Windows.Media
open System.Windows.Input
open Nessos.FsPickler
open System.IO
open MathNet.Numerics.Statistics
open SVGWriter
open System.Xml.Linq
open ClusterHelper
open System
open AutoSortHelper
open System.Windows
open Cluster
open SegmentationFusion

let inline diff xs = xs |> Seq.pairwise |> Seq.map (fun (a,b) -> b - a) |> Seq.toArray
let inline cumsum xs = Seq.scan (+) (LanguagePrimitives.GenericZero) xs

let hostname = Remote "140.247.178.94:5900"

let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
let db2tap = db2tap(cstr,"TwoTapTrialAndErrorV2")
let exptid = 79
let ratpath = @"/root/data/asheshdhawale/Data/Gandhar"

let allsms = 
    use ms = new MemoryStream(getBytes hostname (sprintf @"%s/%dOpConHRTwoTapSMTimes" ratpath exptid) 0UL -1 |> Async.RunSynchronously)
    FsPickler().Deserialize<Map<int,int64*(int64*int64)>>(ms)
    |> Map.toArray
    |> Seq.groupBy (fun (_,(fnum,_)) -> fnum)
    |> Seq.toArray
    |> Array.map (fun (fnum,xs) -> fnum, xs |> Seq.map (fun (smid,(_,(t0,t1))) -> smid,(t0|>uint64,t1|>uint64)) |> Map.ofSeq)
    |> Map.ofArray

let xrange = Range(-3.,5.)
let alignedtimes =
    allsms |> Map.map (fun fnum sms ->
        let path =  sprintf "%s/%d" ratpath fnum
        let fpath =  sprintf "%s/%s" path

        let taptimes = 
            use ms = new MemoryStream(getBytes hostname (fpath (sprintf @"Behavior/%dLeverPressOpCon.tmap" exptid)) 0UL -1 |> Async.RunSynchronously)
            FsPickler().Deserialize<Map<int,(int64*int64)>>(ms)

        let inline gettt (tapd,tapu) = taptimes |> Map.find tapd |> fst

        let getAlignedTimes (xrange:Range<float>) taligns spiketimes = 
            let dtbefore,dtafter = -xrange.Min,xrange.Max
            taligns
            |> Array.Parallel.map (fun talign ->
                spiketimes |> Array.filter (fun x -> x > talign-(dtbefore*30000.|>int64) && x < talign+(dtafter*30000.|>int64))
                |> Array.map (fun t -> (t-talign|>float)/30000.))

        let goodsts = 
            let goodcells =
                use ms = new MemoryStream(getBytes hostname (fpath @"sessiongoodcells") 0UL -1 |> Async.RunSynchronously)
                FsPickler().Deserialize<(int*(string*Set<string>)[])[]>(ms)
                |> Map.ofArray

            sms |> Map.filter (fun smid _ -> smid <> 2 && smid <> 368 && smid <> 602)
            |> Map.toArray
            |> Array.choose (fun (smid,trange) ->
                printfn "%d" smid
                let cells = 
                    Map.find smid goodcells
                    |> Array.filter (fst>>((=)"1"))
                if Array.length cells > 0 then
                    let sts =
                        cells 
                        |>Array.map (loadSpikeTimes trange hostname path)
                        |> Array.filter (fun x -> Array.length x > 0)
                        |> Array.maxBy Array.length
                    Some (smid,sts)
                else None
            )

        let goodtrialspikes () =
            let avgdts = [|0.6;0.2;0.35|]
            let getIPI trial = 
                match trial.Taps with
                | tap1::tap2::_ -> (gettt tap1) - (gettt tap2)
                | _ -> failwith "WTF"

            let get2TapIntervals trials = 
                let ntrials = Array.length trials
                let rawintervals = 
                    trials |> Array.map (fun trial ->
                        trial.Taps |> List.map (fun (tapd,_) -> Map.find tapd taptimes |> fun (a,b) -> [a;b]) |> List.concat
                        |> fun x -> List.head x, (x|>Seq.take 4|>diff|>Array.map (fun x -> (float x)/30000.))
                    )
                rawintervals
                |> Array.mapi (fun i (t0,dts) -> 
                    let y = i|>float
                    let tfs = 
                        Array.zip avgdts dts |> Array.scan (fun (meantcum,(tcum,_)) (meandt,dt) ->
                            let newtcum = tcum+dt
                            let newmeantcum = meantcum+meandt
                            newmeantcum,(newtcum,
                                transform (Range(tcum,newtcum)) (Range(meantcum,newmeantcum))
                            )
                        ) (0.,(0.,id))
                        |> Array.toList |> List.rev
                        |> fun x -> 
                            let meantcum,(tcum,_) = List.head x
                            (System.Double.MaxValue,fun a -> a-tcum+meantcum)::(x|>List.map snd)
                        |> List.rev |> List.toArray
                    t0,tfs
                )

            let getAlignedTimesTW (xrange:Range<float>) intervals spiketimes = 
                let dtbefore,dtafter = -xrange.Min,xrange.Max
                intervals
                |> Array.map (fun (t0,tfs) -> 
                    spiketimes |> Array.filter (fun x -> 
                        x > t0-(dtbefore*30000.|>int64) && x < t0+(dtafter*30000.|>int64)) 
                    |> Array.scan (fun (i,_) x ->                 
                        let straw = (x-t0|>float)/30000.
                        let tcum,f = Array.get tfs i
                        if (straw < tcum) then i else i+1
                        |> fun newi -> 
                            newi,
                            straw
                            |>(tfs.[newi]|>snd)
                            |>Some
                    ) (0,None)
                    |> Array.choose snd
                    |> Array.filter (fun x -> x < dtafter)
                )

            goodsts |> Array.map (fun (smid,sts) ->
                let trials = 
                    let goodipi t = 
                        let dt = getIPI t
                        dt >=700L*30L && dt <= 900L*30L
                    db2tap.get2TapTrials(exptid,smid) |> Seq.pairwise |> Seq.toArray |> Array.choose (fun (t1,t2) -> 
                        if t1.Reward |> Option.isSome && t2.Reward |> Option.isSome && goodipi t2 then Some t2 else None)
                    //|> Array.sortBy getIPI
                //let intervals = get2TapIntervals trials
                let alignedtimes = getAlignedTimes xrange (trials|>Array.map (fun t -> t.Taps |> List.head |> gettt)) sts
                smid,alignedtimes
            )

        let badtrialspikes () =
            goodsts |> Array.map (fun (smid,sts) ->
                let taligns = 
                    db2tap.get2TapTrials(exptid,smid) |> Seq.pairwise |> Seq.toArray |> Array.choose (fun (t1,t2) -> 
                        match t1.Reward,t1.Taps,t2.Taps with
                        | None,[tap1;tap2],tap3::_
                        | None,tap1::tap2::tap3::_,_ -> 
                            let t0 = tap2 |> gettt
                            let t1 = tap3 |> gettt
                            if (t1-t0) < 2000L*30L && (t1-t0) > 1000L*30L then Some (t1,t1-t0) else None
                        | _ -> None 
                    ) |> Array.sortBy snd |> Array.map fst
                printfn "%d" smid
                let alignedtimes = getAlignedTimes xrange taligns sts
                smid,alignedtimes
            )
        goodtrialspikes ()
    )
    |> Map.toArray |> Array.map snd
    |> Array.concat

let savegoodtrialspikes =
    use fs =  new FileStream(@"C:\temp\goodtrialspikesinorder",FileMode.Create)
    FsPickler().Serialize(fs,alignedtimes)

//let alignedtimes =
//    use fs =  new FileStream(@"C:\temp\badtrialspikes",FileMode.Open)
//    FsPickler().Deserialize<(int*float[][])[]>(fs)
//
let plotrastersall =
    let wnd,yrange,sps = 
        let x = 
            alignedtimes //|>Array.filter (snd>>Array.length>>fun x -> x >=40)
            |> Array.mapi (fun i (smid,x) -> 
                printfn "%d" smid
                x,brightColors.[i%(Array.length brightColors)]
                //Array.init 50 (fun i -> x.[i*(Array.length x)/50]),brightColors.[i%(Array.length brightColors)]
            )
        plotspikerasters xrange 0.1 x
    //let evtlines =
    //    [|0.6;0.2;0.35|]|>cumsum|>Seq.toArray|>Array.map (fun t -> (t,yrange.Min),(t,yrange.Max),Colors.LightGray)
    //    |> getLineList
    //    |> fun x -> addSubPlot2 wnd xrange yrange xrange yrange [x]      
    addSubPlot2 wnd xrange yrange xrange yrange [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList] |> ignore  
    wnd.Width <- 600.
    wnd.Height <- 600.
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore

let peths =
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    let dt = 0.1
    let trange = Range(0.0,50.0)
    let yrange = Range(0.0,150.0)
    let firststart,_ = db2tap.getSMTime(exptid,2).Value
    alignedtimes|>Array.filter (snd>>Array.length>>fun x -> x >=20)
    |> Array.map (fun (smid,x) -> 
        printfn "%d" smid
        (db2tap.getSMTime(exptid,smid).Value |> fun (start,dur) -> (start-firststart).TotalDays),
        x|>Array.concat|>Array.filter(fun t -> t > 0. && t < 1.5)|>Array.length|>float|>fun a -> (1./0.5)*a/(Array.length x|>float)
    )
    |> getLinexy Colors.Black
    |> fun x -> addSubPlot wnd trange yrange [x]
    |> ignore
    addXAxis wnd xrange Colors.Black "Time (days)" |> ignore
    addYAxis wnd yrange Colors.Black "Firing Rate (1/s)" |> ignore

#if COMPILED
[<System.STAThread()>]
(new Application()).Run() |> ignore
#endif

//plot.Save(@"Z:\badlands\635276396558304920\rasters.svg")

