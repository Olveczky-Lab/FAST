#if COMPILED
module PlotSpikesWithBehavior2
#else
#load "PlotHelper.fs"
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
let xrange = Range(-3.,5.)

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

let alignedtimes =
    allsms |> Map.filter (fun fnum _ -> fnum >= 635368345668632516L) |> Map.map (fun fnum sms ->
        let path =  sprintf "%s/%d" ratpath fnum
        let fpath =  sprintf "%s/%s" path

        let taptimes = 
            use ms = new MemoryStream(getBytes hostname (fpath (sprintf @"Behavior/%dLeverPressOpCon.tmap" exptid)) 0UL -1 |> Async.RunSynchronously)
            FsPickler().Deserialize<Map<int,(int64*int64)>>(ms)

        let inline gettt rceid = taptimes |> Map.find rceid |> fst

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

            sms
            |> Map.toArray
            |> Array.choose (fun (smid,trange) ->
                printfn "%d" smid
                let cells = 
                    Map.find smid goodcells
                if Array.length cells > 0 then
                    let stss =
                        cells 
                        |>Array.map (loadSpikeTimes trange hostname path)
                        |> Array.filter (fun x -> Array.length x > 0)
                    Some (stss|>Array.map (fun sts -> smid,sts))
                else None
            ) |> Array.concat

        let goodtrialspikes () =
            let getIPI trial = 
                match trial.Taps with
                | tap1::tap2::_ -> (gettt tap2) - (gettt tap1)
                | _ -> failwith "WTF"

            let goodipi t = 
                let dt = getIPI t
                dt >=700L*30L && dt <= 800L*30L

            goodsts |> Array.map (fun (smid,sts) ->
                let trials = 
                    get2TapTrials exptid smid |> Array.choose (fun t ->
                        if t.Reward |> Option.isSome && goodipi t then Some t else None)
//                    get2TapTrials exptid smid |> Seq.pairwise |> Seq.toArray |> Array.choose (fun (t1,t2) -> 
//                        if t1.Reward |> Option.isSome && t2.Reward |> Option.isSome && goodipi t2 then Some t2 else None)
//                    |> Array.sortBy getIPI
                let alignedtimes = getAlignedTimes xrange (trials|>Array.map (fun t -> t.Taps |> List.head |> gettt)) sts
                smid,alignedtimes
            )
        goodtrialspikes ()
    )
    |> Map.toArray |> Array.map snd
    |> Array.concat

let savesalignespikes()  =
    let tosave =
        [|5;6;7|]
        |> Array.map (fun j -> alignedtimes.[j]|>snd)
    use fs =  new FileStream(@"C:\temp\Gandhar79Good",FileMode.Create)
    FsPickler().Serialize(fs,tosave)

let plotrastersall =
    let wnd,yrange,sps = 
        let x = 
            alignedtimes 
            //|> Array.filter (fun (_,x) -> Array.length x >= 10)
            |> Array.mapi (fun i (smid,x) -> 
                printfn "%d" smid
                x,brightColors.[i%(Array.length brightColors)]
                //Array.init 10 (fun i -> x.[i*(Array.length x)/10]),brightColors.[i%(Array.length brightColors)]
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

#if COMPILED
[<System.STAThread()>]
(new Application()).Run() |> ignore
#endif

