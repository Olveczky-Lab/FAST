#if COMPILED
module PlotSpikesWithBehavior3
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

let hostname = Remote "140.247.178.16:5900"

let exptid = 27
let ratpath = @"/root/data/rpoddar/badlands"

let allsms =
    let fnum = 635276396558304920L
    let smids = [|32;36;45;49;58;61;70;73;82;85;94;97;109;112;121;124;133;136;145;148;157;160;169;172;181|]
    let smtimes =
        use ms = new MemoryStream(getArray<byte> hostname (sprintf "%s/%d/TrainingSMStarts.txt" ratpath fnum) 0UL -1)
        use sr = new StreamReader(ms)
        sr.ReadToEnd().Split('\n')|>Array.map uint64
        |> fun x -> x.[7..] |> Array.map (fun t -> t-30000UL*300UL,t+30000UL*3900UL)
    Array.zip smids smtimes |> Map.ofArray
    |> fun x -> [|fnum,x|]|>Map.ofArray

let xrange = Range(-3.,5.)
let alignedtimes =
    allsms |> Map.map (fun fnum sms ->
        let path =  sprintf "%s/%d" ratpath fnum
        let fpath =  sprintf "%s/%s" path

        let taptimes = 
            use ms = new MemoryStream(getBytes hostname (fpath (sprintf @"Behavior/%dLeverPressRhythm.tmap" exptid)) 0UL -1 |> Async.RunSynchronously)
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
                        |> Array.map (loadSpikeTimes trange hostname path)
                        |> Array.filter (fun x -> Array.length x > 0)
                    Some (smid,stss)
                else None
            )

        let goodtrialspikes () =
            let getIPI trial = 
                match trial.Taps with
                | tap1::tap2::_ -> (gettt tap2) - (gettt tap1)
                | _ -> failwith "WTF"

            let goodipi t = 
                let dt = getIPI t
                dt >=500L*30L && dt <= 600L*30L

            goodsts |> Array.map (fun (smid,stss) ->
                printfn "%d" smid
                let trials = 
                    get2TapTrials exptid smid |> Array.choose (fun t ->
                        if t.Reward |> Option.isSome && goodipi t then Some t else None)
//                    get2TapTrials exptid smid |> Seq.pairwise |> Seq.toArray |> Array.choose (fun (t1,t2) -> 
//                        if t1.Reward |> Option.isSome && t2.Reward |> Option.isSome && goodipi t2 then Some t2 else None)
//                    |> Array.sortBy getIPI
                let alignedtimes = 
                    stss |> Array.map (getAlignedTimes xrange (trials|>Array.map (fun t -> t.Taps |> List.head |> gettt)))
                    |> Array.map (fun x -> smid,x)
                alignedtimes
            )
        goodtrialspikes () |> Array.concat
    )
    |> Map.toArray |> Array.map snd
    |> Array.concat

let savealignespikes()  =
    let tosave =
        [|65;70;73;103;109;111;110;106;104;113;116;126;158;179;187;211;228;242;271;277|]
        |> Array.map (fun j -> alignedtimes.[j]|>snd)
    use fs =  new FileStream(@"C:\temp\Badlands27Good",FileMode.Create)
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

let peths =
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    let dt = 0.1
    let trange = Range(0.0,50.0)
    let yrange = Range(0.0,150.0)
    let firststart,_ = getSMTime exptid 2
    alignedtimes|>Array.filter (snd>>Array.length>>fun x -> x >=20)
    |> Array.map (fun (smid,x) -> 
        printfn "%d" smid
        (getSMTime exptid smid |> fun (start,dur) -> (start-firststart).TotalDays),
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

