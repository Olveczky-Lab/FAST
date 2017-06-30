#if COMPILED
module PlotAlignedAll
#else
#time
fsi.ShowDeclarationValues <- false
#load "SVGWriter.fs"
#endif

open System.IO
open Nessos.FsPickler
open Helper
open TwoTapHelper
open RP.Controls
open PlotHelper
open System.Windows.Media
open SVGWriter

let inline transpose xss =
    if Array.length xss = 0 then xss else
    Array.init (Array.length (Array.get xss 0)) (fun i -> xss |> Array.map (fun xs -> xs.[i]))

let getAlignedTimes (dtbefore,dtafter) taligns spiketimes = 
    taligns
    |> Array.Parallel.map (fun talign ->
        spiketimes |> Array.filter (fun x -> x > talign+(dtbefore*30000.|>int64) && x < talign+(dtafter*30000.|>int64))
        |> Array.map (fun t -> (t-talign|>float)/30000.))

let plotaligned xranges xs =
    let wnd,(xrange,xranges),yrange,sps = 
        plotspikerasters xranges 0.25 0.1 10. xs
    let plots =
        sps |> Array.map (Array.map (getSVGSubPlot 600. (50.*(xs|>Array.length|>float)))) |> Array.concat
    for xrange in xranges do
        addSubPlot2 wnd xrange yrange xrange yrange 
            [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList] |> ignore  
    wnd.Width <- 600.
    wnd.Height <- 600.
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    plots

let hostname,fpath = Remote "140.247.178.94:5900", sprintf @"/root/data/asheshdhawale/Data/Gunakari/%s"
let sms = 
    use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConHRTwoTapSMTimes") 0UL -1)
    FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms)
let trials =
    use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConTwoTapTrials") 0UL -1)
    FsPickler().Deserialize<((int * int) * (Trial [] * Map<int,int64>) option) []>(ms)
    |> Array.choose (fun (a,b) -> b |> Option.map (fun x -> a,x)) |> Map.ofArray
let goodcells =
    use ms = new MemoryStream(getArray<byte> hostname (fpath "GoodCellsSessionSpikeTimes") 0UL -1)
    FsPickler().Deserialize<(string*((int*int)*uint64[])[][]) []>(ms)
let hrtimes =
    use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConTwoTapHRTimes") 0UL -1)
    FsPickler().Deserialize<Map<int,Map<int,int64>>>(ms)

let taligns = 
    sms |> Array.map (fun ((exptid,smid) as sm,_) ->
        let hrtimes = hrtimes.[exptid]
        trials.[sm] |> fst |> Array.choose (fun trial ->
            trial.Reward |> Option.bind (fun r -> 
                r.PulseTrain|>Option.map (fun p -> 
                    seq {
                        match trial.Taps with
                        | [t1d,t1u;t2d,t2u] ->
                            yield! [t1d;t1u;t2d;t2u] 
                        |_ -> ()
                        yield p.PulseRCEID
                    } |> Seq.toArray |> Array.map (fun rceid -> hrtimes.[rceid])
                )
            )            
        ) |> Array.sortBy (fun xs -> xs.[2] - xs.[0]) 
        |> fun xss -> Array.length xss,transpose xss
    ) |> Array.zip (sms |> Array.map fst) |> Map.ofArray

let ((exptid,smid) as sm),(fnum,(t0,t1)) = sms.[90]

let axl = 
    let raw = getAXL hostname (fpath (sprintf "%d/AXL" fnum)) ((t0-3000L)/4L|>uint64) ((t1-t0)/4L|>int)
    let nchans, nsamps = Array2D.length2 raw, Array2D.length1 raw
    Array.init nchans (fun ch -> Array.init nsamps (fun i -> raw.[i,ch]|>float))
let xrange = Range(-1.,3.)
let alignedaxls =
    let curtaligns = taligns.[sm]|>snd|>fun x -> x.[0]
    let inline f t = (t-t0)/4L|>int
    curtaligns|>Array.map (fun t ->
        axl |> Array.map (fun x -> [|f (t+(xrange.Min*30000.|>int64))..10..f (t+(xrange.Max*30000.|>int64))|] |> Array.map (Array.get x))
    ) |> fun x -> x.[50..69]
let min,max = 
    let x = alignedaxls |> Array.concat |> Array.concat |> Array.sort
    let l = Array.length x
    x.[l*1/100],x.[l*99/100]
let ntrials = Array.length alignedaxls
let yrange = Range(0.,ntrials|>float)
let wnd = new D3DPlot2DWindow()
wnd.Show()
let yranges = aggRange wnd yrange (Array.init ntrials (fun i -> (i|>float,1.0),(min,max)))
let sps = 
    Array.zip alignedaxls yranges |> Array.map (fun (ls,r) ->
        addSubPlot2 wnd xrange r xrange r
            (Array.map2 (fun l c -> getLinexy2 c (l |> Array.mapi (fun i x -> -1. + (1./750.)*(i|>float),x))) 
                ls [|Colors.DarkGreen;Colors.Red;Colors.Blue|] |> Seq.cast)
    )
addXAxis wnd xrange Colors.Black "Time" |> ignore
addYAxis wnd yrange Colors.Black "" |> ignore

let tranges = [|(-3.0,5.0)|]

let alignedtimes =
    goodcells |> Array.map snd |> Array.concat |> Array.concat |> Array.choose (fun (smcur,sts) -> 
        if smcur = sm then Some (sts|>Array.map int64) else None)    
    |> Array.mapi (fun i sts ->
        let ntrials,taligns = taligns.[sm]
        let taligns = taligns.[0..0]
        Array.zip tranges taligns |> Array.map (fun (trange,taligns) -> 
            getAlignedTimes trange taligns (sts|>Array.map int64))
        ,brightColors.[i%Array.length brightColors]
    ) 

let r = plotaligned tranges alignedtimes

