#if COMPILED
module PlotTwoTapBehvaior
#else
#load "SVGWriter.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open IntelIPP
open PlotHelper
open System.Windows.Media
open System.Windows.Media.Imaging
open RP.Controls
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open TwoTapHelper
open MathNet.Numerics.Statistics
open Helper
open System.IO
open Nessos.FsPickler
open SVGWriter

let inline getipis (ts,m) = 
    let inline f x = Map.find x m
    ts |> Array.choose (fun t -> 
        match t.Taps with
        | (t1,_)::(t2,_)::_ -> Some (f t2 - f t1 |> float)
        | _ -> None
    )

let rat = "arches"
let hostname = Remote "140.247.178.16:5900"
let fpath = sprintf @"/root/data/rpoddar/%s/%s" rat
let cstr = 
    "Data Source=140.247.178.203,5900;Initial Catalog=OpConRPODDARTwoTap;User ID=rajeshpoddar;Password=htsn0f00d!",
    "TwoTapTrialAndErrorV2"
//let cstr = 
//    "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!",
//    "TwoTapTrialAndErrorV2"

let sms = 
    let db2tap = db2tap(cstr)
    use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConHRTwoTapSMTimes") 0UL -1)
    let smtimes = FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms)
    let smrewards =
        smtimes
        |> Array.map (fst>>fst) |> Seq.distinct
        |> Seq.map (fun exptid -> db2tap.getTwoTapSMs(exptid) |> Seq.map (fun (smid,x) -> (exptid,smid),x))
        |> Seq.concat |> Map.ofSeq
    smtimes |> Array.map (fun (sm,_) -> sm,smrewards.[sm])

let trials =
    use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConTwoTapTrials") 0UL -1)
    FsPickler().Deserialize<((int * int) * (Trial [] * Map<int,int64>) option) []>(ms)
    |> Array.choose snd

let plotheatmap = 
    let hm = 
        let ((y0,y1),yn) as yp = (0.,1200.),1000
        let ipis = 
            trials |> Array.map getipis |> Seq.concat
    //                |> Seq.filter (fun x -> x > 0.)
    //                |> Seq.map log
            |> Seq.filter (fun ipi -> ipi > y0 && ipi < y1)
            |> Seq.toArray
        let (_,xn) as xp = (0.0,Array.length ipis-1|>float),1000
        let x = imhist (4.f,4.f) xp yp (ipis |> Array.mapi (fun i x -> i|>float,x))
        let x' = x |> convert2Dto1D
        getheatmap (x' |> Array.min, x' |> Array.max) x
    //saveimage hm (sprintf @"Z:\FiguresForDAC\IPIHeatMap_%s.tif" rat)
    let wnd = dispimg hm
    wnd.Title <- rat
    wnd.Show()

let plotrewardscatter = 
    let cols = [|Colors.Black;Colors.Blue;Colors.Cyan;Colors.Green;Colors.Yellow;Colors.Red|]
    let xs = 
        seq {
            for (x,m),((_,_),(l,u,_)) in Array.zip trials sms do
                let inline f a = Map.find a m
                for t in x do
                    match t.Taps with
                    | (t1d,t1u)::(t2d,t2u)::_ ->
                        let a = ((f t2d - f t1u |> float),((f t1u - f t1d |> float)))
                        let nr = 
                            match t.Reward with
                            | Some x ->
                                match x.PulseTrain with
                                | Some y -> y.NumPulses
                                | _ -> 0
                            | _ -> 0
                        yield (a, cols.[nr]),(l,u)
                    | _ -> ()
        }
        |> Seq.toArray
    let n = Array.length xs    
    let wndsize,step = 500,100
    let ys i =  xs.[i*step..i*step+wndsize-1]
    let xrange = Range(0.,1200.)
    let yrange = Range(0.,1200.)
    let scatterplot = 
        xs.[n - 500..] |> Array.map fst
        |> Seq.groupBy snd |> Seq.map (fun (col,xs) ->
            getSVGPointList 300. 300. xrange yrange 1. col (xs |> Seq.map fst)
        )
    let inline f b = (b|>float32)/255.0f
    let wnd = new D3DPlot2DWindow()
    wnd.Show()    
    let initpos = 0
    let ys' = ys initpos
    wnd.Title <- sprintf "%s %d %d" rat (ys'.[0]|>snd|>fst) (ys'.[0]|>snd|>snd)
    let l =
        ys initpos |> Array.map (fun ((y,col),(_)) -> y,(xrange.Interval()/100.,yrange.Interval()/100.0),col)
        |> getMarkerPoints
        |> fun l -> 
            addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
            l.data
    wnd.KeyDown |> Observable.scan (fun i args ->
        min (i+1) ((n-wndsize)/step-1)
    ) initpos
    |> Observable.subscribe (fun i ->
        let ys' = ys i
        for j in 0..(Array.length l-1) do
            let ((x,y),col),_ = ys'.[j]
            l.[j].x <- x |> float32
            l.[j].y <- y |> float32
            l.[j].r <- f col.R
            l.[j].g <- f col.G
            l.[j].b <- f col.B            
        wnd.Title <- sprintf "%s %d %d" rat (ys'.[0]|>snd|>fst) (ys'.[0]|>snd|>snd)
        wnd.Plot.ForceRefresh()
    ) |> ignore
    addXAxis wnd xrange Colors.Black "IPI (ms)" |> ignore
    addYAxis wnd yrange Colors.Black "PD (ms)" |> ignore
