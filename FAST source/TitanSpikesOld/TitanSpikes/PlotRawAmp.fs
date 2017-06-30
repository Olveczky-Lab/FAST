#if COMPILED
module PlotRawAmp
#else

#load "Helper.fs"
#load "HDF5Helper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"
#load "MathHelper.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open System
open System.Windows.Media
open System.Windows
open RP.Controls
open System.IO

open Helper
open PlotHelper
open MathHelper

[<Measure>] type s //seconds
[<Measure>] type hz = 1/s
//let chnum = 0
let sramp = 30000.0<hz>
let duration = 5.0<s>
let chnums = [|0..63|]
let path = sprintf @"Z:\dudley\%s.amp"
let (fnum,samplenum) = "635202176618351961",(30000L*((65790)|>int64))

let sampletime = DateTime(fnum|>int64).AddSeconds((samplenum|>float)/30000.)

let amps rawampfname starttime = 
    use fs = new FileStream(rawampfname,FileMode.Open,FileAccess.Read)
    let numsamples = sramp*duration |> int
    let b = Array.zeroCreate<byte> (numsamples*64*2)
    fs.Seek((sramp*starttime|>int64)*64L*2L,SeekOrigin.Begin) |> ignore
    fs.Read(b,0,b.Length) |> ignore
    chnums |> Array.map (fun chnum -> 
        scaleData 32768.0 numsamples b chnum 
        |> Array.map (fun x -> x*0.195e-6) |> filtfilt spikes_bandpass
    )

let inline sampletotime x = (x|>float)/sramp

let curamps = amps (path fnum) (samplenum|>sampletotime)

let calcMAD = 
    let MAD d =
        let med d =
            let sorted_d = d |> Array.sort
            sorted_d.[d.Length/2]
        let m = med d
        med (d|> Array.map (fun x -> abs (x - m)))
    
    curamps |> Array.map (fun d -> (MAD d)/1e-6) |> Array.sort |> Seq.iter (printfn "%.2f")    

let wnd = new D3DPlot2DWindow()
wnd.Show()
let xrange = Range(0.0,duration/1.0<s>)
addTimeXAxis wnd xrange |> ignore
let yrangeampraw = Range(-400e-6,200e-6)
addScaledYAxis wnd yrangeampraw Colors.Black (sprintf "Raw (%sV)") |> ignore
let sp = addSubPlot wnd xrange yrangeampraw []

let inline plotline chnum =
    wnd.Title <- sprintf "%d" chnum
    let ampline =  
        curamps.[chnum]              
        |> fun amp -> (getLine Colors.Black 0.0 (1.0<hz>/sramp) amp)
    sp.Lines <- [|ampline|]
    wnd.Plot.ForceRefresh()

plotline 0
wnd.KeyDown |> Observable.scan (fun x _ -> x+1) 0 |> Observable.subscribe (fun x -> plotline x) |> ignore

