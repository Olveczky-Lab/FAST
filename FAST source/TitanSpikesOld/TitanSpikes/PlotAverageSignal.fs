#if COMPILED
module PlotAverageSignal
#else

#load "Helper.fs"
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
let duration = 0.1<s>

let filta = [|1.000000000000000; -2.539340515376365; 2.678216265216509; -1.312704990070111; 0.263921227672945; |]
let filtb = [|0.495801912843700; -1.926015700863270; 2.861328495913542; -1.926015700863267; 0.495801912843699; |]

let amps rawampfname starttime = 
    use fs = new FileStream(rawampfname,FileMode.Open,FileAccess.Read)
    let numsamples = sramp*duration |> int
    let b = Array.zeroCreate<byte> (numsamples*64*2)
    fs.Seek((sramp*starttime|>int64)*64L*2L,SeekOrigin.Begin) |> ignore
    fs.Read(b,0,b.Length) |> ignore
    let chnums = [|0..63|]
    let numchs = Array.length chnums
    chnums
    |> Array.map (fun chnum ->
                    scaleData 32768.0 numsamples b chnum 
                    |> Array.map (fun x -> x*0.195e-6) |> filtfilt (filta,filtb))
    |> fun x -> 
        Array.init numsamples (fun i -> Array.init (numchs) (fun chnum -> x.[chnum].[i]) |> Array.average)

let wnd = new D3DPlot2DWindow()
wnd.Show()
let xrange = Range(0.0,duration/1.0<s>)
addTimeXAxis wnd xrange |> ignore
let yrangeampraw = Range(-1e-4,1e-4)
addScaledYAxis wnd yrangeampraw Colors.Black (sprintf "Raw (%sV)") |> ignore
let inline sampletotime x = (x|>float)/sramp-(duration/2.0)
let samplenums = 
    File.ReadAllLines(@"C:\Users\rpoddar\Desktop\samplenums.txt") 
    |> Array.map (fun x -> match x.Split(' ') with
                           | [|fname;samplenum|] -> fname,samplenum|>int64
                           | _ -> failwith "Invalid data in file"
                           )
    |> Array.toList

let sp = addSubPlot wnd xrange yrangeampraw []
let inline plotline (fnum,samplenum) =
    wnd.Title <- sprintf "%s %d" fnum samplenum
    let ampline =
        amps (sprintf @"Z:\bairagi\raw\%s.amp" fnum) (samplenum|>sampletotime)
        |> fun amp -> (getLine Colors.Black 0.0 (1.0<hz>/sramp) amp)
    sp.Lines <- [|ampline|]
    wnd.Plot.ForceRefresh()

plotline (List.head samplenums)

wnd.KeyDown
|> Observable.filter (fun args -> args.Key = Input.Key.Space)
|> Observable.scan (fun xs _ -> 
                        match xs with
                        | [] -> []
                        | h::t -> 
                            plotline h
                            t) (List.tail samplenums)
|> Observable.subscribe (fun _ -> ())
|> ignore
