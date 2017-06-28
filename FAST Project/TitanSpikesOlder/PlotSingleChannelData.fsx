#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.IO.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.FSharp.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\IntelIPP.dll"

#r @"WindowsBase.dll"
#r @"PresentationCore.dll"
#r @"PresentationFramework.dll"
#r @"System.Xaml.dll"
#r @"UIAutomationTypes.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot2D.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\RPCommon.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"

#load "D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\Helper.fs"
#load "D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\AmpDataHelper.fs"

#time
fsi.ShowDeclarationValues <- false

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open Helper
open System.IO
open AmpDataHelper

let spike_max_indx = 31

let calcThresholds blocksize b chnum =
    let d = scaleData 32768.0 blocksize b chnum |> filtfilt spikes_bandpass
    let mad = MAD d
    let sd = mad*1.4826
    (sd*5.0,sd*2.0)

let fnum = "635025802044762119"
let path = @"\\ephys1\E\Data\"
let fname = path + fnum + ".amp"
let sr = 30000 // sampling rate
let f = new FileInfo(fname)
let numchannels = 64
let dfact = 100 //downsamplefactor for LFP

let chnums = [11;12]

let thrs =   
    let blocksize = sr*10 //10seconds
    let b = Array.zeroCreate<byte> (blocksize*2*64)
    use fs = f.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    fs.Read(b,0,b.Length) |> ignore
    chnums |> List.map (calcThresholds blocksize b)

let blocksize = sr*30 //30 seconds
let endPadding = sr/10 // 100 ms
let b = 
    use fs = f.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    fs.Seek((2.09*30000.0|>int64)*2L*64L,SeekOrigin.Current) |> ignore
    let b = Array.zeroCreate<byte> ((blocksize+endPadding*2)*2*64)
    let numRead = fs.Read(b,0,b.Length)
    b

let extractSpikes blocksize endPadding (thrs:(float*float)list) b =
    let ds =
        List.zip thrs chnums
        |> List.map (fun ((event_thr,return_thr),chnum) ->
                        let d_s =
                            scaleData (32768.0) (blocksize+endPadding*2) b chnum
                        let downsampled_d = d_s |> downsample100
                        let d = d_s |> filtfilt spikes_bandpass
                        (event_thr,return_thr,d),downsampled_d.[endPadding/dfact..(endPadding+blocksize)/dfact-1])
    let spikes = ds |> List.map fst |> List.toArray |> getSpikes endPadding
    ds |> List.map (fun ((_,_,d),lfp) -> (d,lfp)),spikes

let ds,spikes = 
    extractSpikes blocksize endPadding thrs b

let wnd = new D3DPlot2DWindow()
wnd.Show()
let xrange = Range(0.0,30.0)
addTimeXAxis wnd xrange |> ignore
let plot chindx event_thr d =
    let yrangeampraw = Range(-1e-3,1e-3)
    addScaledYAxis wnd yrangeampraw Colors.Black (sprintf "Raw (%sV)") |> ignore
    let spikes_s = spikes |> List.map (fun (s:float list [],st) -> st/30000.0,s.[chindx] |> List.toArray |> Array.map ((*)0.195e-6))
    let ampl = getPoints Colors.Blue (spikes_s |> List.map (fun (st,s:float[]) -> st,s.[spike_max_indx]))
    let spikesl = spikes_s |> List.map (fun (st,s) -> getLine Colors.HotPink (st-(float spike_max_indx)/(30000.0)) (1.0/30000.0) s)
    let event_thr_s = event_thr*0.195e-6
    addSubPlot wnd xrange yrangeampraw
        ((ampl
          ::(getLine Colors.Blue 0.0 30.0 [|event_thr_s;event_thr_s|])
          ::(getLine Colors.Blue 0.0 30.0 [|-event_thr_s;-event_thr_s|])
          ::(getLine Colors.Black 0.0 (1.0/30000.0) (d|>Array.map ((*)0.195e-6))
          ::spikesl)) |> Seq.cast) |> ignore
    wnd.KeyDown.Add (fun args ->    let delta_s, delta_a = 
                                        match args.Key with
                                        | Input.Key.D -> 1.0f,0
                                        | Input.Key.A -> -1.0f,0
                                        | Input.Key.S -> 0.0f,-25
                                        | Input.Key.W -> 0.0f,25
                                        | _ -> 0.0f,0
                                    ampl.Width <- ampl.Width + delta_s
                                    ampl.Height <- ampl.Height + delta_s
                                    ampl.Color <- Color.FromArgb(byte (clamp 0 255 ((int ampl.Color.A) + delta_a)),ampl.Color.R,ampl.Color.G,ampl.Color.B)
                                    wnd.Plot.ForceRefresh())
List.zip thrs ds
|> List.iteri (fun i ((event_thr,_),(d,_)) -> plot i event_thr d)

