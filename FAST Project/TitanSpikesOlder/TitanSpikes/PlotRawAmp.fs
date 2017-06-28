#if COMPILED
module PlotRawAmp
#else
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

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\D3DPlot2D.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\D3DPlot.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\RPCommon.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Release\HDF5IO.dll"

#load "Helper.fs"
#load "AmpDataHelper.fs"
#load "PlotHelper.fs"

#endif

open System
open System.Windows.Media
open System.Windows
open RP.Controls
open System.IO

open Helper
open PlotHelper
open AmpDataHelper

let rawampfname = @"Z:\bairagi\raw\635021511117167662.amp"

[<Measure>] type s //seconds
[<Measure>] type hz = 1/s
let chnums = [4]
let sramp = 30000.0<hz>
let totalduration = (FileInfo(rawampfname).Length |> float) / 2.0 / 64.0 / sramp
let starttime = totalduration*0.90+0.0<s>
let duration = 30.0<s>

let amps = 
    use fs = new FileStream(rawampfname,FileMode.Open,FileAccess.Read)
    fs.Seek((sramp*(starttime)|>int64)*64L*2L,SeekOrigin.Begin) |> ignore
    let numsamples = sramp*duration |> int
    let b = Array.zeroCreate<byte> (numsamples*64*2)
    fs.Read(b,0,b.Length) |> ignore
    [| for chnum in chnums -> scaleData 32768.0 numsamples b chnum |> Array.map (fun x -> x*0.195e-6) |> filtfilt spikes_bandpass|]

let wnd = new D3DPlot2DWindow()
wnd.Show()
let xrange = Range(0.0,duration/1.0<s>)
addTimeXAxis wnd xrange |> ignore

let yrangeampraw = Range(-0.5e-3,1.5e-3)
addScaledYAxis wnd yrangeampraw Colors.Black (sprintf "Raw (%sV)") |> ignore
let amplines = amps |> Seq.map (getLine Colors.Black 0.0 (1.0<hz>/sramp)) |> Seq.cast
addSubPlot wnd xrange yrangeampraw amplines |> ignore
