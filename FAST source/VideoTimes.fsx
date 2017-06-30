#load @"D:\Rajesh\Ephys\FlexiPhys\PlotBin\PlotHelper.fsx"

open System.IO
open System
open PlotHelper
open RP.Controls
open System.Windows.Media

let fname = @"Z:\dudley\WebCam\635205697864876242.times"

let times = 
    use fs = new FileStream(fname,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
    let raw = Array.zeroCreate<byte> (fs.Length|>int)
    fs.Read(raw,0,raw.Length)|>ignore
    Array.init (raw.Length/8) (fun i -> System.BitConverter.ToInt64(raw,i*8))

PlotHelper.plottimeline "FrameTimes" 1.0 (times|>Array.map float)
