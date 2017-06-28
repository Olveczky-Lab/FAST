#load @"D:\Rajesh\Ephys\FlexiPhys\PlotBin\PlotHelper.fsx"
fsi.ShowDeclarationValues <- false
open System.IO
open System
open PlotHelper
open RP.Controls
open System.Windows.Media

let fnum = 635267850891815899L

let fifodata = 
    use fs = new FileStream(sprintf @"Z:\acadia\%d.fifo" fnum,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
    let raw = Array.zeroCreate<byte> (fs.Length|>int)
    fs.Read(raw,0,raw.Length)|>ignore
    Array.init (raw.Length/4) (fun i -> System.BitConverter.ToUInt32(raw,i*4))

PlotHelper.plottimeline "" 1.0 (fifodata|>Array.map float)

fifodata |> Array.max |> printfn "%A"

fifodata |> Array.mapi (fun i x -> (i,x)) |> Array.filter (fun (i,x) -> x > 1000000u)
|>Array.iter (fun (i,x) -> printfn "%d\t%d" i x)
