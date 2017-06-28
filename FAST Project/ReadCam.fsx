#load @"D:\Rajesh\Ephys\FlexiPhys\PlotBin\PlotHelper.fsx"
fsi.ShowDeclarationValues <- false
open System.IO
open System
open PlotHelper
open RP.Controls
open System.Windows.Media

let metafile = 
    let fnum = "635267634229596668"
    sprintf @"Z:\acadia\Cam2\%s\%s.meta" fnum fnum

let framenums,ts =
    use fs = new FileStream(metafile,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
    use br = new BinaryReader(fs)
    let tmp = Array.init ((fs.Length|>int)/4) (fun i -> br.ReadUInt32())
    Array.init ((Array.length tmp)/2) (fun i -> tmp.[2*i]),Array.init ((Array.length tmp)/2) (fun i -> tmp.[2*i+1])
printfn "%d" (framenums.[framenums.Length-1]-framenums.[0]-(framenums.Length|>uint32))
PlotHelper.plottimeline "" 1.0 (framenums |> Array.mapi (fun i x -> x - framenums.[0]-(i|>uint32))  |> Array.map float)

let timesfile = @"Z:\arches\635248654304065837\635248654304065837.times"

let times =
    use fs = new FileStream(timesfile,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
    use br = new BinaryReader(fs)
    (Array.init ((fs.Length|>int)/8) (fun i -> br.ReadInt64()|>float)).[15..]

PlotHelper.plottimeline "" 1.0 (Array.init (Array.length times - 1) (fun i -> times.[i+1] - times.[i]))

let df = 8.33//((times.[times.Length-1]-times.[2000]))/((times.Length-2001)|>float)
PlotHelper.plottimeline "" 1.0 (Array.init (Array.length times) (fun i -> times.[i] - (times.[0]+(i|>float)*df)))

