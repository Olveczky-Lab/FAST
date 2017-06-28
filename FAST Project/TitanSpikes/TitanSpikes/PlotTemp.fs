#if COMPILED
module PlotTemp
#else
#r @"..\..\Math.Net Numerics\MathNet.Numerics.dll"
#r @"..\..\Math.Net Numerics\MathNet.Numerics.FSharp.dll"
#r @"bin\Debug\IntelIPP.dll"

#r @"WindowsBase.dll"
#r @"PresentationCore.dll"
#r @"PresentationFramework.dll"
#r @"System.Xaml.dll"
#r @"UIAutomationTypes.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"

#r @"bin\Debug\D3DPlot2D.dll"
#r @"bin\Debug\D3DPlot.dll"
#r @"bin\Debug\RPCommon.dll"

#r @"..\..\HDF5IO\bin\Debug\HDF5IO.dll"

#load "Helper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"
#load "RemoveNoise.fs"
#load "Cluster.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open Helper
open RemoveNoise
open RP.HDF5.H5TypeProviderRuntime

let channelgroups = [|1;5;7;8;10;11;12;15;16;17;20;21;22;23;26;28|]|>Array.map (sprintf "%d")
let channelstouse = [|0;1|]
let f0,s0,s1 = 635024291104749295L,687603712L,795115520L
let padding = 0L*30000L //1 hour
let fname = (sprintf "%s\%d.h5" @"C:\EphysData\Sullivan\Ashesh\Bairagi" f0)
let spikeindices =
    channelgroups |> Array.map (fun chgroup -> 
                        chgroup, Option.get <| getSpikesIndex (s0-padding) (s1+padding) fname chgroup)
let non_noise =
    spikeindices |> Array.map (fun (chgroup,(spikeoffset,spikecount)) -> 
                        printfn "%d %s %d %d" f0 chgroup spikeoffset spikecount
                        readData fname ("./ChGroup_" + chgroup + "/SpikeTimes") [|spikeoffset|] null [|spikecount|] :?> int64[]
                        |> Array.toList)
    |> getCorrelationCountAll
    |> Array.map (Array.map (fun x -> x <= 4))

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open RP.HDF5.H5TypeProviderRuntime
open Helper
open PlotHelper
open Cluster

let chgroup = "1"

let spike_max_indx = 31
let spikelen = 64

let totaltoread = 388573UL
let maxtoload = 388573UL //downsample to a max of 1 million spikes
let offset = 2582118UL

let chnums,spikes,spikesraw = getSpikes fname chgroup maxtoload totaltoread offset
let scaled_spikes = spikes |> Array.map (fun (st,s) -> scalespike (st,Array.init spikelen (fun i -> s.[i,1])))

let tmax,_ = scaled_spikes.[scaled_spikes.Length-1]
let tmin,_ = scaled_spikes.[0]
let xrange = Range(tmin,tmax)

let wnd = new D3DPlot2DWindow()
wnd.Plot.SelectionBoxEnabled <- true
wnd.Show()
addTimeXAxis wnd xrange |> ignore

let good,bad =
    Array.zip non_noise.[0] scaled_spikes
    |> Array.partition fst
    |> fun (x,y) -> x|>Array.map snd,y|>Array.map snd


plotSpikeAmps 3.0f xrange (Range(-1000.0e-6,0.0)) spike_max_indx wnd Colors.Black (sprintf "Ch %d (%sV)" chnums.[1]) [Colors.Blue,good;Colors.Red,bad]
                (fun r -> 
                    let inside = getSpikesInWindow spike_max_indx r.Left r.Right r.Top r.Bottom scaled_spikes
                    let nspk = Array.length inside
                    printfn "%d" nspk
                )

#if COMPILED
[<STAThread()>]
(new Application()).Run() |> ignore
#endif