#if COMPILED
module PlotSpikes
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

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot2D.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\RPCommon.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"

#load "Helper.fs"
#load "AmpDataHelper.fs"
#load "PlotHelper.fs"
#load "Cluster.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open RP.HDF5.H5TypeProviderRuntime
open Helper
open PlotHelper
open Cluster

[<Literal>]
let fname = @"C:\EphysData\Sullivan\Ashesh\635025802044762119.h5"

let chgroup = "1"
let dims = getDataSetDimensions fname (sprintf "./ChGroup_%s/SpikeTimes" chgroup)

let spike_max_indx = 31
let spikelen = 64

let totaltoread = dims.[0]
let maxtoload = 1000000UL //downsample to a max of 1 million spikes
let offset = 0UL

let chnums,spikes,spikesraw = getSpikes fname chgroup maxtoload totaltoread offset
let scaled_spikes = spikes |> Array.map (fun (st,s) -> scalespike (st,Array.init spikelen (fun i -> s.[i,1])))

let wnd = new D3DPlot2DWindow()
wnd.Show()
let tmax,_ = scaled_spikes.[scaled_spikes.Length-1]
let xrange = Range(-5.0,tmax)
addTimeXAxis wnd xrange |> ignore

plotSpikeAmps 3.0f xrange (Range(0.0,800.0e-6)) spike_max_indx wnd Colors.Black (sprintf "Ch %d (%sV)" chnums.[0]) [Colors.Blue,scaled_spikes]
                (fun r -> 
                    let inside = getSpikesInWindow spike_max_indx r.Left r.Right (-r.Bottom) (-r.Top) scaled_spikes
                    let nspk = Array.length inside
                    if (nspk > 0 && nspk < 2000) then
                        let mintemp =  0.0
                        let tempstep = 0.01
                        let maxtemp =  0.2
                        let wnd = new D3DPlot2DWindow()
                        wnd.Show()
                        wnd.Title <- sprintf "Number of Spikes = %d" nspk

                        let clus = wave_clus 15 mintemp (maxtemp+tempstep/2.0) tempstep 200 11 0 @"C:\EphysData\Sullivan\Ashesh\temp" spikesraw inside [|0|]
                        let clus_counts = clus |> Array.map (fun clu -> clu |> Seq.countBy id)
                        let clus_groups = clus |> Array.map (fun clu -> clu |> Seq.groupBy id  |> Seq.sortBy fst)
                        let xrange = Range(mintemp-tempstep,maxtemp+tempstep)
                        let yrange = Range(0.0,(log10 (float nspk))*1.1)
                        addXAxis wnd xrange |> ignore
                        addYAxis wnd yrange Colors.Black ("Number of Spikes") |> ignore
                        addSubPlot wnd xrange yrange [getPoints 10.0f Colors.Black 
                                                        (clus_counts 
                                                         |> Seq.mapi (fun i x -> x |> Seq.map (fun (_,c) -> (mintemp+tempstep*(float i)),log10 (float c)))
                                                         |> Seq.concat)] |> ignore
                        clus
                        |> Seq.iteri (fun i clu ->
                                        plotlines (sprintf "Temperature %f" (mintemp + (float i)*tempstep)) (Range(-0.5,((float spikelen)-0.5)))

                                           (clu
                                            |> Seq.mapi (fun i groupnum ->
                                                            let col = match groupnum with
                                                                      | 0us -> Colors.Black
                                                                      | 1us -> Colors.Blue
                                                                      | 2us -> Colors.Red
                                                                      | 3us -> Colors.Green
                                                                      | _ -> Colors.Gray
                                                            inside.[i],col)
                                            |> Seq.toArray) scaled_spikes)
                    )

#if COMPILED
[<STAThread()>]
(new Application()).Run() |> ignore
#endif