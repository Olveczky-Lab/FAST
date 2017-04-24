#if COMPILED
module PlotSpikes
#else

#load "Helper.fs"
#load "HDF5Helper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"
#load "Cluster.fs"
#load "RemoveNoise.fs"

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
open RemoveNoise
open HDF5Helper

let f0,s0,s1 = 635025802044762119L,8682375247L,8790114304L
let fname = (sprintf "%s\%d.h5" @"C:\EphysData\Sullivan\Ashesh\Bairagi" f0)

let chgroup,chnum = "26",0

let spike_max_indx = 31+64*chnum
let spikelen = 64

//saveNonNoise fname [|"1";"5";"7";"8";"10";"13";"14";"15";"20";"21";"22";"23";"26"|] s0 s1
let offset,numspikes = Option.get <| getSpikesIndex s0 s1 fname chgroup

let chnums,spikes,spikesraw = getSpikes fname chgroup numspikes numspikes offset
let scaled_spikes = spikes 
                    |> Array.map 
                        (fun (st,s) -> 
                            scalespike (st, Array.init (Array2D.length2 s) 
                                            (fun i -> Array.init (Array2D.length1 s) (fun j -> s.[j,i]))
                                            |> Array.concat))
//let non_noise = loadNonNoise fname chgroup s0 |> Array.map (fun cc -> cc <= 7)

let tmax,_ = scaled_spikes.[scaled_spikes.Length-1]
let tmin,_ = scaled_spikes.[0]
let xrange = Range(tmin,tmax)

let wnd = new D3DPlot2DWindow()
wnd.Plot.SelectionBoxEnabled <- true
wnd.Show()
addTimeXAxis wnd xrange |> ignore

//let non_noisespikes,noisespikes = 
//    scaled_spikes |> Array.zip [|0..(numspikes|>int)-1|] |> Array.partition (fun (i,_) -> non_noise.[i])
//    |> fun (x,y) -> (x|>Array.map snd,y|>Array.map snd)
plotSpikeAmps 3.0f xrange (Range(-600.0e-6,0.0)) spike_max_indx wnd Colors.Black (sprintf "Ch %d (%sV)" chnums.[chnum]) 
                [Colors.Blue,scaled_spikes]
                (fun r -> 
                    let inside = getSpikesInWindow spike_max_indx r.Left r.Right r.Top r.Bottom scaled_spikes
                                 //|> Array.filter (fun i -> not (non_noise.[i]))
                    let nspk = Array.length inside
                    if (nspk > 0 && nspk < 2000) then
                        let mintemp =  0.01
                        let tempstep = 0.01
                        let maxtemp =  0.2
                        let waveletfeatures = getFeatures spikesraw inside [|getwavelets [|0|] 64|]
                                              |> cullfeaturesks 20
                        let ampfeatures = getFeatures spikesraw inside [|getspikeamp [|(31,0)|]>>(Array.map (fun x -> x*2.0))|]
                        let features = Array.zip waveletfeatures ampfeatures |> Array.map (fun (x,y) -> Array.append x y)
                        let clus = wave_clus mintemp (maxtemp+tempstep/2.0) tempstep 300 11 0 
                                    @"C:\EphysData\Sullivan\Ashesh\temp\temp" features
                        let ntemp = clus |> Array.length
                        plotClusters (Array.zip inside clus) 
                                      mintemp maxtemp tempstep scaled_spikes
                )

#if COMPILED
[<STAThread()>]
(new Application()).Run() |> ignore
#endif
