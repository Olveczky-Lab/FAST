module ClusterViewer

open RP.Controls
open System.Windows.Media
open System.Windows
open RP.HDF5.H5TypeProviderRuntime
open Helper
open PlotHelper
open ClusterPicker

let fname = @"C:\EphysData\Sullivan\Ashesh\635025802044762119.h5"
let chgroup = "26"

let spike_max_indx = 31
let numperclustering = 1000

let spikes,clus = getSpikesAndClusters fname chgroup (0UL*700000UL) 700000
let scaled_spikes = spikes 
                    |> Array.map 
                        (fun (st,s) -> 
                            scalespike (st, Array.init (Array2D.length2 s) 
                                            (fun i -> Array.init (Array2D.length1 s) (fun j -> s.[j,i]))
                                            |> Array.concat))

let spikelen = scaled_spikes.[0] |> snd |> Array.length

let sorted = 
    [0..numperclustering..(Array.length clus)-1]
    |> Seq.map (fun offset -> async {return sortSpikes offset numperclustering (spike_max_indx,0) clus spikes})
    |> Async.Parallel
    |> Async.RunSynchronously

//sorted |> Array.zip [|0..699|] |> Array.filter (snd >> Option.isNone) |> Array.iter (printfn "%A");;

let labels = label numperclustering sorted clus (scaled_spikes |> Array.length)
let allspikes = Array.zip labels scaled_spikes
let getspikeclass i = allspikes|>Array.filter (fst>>((=)i))|>Array.map snd

let wnd = new D3DPlot2DWindow()
wnd.Show()
let (tmax,_) = scaled_spikes.[scaled_spikes.Length-1]
let (tmin,_) = scaled_spikes.[0]
let xrange = Range(-5.0+tmin,5.0+tmax)
addTimeXAxis wnd xrange |> ignore
let cols = [|Colors.Green;Colors.Blue;Colors.Red|]

plotSpikeAmps 3.0f xrange (Range(0.0,800.0e-6)) spike_max_indx wnd Colors.Black (sprintf "Ch %d (%sV)" 7) 
              (cols |> Seq.mapi (fun i col -> (col,(getspikeclass i))) |> Seq.toList)
                (fun r -> 
                    let inside = getSpikesInWindow spike_max_indx r.Left r.Right r.Top r.Bottom scaled_spikes
                    let nspk = Array.length inside
                    if (nspk > 0) then
                        plotlines (sprintf "Numspikes = %d" nspk) (Range(-0.5,((float spikelen)-0.5)))
                                  (inside |> Array.map (fun i -> i, cols.[labels.[i]]))  
                                  (scaled_spikes)
                )



let getclusoffset t = 
    let spikenumtofind = scaled_spikes |> Array.findIndex (fun (st,s) -> st > t*3600.0)
    let clusindx = clus |> Array.findIndex (fun (spikenum,_) -> spikenum > spikenumtofind)
    1000*(clusindx/1000)

let maxtemp =  0.2
let mintemp = 0.01
let tempstep = 0.01
let offset = 554000
let numspikes = 1000
for tempnum in 1..10 do
    let spikecols = 
        clus.[offset..offset+numspikes-1]
        |> Array.map (fun  (spikenum,clu) ->
                        let col = match (clu.[tempnum]) with
                                    | 0us -> Colors.Black
                                    | 1us -> Colors.Blue
                                    | 2us -> Colors.Red
                                    | 3us -> Colors.Green
                                    | _ -> Colors.Gray
                        (spikenum,col))

    plotlines (sprintf "Temperature %f" (mintemp + (float tempnum)*tempstep)) (Range(-0.5,((float spikelen)-0.5)))
              spikecols (scaled_spikes)


