#if COMPILED
module ClusterViewer
#else

#load "Helper.fs"
#load "HDF5Helper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"
#load "Cluster.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open RP.Controls
open System.Windows.Media
open System.Windows
open RP.HDF5.H5TypeProviderRuntime
open Helper
open PlotHelper
open ClusterHelper
open Cluster
open HDF5Helper

let f0 = 635025802044762119L
let fname = (sprintf "%s\%d.h5" @"C:\EphysData\Sullivan\Ashesh\Bairagi" f0)
let chgroup,chnum = "26",0
let exptid,smid = 2,62
let startindx,numperclustering,numclusterwindows = 216200,200,158

//open RP.HDF5
//type h5file = HDF5File<fname>

let mintemp =  0.01
let tempstep = 0.01
let maxtemp =  0.2
let spikes,clus,spikesraw = getSpikesAndClusters fname chgroup (startindx|>uint64) (numperclustering*numclusterwindows)
let scaled_spikes = spikes 
                    |> Array.map 
                        (fun (st,s) -> 
                            scalespike (st, Array.init (Array2D.length2 s) 
                                            (fun i -> Array.init (Array2D.length1 s) (fun j -> s.[j,i]))
                                            |> Array.concat))


let spike_max_indx = 31

open System.IO

let mutable (mnewclus:uint16[][]) = [||]
let mutable (moldclus:uint16[][]) = [||]
let curclus cluswindowindx = clus.[cluswindowindx*numperclustering..(cluswindowindx+1)*numperclustering-1]

let sortSpikes cluswindowindx =
    let c = curclus cluswindowindx
    let getAvgAmp tempnum clusnum =
        c
        |> Array.choose (fun (spikenum,c) -> 
            if (tempnum = -1 || c.[tempnum] = clusnum) then
                Some ((spikes.[spikenum]|>snd).[31,chnum]|>float|>abs)
            else None)
        |> fun x -> (if Array.length x < 4 then 0.0 else x |> Array.average)
    let stable =
        getStableClusters 20 c
        |> Seq.groupBy fst |> Seq.map (fun (key,vals) -> key,vals|>Seq.map snd)
        |> Seq.sortBy fst
        |> Seq.tryPick (fun (tempnum,xs) -> 
            match xs |> Seq.toList with
            | ((clu1,cnt1)::_)::((clu2,cnt2)::_)::_ as xs when cnt1 > 8 && cnt2 > 8 -> 
                let (clunum,stability) = 
                    xs
                    |> List.map (fun ys ->
                            let (clunum,_) = List.head ys
                            (clunum,List.length ys),getAvgAmp tempnum clunum)
                    |>List.maxBy snd
                    |>fst
                if (stability > 2) then Some (tempnum,clunum) else None
            | _ -> None
        )
    match stable with
    | Some x -> x
    | None -> (0,0us)
    |> fun x -> [x]

let sorting () = 
    let sortingfname = sprintf "%s.%d.%d.%s.sorting" fname exptid smid chgroup
    if (File.Exists(sortingfname)) then
        loadSorting sortingfname
    else
        let s = 
            [|0..numclusterwindows-1|]
            |> Array.Parallel.map (fun i -> sortSpikes i)
        saveSorting sortingfname s
        s        

let clusterwindow cluswindowindx =
    let curclus = curclus cluswindowindx
    let curspikes = curclus |> Array.map fst
    let waveletfeatures = getFeatures spikesraw curspikes [|getwavelets [|0;1|] 64|]
                          |> cullfeaturesks 20
    let ampfeatures = getFeatures spikesraw curspikes [|getspikeamp [|(31,0);(31,1)|]>>(Array.map (fun x -> x*2.0))|]
    let features = Array.zip waveletfeatures ampfeatures |> Array.map (fun (x,y) -> Array.append x y)
    let newclus = wave_clus mintemp (maxtemp+tempstep/2.0) tempstep 300 11 0 @"C:\EphysData\Sullivan\Ashesh\temp\temp" features
    mnewclus <- newclus
    moldclus <- curclus |> Array.map snd
    plotClusters (Array.zip curspikes newclus)
                    mintemp maxtemp tempstep 
                    scaled_spikes

let wnd = new D3DPlot2DWindow()
wnd.Show()
let (tmax,_) = scaled_spikes.[scaled_spikes.Length-1]
let (tmin,_) = scaled_spikes.[0]
let xrange = Range(-5.0+tmin,5.0+tmax)
let yrange = Range(-1000.0e-6,0.0)
addTimeXAxis wnd xrange |> ignore
addSelRect (wnd.Plot) xrange yrange 
           (Array.init numclusterwindows (fun i -> 
                                            (scaled_spikes.[clus.[i*numperclustering]|>fst]|>fst,
                                             scaled_spikes.[clus.[(i+1)*numperclustering-1]|>fst]|>fst))) 
           (2000.0e-6,-2000e-6)
           (fun cluswindowindx -> 
                        plotClusters (curclus cluswindowindx) mintemp maxtemp tempstep 
                                     scaled_spikes)
           (fun cluswindowindx -> 
                printfn "%d" cluswindowindx)
                //clusterwindow cluswindowindx)

let spikestoplot =
    let labels = label numperclustering (sorting ()) clus (Array.length scaled_spikes)
    let cols =
        Array.zip labels scaled_spikes
        |> Array.map (fun (label,spike) -> 
            let col = 
                match label with
                | GoodCluster -> Colors.Blue
                | BadCluster -> Colors.Red
                | _ -> Colors.Green
            (col, spike))
    let filtclus col = col,cols |> Array.filter (fun (c,_) -> col = c) |> Array.map snd
    [filtclus Colors.Blue;filtclus Colors.Red]

//let spikestoplot =
//    let good = clus |> Array.map (fun (spikenum,_) -> scaled_spikes.[spikenum])
//    [Colors.Blue,good]

plotSpikeAmps 3.0f xrange yrange (spike_max_indx+64*chnum) wnd Colors.Black (sprintf "Chgroup %s (%sV)" chgroup) spikestoplot
                (fun r -> ())

#if COMPILED
[<System.STAThread()>]
(new Application()).Run() |> ignore
#endif
