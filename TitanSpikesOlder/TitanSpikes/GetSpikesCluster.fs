#if COMPILED
module GetSpikesCluster
#else

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.IO.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.FSharp.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\IntelIPP.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"

#load "Helper.fs"
#load "AmpDataHelper.fs"
#load "ClusterPicker.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open ClusterPicker
open RP.HDF5.H5TypeProviderRuntime
open RP.HDF5

let fname = @"C:\EphysData\Sullivan\Ashesh\635025802044762119.h5"
let chgroup = "26"

let spike_max_indx = 31
let numperclustering = 1000

let getRealSpikes offset numclustered =
    let spikes,clus = getSpikesAndClusters fname chgroup offset numclustered
    let sorted = 
        [0..numperclustering..(Array.length clus)-1]
        |> Seq.map (fun o -> async {return sortSpikes o numperclustering (spike_max_indx,0) clus spikes})
        |> Async.Parallel
        |> Async.RunSynchronously

    let labels = label numperclustering sorted clus (spikes |> Array.length)
    let allspikes = Array.zip labels spikes
    allspikes|>Array.filter (fst>>((=)1))|>Array.map snd

let realspikes = 
    let numtotal = 4755000UL
    [|0UL..500000UL..numtotal-1UL|]
    |> Array.map (fun offset -> 
                    printfn "Extracting Block %d" offset
                    getRealSpikes offset (min 500000 ((numtotal-offset)|>int)))
    |> Array.concat

let numspikes = Array.length realspikes
let numsamplesperspike = Array2D.length1 (realspikes.[0]|>snd)
let numchannelsperspike = Array2D.length2 (realspikes.[0]|>snd)

[<Literal>]
let outputfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\Processed.h5"
type h5file = HDF5File<outputfname>
h5file.SpikeTimes.WriteData(null,null,realspikes|>Array.map fst)
h5file.Spikes.WriteData(null,null,Array3D.init numspikes numsamplesperspike numchannelsperspike (fun i j k -> (realspikes.[i]|>snd).[j,k]))

