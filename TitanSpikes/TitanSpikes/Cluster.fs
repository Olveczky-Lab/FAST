module Cluster

open IntelIPP 
open Helper        
open System.IO
open System.Diagnostics
open System.Reflection


let rec wavedec x n = 
    let ny = 
        let nx = Array.length x
        if ((nx % 2) = 0) then nx/2 else (nx+1)/2
    let yl = Array.zeroCreate ny
    let yh = Array.zeroCreate ny
    WaveletTransform.WTHaarFwd(x,yl,yh)
    if (n = 1) then
        [yh;yl]
    else
        yh::(wavedec yl (n-1))

let maxks xs = 
    let m = SpecialFunctions.Mean(xs)
    let std = SpecialFunctions.StdDev(xs)
    let xs_central = xs |> Array.filter (fun x -> x < m+3.0*std && x > m-3.0*std)
    let nx = Array.length xs_central
    let ys = Array.zeroCreate nx
    IPPS.sort(xs_central)
    SpecialFunctions.ErfC(xs_central|>Array.map (fun x -> -(x-m)/((sqrt 2.0)*std)),ys)
    ys |> Array.mapi (fun i y -> 
                            let d1 = abs (0.5*y - (float i)/(float nx))
                            let d2 = abs (0.5*y - (float i+1.0)/(float nx))
                            max d1 d2)
       |> Array.max

let getFeatures (spikes:int16[,,]) spikestocluster featurefuncs =
    let numspikes = Array.length spikestocluster
    let numchannels = Array3D.length3 spikes
    Array.init numspikes
                (fun spikenum -> 
                    featurefuncs 
                    |> Array.map (fun f -> 
                                    let spike = fun i j -> spikes.[spikestocluster.[spikenum],i,j]
                                    f spike)
                    |> Array.concat
                )

let cullfeaturesks numfeatures features =
    let numcoeffs = (Array.get features 0) |> Array.length
    let numspikes = Array.length features
    let ks = 
        Array.init numcoeffs (fun i -> i, Array.init numspikes (fun j -> features.[j].[i]) |> maxks)
        |> Array.sortBy snd
        |> Array.rev
        |> fun xs -> 
            xs.[0..numfeatures-1]
        |> Array.map fst
    Array.init numspikes (fun i -> Array.init numfeatures (fun j -> features.[i].[ks.[j]]))

let inline getwavelets channelstouse spikelen spike =
    channelstouse |> Array.map
                (fun chi ->
                    wavedec (Array.init spikelen (fun j -> spike j chi|>float)) 4
                    |> List.toArray |> Array.concat)
    |> Array.concat
    
let inline getspikeamp pos spike = pos |> Array.map (fun (spike_max_indx,chnum) -> spike spike_max_indx chnum |> float)
let inline getspikeenergy spikelen numchs spike = 
    seq {for i in 0..spikelen-1 do for j in 0..numchs-1 -> spike i j|>float} 
    |> Seq.map (fun x->x*x) |>Seq.sum |> (fun x -> [|x/((spikelen*numchs)|>float)|>sqrt|])
        

let wave_clus mintemp maxtemp tempstep swcycles knn seed tmpfname features =
    let dim = (Array.get features 0)|>Seq.length
    let numspikes = Array.length features
    let fi = FileInfo(tmpfname)
    let deleteTempFiles () = 
        Directory.GetFiles(fi.DirectoryName,fi.Name+"*") 
        |> Array.iter (fun f -> File.Delete(f))
    let runcluster data =
        deleteTempFiles()
        try
            let file = new StreamWriter(new FileStream(sprintf "%s.run" tmpfname,FileMode.Create,FileAccess.Write))
            file.WriteLine(sprintf "NumberOfPoints: %d" numspikes)
            file.WriteLine(sprintf "DataFile: %s" tmpfname)
            file.WriteLine(sprintf "OutFile: %s" tmpfname)
            file.WriteLine(sprintf "Dimensions: %d" dim)
            file.WriteLine(sprintf "MinTemp: %f" mintemp)
            file.WriteLine(sprintf "MaxTemp: %f" maxtemp)
            file.WriteLine(sprintf "TempStep: %f" tempstep)
            file.WriteLine(sprintf "SWCycles: %d" swcycles)
            file.WriteLine(sprintf "KNearestNeighbours: %d" knn)
            file.WriteLine("MSTree|")
            file.WriteLine("DirectedGrowth|")
            file.WriteLine("SaveSuscept|")
            file.WriteLine("WriteLabels|")
            if (seed <> 0) then
                file.WriteLine(sprintf "ForceRandomSeed: %d" seed)
            file.Close()
            let datafile = new StreamWriter(new FileStream(tmpfname,FileMode.Create,FileAccess.Write))
            for i in 0..numspikes-1 do
                features.[i]
                |> Seq.iter (fun (x:float) ->
                                datafile.Write(x)
                                datafile.Write(" "))
                datafile.WriteLine()
            datafile.Close()
            let clusterer = @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\Cluster.exe"
            let si = ProcessStartInfo(clusterer,
                                      (sprintf "%s.run" tmpfname),
                                      UseShellExecute=false,
                                      CreateNoWindow=true)        
            Process.Start(si).WaitForExit()

            let clu = 
                let lines =
                    File.ReadAllLines(sprintf "%s.dg_01.lab" tmpfname)
                    |> Array.map (fun s -> let fields = 
                                                s.Split()
                                                |> Array.filter ((<>)"")
                                           fields.[2..])
                Array.init numspikes 
                            (fun i -> 
                            Array.init (Array.length lines) 
                                       (fun temp -> uint16 lines.[temp].[i]))
        
            clu
        finally
            deleteTempFiles()

    features |> runcluster    
    