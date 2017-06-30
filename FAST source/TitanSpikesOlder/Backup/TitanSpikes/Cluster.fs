module Cluster

open IntelIPP 
open Helper        
open System.IO
open System.Diagnostics


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

let wave_clus dim mintemp maxtemp tempstep swcycles knn seed tmpfname (spikes:int16[,,]) spikestocluster channelstouse =
    let numspikes = Array.length spikestocluster
    let spikelen = Array3D.length2 spikes
    let numchannels = Array3D.length3 spikes
    let runcluster (data:float[][]) =
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
            for j in 0..dim-1 do
                datafile.Write(data.[i].[j])
                datafile.Write(" ")
            datafile.WriteLine()
        datafile.Close()
        let si = ProcessStartInfo(@"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\Cluster.exe",
                                  (sprintf "%s.run" tmpfname),
                                  UseShellExecute=false,
                                  CreateNoWindow=true)        
        Process.Start(si).WaitForExit()
        let clu = 
            File.ReadAllLines(sprintf "%s.dg_01.lab" tmpfname)
            |> Array.map (fun s -> let fields = 
                                        s.Split()
                                        |> Array.filter ((<>)"")
                                   fields.[2..]
                                   |> Array.map uint16)
        let fi = FileInfo(tmpfname)
        Directory.GetFiles(fi.DirectoryName,fi.Name+"*") 
        |> Array.iter (fun f -> File.Delete(f))
        clu

    let wavelet_coeffs = 
        Array.init numspikes
                   (fun spikenum -> 
                    Array.init (Array.length channelstouse)
                               (fun chi ->
                                    wavedec (Array.init spikelen (fun j -> spikes.[spikestocluster.[spikenum],j,channelstouse.[chi]]|>float)) 6
                                    |> List.toArray |> Array.concat)
                    |> Array.concat)
    let numcoeffs = wavelet_coeffs.[0] |> Array.length
    let ks = 
        Array.init numcoeffs (fun i -> i, Array.init numspikes (fun j -> wavelet_coeffs.[j].[i]) |> maxks)
        |> Array.sortBy snd
    let data = Array.init numspikes (fun i -> Array.init dim (fun j -> wavelet_coeffs.[i].[ks.[numcoeffs-1-j]|>fst]))
    data |> runcluster    
    
               