#if COMPILED
module AutoSorter
#else
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open Helper
open System.IO
open System.Xml.Linq
open Nessos.FsPickler
open AutoSortHelper
open ClusterHelper
open Cluster
open ClusterTreeHelper

System.Environment.CurrentDirectory <- @"D:\Rajesh\Ephys\Data Analysis\FSharp\ZMQRPC\ZMQRPC"

let hostname = Remote "140.247.178.94:5900"
let ratpath = @"/root/data/asheshdhawale/Data/Dhanashri"
let localratpath = @"Z:\Data\Dhanashri"

let deleteautosorts fnums =
    for fnum in fnums do
        printfn "%s" fnum
        let fpath =  sprintf @"%s/%s/%s" ratpath fnum
        let localpath = sprintf @"%s\%s" localratpath fnum

        let esets =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
            loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
        for chgroup,_ in esets do 
            let dir = sprintf @"%s\ChGroup_%s\AutoSort" localpath chgroup
            if Directory.Exists(dir) then Directory.Delete(dir,true)
            let dir = sprintf "%s\ChGroup_%s\MergedChains" localpath chgroup
            if Directory.Exists(dir) then Directory.Delete(dir,true)
            Directory.CreateDirectory(sprintf @"%s\ChGroup_%s\AutoSort" localpath chgroup) |> ignore
        let file = sprintf @"%s\AutoSort.meta" localpath
        if File.Exists(file) then File.Delete(file)
        let file = sprintf @"%s\MergedChains.meta" localpath
        if File.Exists(file) then File.Delete(file)

let savesortmeta chgroups localpath dir =
    let x = 
        chgroups |> Array.map (fun chgroup ->
            printfn "Doing %s" chgroup
            Directory.GetFiles(sprintf @"%s\ChGroup_%s\%s" localpath chgroup dir,"*.stimes")
            |> Array.map (fun fname -> 
                let ntotalspikes = (getSize Local fname |> Async.RunSynchronously)/8L|>uint64
                (FileInfo(fname).Name |> fun x -> x.[0..Seq.length x-1-Seq.length ".stimes"]),
                (getSpikeTimes Local fname 0UL 1 |> Seq.head,
                    getSpikeTimes Local fname (ntotalspikes-1UL) 1 |> Seq.head)))
    let fsp = new FsPickler()
    use ms = new MemoryStream()
    fsp.Serialize(ms,Array.zip chgroups x)
    let bytes = ms.ToArray()
    writeArray Local (sprintf @"%s\%s.meta" localpath dir) 0UL bytes |> Async.RunSynchronously        

let saveautosorts fnums = 
    for (fnum,chgroups) in fnums do
        printfn "%s" fnum
        let fpath =  sprintf @"%s/%s/%s" ratpath fnum
        let localpath = sprintf @"%s\%s" localratpath fnum
        let esets =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
            loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
        let chgroups = 
            if Array.length chgroups = 0 then esets |> Array.map fst else chgroups
        let spikelen = 64

        let savesort chgroup = 
            printfn "Doing %s" chgroup
            let nchans = esets|>Array.find (fun (x,_) -> x = chgroup)|>snd|>Array.length
            let numperblock = 1000
            let fname x = fpath (sprintf @"ChGroup_%s/%s" chgroup x)
            let l2sfname,l2stfname,(l2clusfname,l2clustemp),l2snumfname,outpath,stfname = 
                fname "Level2/Spikes", fname "Level2/SpikeTimes", (fname "Level2/Clusters",10),(fname @"Level2/ClusterSpikeNums"),
                (fname @"AutoSort"),(fname @"SpikeTimes")
            if File.Exists(sprintf @"%s\ChGroup_%s\Level2\Clusters" localpath chgroup) then
                let ntotalspikes = (getSize hostname (sprintf "%s.indx" l2clusfname) |> Async.RunSynchronously)/8L|>int    
                let firstblock,numblocks = 0,ntotalspikes/numperblock
                printfn "%d" numblocks
                if numblocks > 0 then
                    let maxtempnum = 7
                    calcAndSaveChainSort hostname l2sfname l2stfname l2snumfname stfname outpath ""
                        spikelen nchans l2clusfname l2clustemp maxtempnum numperblock firstblock numblocks
            else
                printfn "0"
    
        for chgroup in chgroups do
            savesort chgroup
        savesortmeta chgroups localpath "AutoSort"

let saveautosortmeta fnums = 
    for (fnum,chgroups) in fnums do
        printfn "%s" fnum
        let localpath = sprintf @"%s\%s" localratpath fnum
        savesortmeta chgroups localpath "AutoSort"

let saveautosortstimes fnums =
    fnums |> Array.iter (fun (fnum,chgroups) ->   
        printfn "%s" fnum
        let path =  sprintf @"%s/%s" ratpath fnum
        let fpath =  sprintf @"%s/%s" path
        let autosorts = 
            let fsp = new FsPickler()
            use ms = new MemoryStream(getArray<byte> hostname (fpath "AutoSort.meta") 0UL -1)
            fsp.Deserialize<(string*((string*(uint64*uint64))[]))[]>(ms)
        let esets =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
            loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
        let chgroups = 
            if Array.length chgroups = 0 then esets |> Array.map fst else chgroups
        chgroups |> Array.iter (fun chgroup ->
            printfn "%s" chgroup
            let f chain x = fpath (sprintf "ChGroup_%s/AutoSort/%s.%s" chgroup chain x)
            let chains = autosorts |> Array.find(fst>>(=)chgroup) |> snd |> Array.map fst
            let l2spikenumss = 
                 chains |> Array.map (fun chain ->
                    getSpikeTimes hostname (f chain "l2snums") 0UL -1
                )
            let l1spikenumss = 
                getArraySubsets<uint64[]> hostname (fpath (sprintf "ChGroup_%s/Level2/ClusterSpikeNums" chgroup)) l2spikenumss
                |> Array.map (Array.concat>>Array.sort)
            let l1spiketimess =
                getSpikeTimeSubsets hostname (fpath (sprintf "ChGroup_%s/SpikeTimes" chgroup)) l1spikenumss
            Array.zip3 chains l1spikenumss l1spiketimess |> Array.iter (fun (chain,xs,ys) ->
                writeArray hostname (f chain "snums") 0UL xs |> Async.RunSynchronously
                writeArray hostname (f chain "stimes") 0UL ys |> Async.RunSynchronously
            )
        )
    )

let allmergers fnums =
    fnums |> Array.iter (fun (fnum,chgroups) ->   
        printfn "%s" fnum
        let path =  sprintf @"%s/%s" ratpath fnum
        let fpath =  sprintf @"%s/%s" path
        let esets =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
            loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
        let chgroups = 
            if Array.length chgroups = 0 then esets |> Array.map fst else chgroups
        let m = 
            fnum,
            chgroups |> Array.map (fun chgroup ->
                let nchans = esets|>Array.find (fun (x,_) -> x = chgroup)|>snd|>Array.length
                printfn "%s" chgroup
                let mergers = 
                    mergeChainsL2 0.02 10 hostname path chgroup (nchans)
                    |> Map.toArray |> Array.map (snd>>fst)
                Directory.CreateDirectory(sprintf @"%s\%s\ChGroup_%s\MergedChains" localratpath fnum chgroup) |> ignore
                printfn "Number of units : %d" (Array.length mergers)
                if Array.length mergers > 0 then
                    let f chain x = fpath (sprintf "ChGroup_%s/AutoSort/%s.%s" chgroup chain x)
                    let g i x = fpath (sprintf "ChGroup_%s/MergedChains/%d.%s" chgroup i x)
                    mergers |> Array.iteri (fun i (Node((s,_),_)) -> 
                        s |> Set.toArray |> Array.map (fun chain -> 
                            getSpikeTimes hostname (f chain "l2snums") 0UL -1,
                            Array.zip
                                (getSpikeTimes hostname (f chain "snums") 0UL -1)
                                (getSpikeTimes hostname (f chain "stimes") 0UL -1))
                        |> fun xs -> 
                            xs |> Array.map snd |> Array.concat |> Array.sortBy fst, 
                            xs |> Array.map fst |> Array.concat |> Array.sort
                        |> fun (xs,ys) -> 
                            writeArray hostname (g i "snums") 0UL (xs |> Array.map fst) |> Async.RunSynchronously
                            writeArray hostname (g i "stimes") 0UL (xs |> Array.map snd) |> Async.RunSynchronously
                            writeArray hostname (g i "l2snums") 0UL ys |> Async.RunSynchronously
                    )
                chgroup,mergers
            )
        let localpath = sprintf @"%s\%s" localratpath fnum
        savesortmeta chgroups localpath "MergedChains"
        use fs = new FileStream(sprintf @"%s\allmergers" localpath,FileMode.Create)
        FsPickler().Serialize(fs,m)
    )

let savel2stimes fnums =
    fnums |> Array.iter (fun (fnum,chgroups) ->   
        printfn "%s" fnum
        let path =  sprintf @"%s/%s" ratpath fnum
        let fpath =  sprintf @"%s/%s" path
        let esets =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
            loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
        let chgroups = 
            if Array.length chgroups = 0 then esets |> Array.map fst else chgroups
        chgroups |> Array.iter (fun chgroup ->
            printfn "%s" chgroup
            if File.Exists(sprintf "%s\%s\ChGroup_%s\Level2\SpikeTimes" localratpath fnum chgroup) then
                let l1snums = getSpikeTimes hostname (fpath (sprintf @"ChGroup_%s/Level2/SpikeTimes" chgroup)) 0UL -1
                let sts = getSpikeTimeSubsets hostname (fpath (sprintf @"ChGroup_%s/SpikeTimes" chgroup)) [|l1snums|]
                writeArray hostname (fpath (sprintf @"ChGroup_%s/Level2/SpikeTimesRaw" chgroup)) 0UL
                    sts |> Async.RunSynchronously
        )
    )

let savemergerdetails fnums =
    fnums |> Array.iter (fun fnum ->   
        printfn "%s" fnum
        let fname = sprintf @"%s\%s\allmergers" localratpath fnum
        if File.Exists(fname) then
            use ms = new MemoryStream(getArray<byte> Local fname 0UL -1)
            use fs = new FileStream(sprintf "%s.txt" fname,FileMode.Create)
            use sr = new StreamWriter(fs)
            FsPickler().Deserialize<string * (string * Tree<Set<string> * (int * float) option> []) []>(ms)
            |> snd |> Array.iter (fun (chgroup,xs) ->
                sr.WriteLine(sprintf @"ChGroup_%s" chgroup)
                xs|>Array.iter (fun (Node((a,_),_)) -> 
                    a |> Set.toArray |> String.concat " " |> sr.WriteLine)
                )
        )

let fnums = 
  [|"635400060049541397",[|"0"|]|]

//deleteautosorts fnums
//saveautosorts fnums
saveautosortmeta fnums //run this if autosorting is done using TitanSpikeCluster
saveautosortstimes fnums
allmergers fnums
savel2stimes fnums
savemergerdetails (fnums |> Array.map fst)