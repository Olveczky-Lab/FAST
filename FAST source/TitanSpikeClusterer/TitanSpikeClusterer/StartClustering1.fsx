#r "System.Xml.Linq"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\FsPickler\bin\Release\FsPickler.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeClusterer\TitanSpikeClusterer\bin\Release\TitanSpikeClusterer.exe"

open System.Xml.Linq
open System.IO
open Helper
open Nessos.FsPickler
open System.Text.RegularExpressions
open System
open ClusterHelper

let xn = XName.op_Implicit

let cmdpath = @"& '\\140.247.178.88\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeClusterer\TitanSpikeClusterer"

//let intdatahostip = "192.168.0.100:5900"
let intdatahostip = "140.247.178.16:5900"
let extdatahost = Remote "140.247.178.94:5900"
let userpathlocal = @"Y:"
let userpathremote = @"/root/data/steffenwolff"
let rat = "RAT16"
let minclussize = 15

let fnumsall () = 
    Directory.GetDirectories(sprintf "%s\%s" userpathlocal rat)
    |>Array.filter (fun x -> Regex.IsMatch(x,@"\\[0-9]+$"))
    |>Array.map (fun x -> DirectoryInfo(x).Name)
    |> Array.sort

let fnums =
  [|"635586693150789656"|]

for fnum in fnums do
    let esets = loadnTrodes (XElement.Load(sprintf @"%s\%s\%s\SnippeterSettings.xml" userpathlocal rat fnum).Element(xn "EIBElectrodePlacement"))
    let createDirs () =
        printfn "%s" fnum
        Directory.CreateDirectory(sprintf @"%s\%s\%s\Behavior" userpathlocal rat fnum) |> ignore
        for chgroup,_ in esets do
            Directory.CreateDirectory(sprintf @"%s\%s\%s\ChGroup_%s\Level2" userpathlocal rat fnum chgroup) |> ignore
            Directory.CreateDirectory(sprintf @"%s\%s\%s\ChGroup_%s\AutoSort" userpathlocal rat fnum chgroup) |> ignore
            let serverf = sprintf @"%s/%s/%s/ChGroup_%s/%s" userpathremote rat fnum chgroup
            let numblocks = FileInfo(sprintf @"%s\%s\%s\ChGroup_%s\SpikeTimes" userpathlocal rat fnum chgroup).Length/8L/1000L|>uint64
            let numperwrite =100000000UL
            if numblocks > 0UL then
                for offset in [0UL..numperwrite..1000UL*numblocks-1UL] do
                    writeArray extdatahost (serverf "Clusters1.indx") offset [|offset..offset+(min numperwrite (1000UL*numblocks-offset))-1UL|] |> Async.RunSynchronously

    let dispstopcmd () =
        let cmd = sprintf @"%s\StopClustering.ps1'" cmdpath
        printfn "%s" cmd

    let dispcluscmd chgroup nchans i =
        let cmd = sprintf @"%s\StartClustering.ps1'" cmdpath
        let serverf = sprintf @"%s/%s/%s/ChGroup_%s/%s" userpathremote rat fnum chgroup
        let fname = sprintf @"%s\%s\%s\ChGroup_%s\Clusters%d.indx" userpathlocal rat fnum chgroup i
        if File.Exists(fname) then
            let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
            if numblocks > 0UL then
                printfn "%s %s %s %s 0 %d %d 0.01 0.15"  cmd intdatahostip (serverf "Spikes") (serverf (sprintf "Clusters%d" i)) (numblocks-1UL) nchans
                dispstopcmd ()
    
    let disptermcmd minclussize chgroup nchans i =
        let cmd = sprintf @"%s\StartTerminating.ps1'" cmdpath
        let serverf = sprintf @"%s/%s/%s/ChGroup_%s/%s" userpathremote rat fnum chgroup
        let fname = sprintf @"%s\%s\%s\ChGroup_%s\Clusters%d.indx" userpathlocal rat fnum chgroup i
        if File.Exists(fname) then
            let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
            if numblocks > 0UL then
                printfn "%s %s %d %s %s %s %s %d %d" cmd intdatahostip minclussize (serverf "Spikes") (serverf "SpikeTimes") 
                    (serverf (sprintf "Clusters%d" i)) (serverf (sprintf "Clusters%d" (i+1))) 
                    numblocks nchans
                dispstopcmd ()

    let displevel2cmd chgroup nchans =
        let nspikes = FileInfo(sprintf @"%s\%s\%s\ChGroup_%s\SpikeTimes" userpathlocal rat fnum chgroup).Length/8L|>uint64
        let cmd = sprintf @"C:\Titanic\Clusterer\TitanSpikeClusterer.exe"
        let serverf = sprintf @"%s/%s/%s/ChGroup_%s" userpathremote rat fnum chgroup
        let nlevels = 
            Directory.GetFiles(sprintf @"%s\%s\%s\ChGroup_%s" userpathlocal rat fnum chgroup,"Clusters*.large")
            |> Array.length
        if nlevels > 0 then
            printfn "%s Level2Worker %s %s %s 0 %d %d %d" cmd intdatahostip (sprintf "%s/Spikes" serverf) serverf nspikes nchans nlevels

    let writeclus2indx chgroup =
        let localf = sprintf @"%s\%s\%s\ChGroup_%s\Level2\%s" userpathlocal rat fnum chgroup
        let fname = localf "SpikeTimes"
        if File.Exists(fname) then
            let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
            if numblocks > 0UL then
                let numperwrite =100000000UL
                for offset in [0UL..numperwrite..1000UL*numblocks-1UL] do
                    writeArray Local (localf "Clusters.indx") offset [|offset..offset+(min numperwrite (1000UL*numblocks-offset))-1UL|] |> Async.RunSynchronously

    let dispclus2cmd chgroup nchans =
        let cmd = sprintf @"%s\StartClustering.ps1'" cmdpath
        let serverf = sprintf @"%s/%s/%s/ChGroup_%s/Level2/%s" userpathremote rat fnum chgroup
        let fname = sprintf @"%s\%s\%s\ChGroup_%s\Level2\Clusters.indx" userpathlocal rat fnum chgroup
        if File.Exists(fname) then
            let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
            printfn "%s %s %s %s 0 %d %d 0.01 0.1"  cmd intdatahostip (serverf "Spikes") (serverf "Clusters") (numblocks-1UL) nchans

    let computeoffslevel2 chgroup = 
        let fsp = new FsPickler()
        let fname = sprintf @"%s\%s\%s\ChGroup_%s\Level2\ClusterSpikeNums" userpathlocal rat fnum chgroup
        if File.Exists(fname) then
            use ms = new MemoryStream(getBytes Local fname 0UL -1|>Async.RunSynchronously)
            Seq.unfold (fun _ -> 
                if ms.Position >= ms.Length then None else
                fsp.Deserialize<uint64[]>(ms)|>ignore
                (ms.Position|>uint64,())|>Some) ()
            |> Seq.toArray
            |> writeArray Local (sprintf "%s.off" fname) 0UL |> Async.RunSynchronously
    
    let dispttl () =
        let fname = sprintf @"%s\%s\%s\TTLIns" userpathlocal rat fnum
        let numsamples = FileInfo(fname).Length/2L|>uint64
        let cmd = sprintf @"%s\StartTTL.ps1'" cmdpath
        Directory.CreateDirectory(sprintf @"%s\%s\%s\TTLChanges" userpathlocal rat fnum) |> ignore
        printfn "%s %s %s %d" cmd intdatahostip (sprintf @"%s/%s/%s" userpathremote rat fnum) numsamples
    
    //dispttl ()    
    createDirs ()
    for chgroup,chans in esets do
        let nchans = Array.length chans
//        dispcluscmd chgroup nchans 1
//        disptermcmd 15 chgroup nchans 1
        //displevel2cmd chgroup nchans
        printfn "%A" (fnum,chgroup)
//        writeclus2indx chgroup
        //dispclus2cmd chgroup nchans
        computeoffslevel2 chgroup

let ntotalblocks = 
    fnums |> Array.map (fun fnum ->
        let esets = loadnTrodes (XElement.Load(sprintf @"%s\%s\%s\SnippeterSettings.xml" userpathlocal rat fnum).Element(xn "EIBElectrodePlacement"))
        esets |> Array.sumBy (fun (chgroup,_) ->
        let fname = sprintf @"%s\%s\%s\ChGroup_%s\Clusters1.indx" userpathlocal rat fnum chgroup
        if File.Exists(fname) then FileInfo(fname).Length/8L/1000L|>uint64 else 0UL
        )
    )