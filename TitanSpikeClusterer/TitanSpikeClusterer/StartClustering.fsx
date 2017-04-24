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
open ClusterTreeHelper
open AutoSortHelper

let xn = XName.op_Implicit

let cmdpath = @"& 'C:\Titanic\Clusterer"

//let intdatahostip = "192.168.0.100:5900"
let intdatahostip = "140.247.178.16:5900"
let extdatahostip = "140.247.178.16:5900"
let extdatahost = Remote extdatahostip
let userpathlocal = @"Y:\Data"
let userpathremote = @"/root/data/asheshdhawale/Data"
let rat = "Hindol"
let minclussize = 15
let numblocksperazuretask = 200
let cmdfile = @"D:\Rajesh\testcmds"
if File.Exists(cmdfile) then File.Delete(cmdfile)

let fnumsall () = 
    Directory.GetDirectories(sprintf "%s\%s" userpathlocal rat)
    |>Array.filter (fun x -> Regex.IsMatch(x,@"\\[0-9]+$"))
    |>Array.map (fun x -> DirectoryInfo(x).Name)
    |> Array.sort

let createDirs fnum =
    printfn "%s" fnum
    let esets = loadnTrodes (XElement.Load(sprintf @"%s\%s\%s\SnippeterSettings.xml" userpathlocal rat fnum).Element(xn "EIBElectrodePlacement"))
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

let dispcluscmd fnum chgroup nchans i =
    let cmd = sprintf @"%s\StartClustering.ps1'" cmdpath
    let serverf = sprintf @"%s/%s/%s/ChGroup_%s/%s" userpathremote rat fnum chgroup
    let fname = sprintf @"%s\%s\%s\ChGroup_%s\Clusters%d.indx" userpathlocal rat fnum chgroup i
    if File.Exists(fname) then
        let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
        if numblocks > 0UL then
            printfn "%s %s %s %s 0 %d %d 0.01 0.15"  cmd intdatahostip (serverf "Spikes") (serverf (sprintf "Clusters%d" i)) (numblocks-1UL) nchans
            dispstopcmd ()

let dispautosortcmd fnum chgroup nchans =
    let cmd = sprintf @"%s\StartAutoSorting.ps1'" cmdpath
    let serverf = sprintf @"%s/%s/%s/ChGroup_%s" userpathremote rat fnum chgroup
    let fname = sprintf @"%s\%s\%s\ChGroup_%s\Level2\Clusters.indx" userpathlocal rat fnum chgroup
    if File.Exists(fname) then
        let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
        if numblocks > 0UL then
            printfn "%s %s %s %s %d 10 7 %d"  cmd "AutoSort" intdatahostip serverf nchans numblocks
            dispstopcmd ()
            printfn "%s %s %s %s %d 10 7 %d"  cmd "AutoSortSaver" intdatahostip serverf nchans numblocks
            dispstopcmd ()
    
let savecluscmdazure fnum chgroup nchans i =
    let serverf = sprintf @"%s/%s/%s/ChGroup_%s/%s" userpathremote rat fnum chgroup
    let fname = sprintf @"%s\%s\%s\ChGroup_%s\Clusters%d.indx" userpathlocal rat fnum chgroup i
    if File.Exists(fname) then
        let numblocks = FileInfo(fname).Length/8L/1000L|>int
        if numblocks > 0 then
            let cmds = seq {
                for j in 0..numblocksperazuretask..numblocks-1 do
                yield sprintf "%s %s %d %s 0.01 0.15 %d %d"  extdatahostip (serverf "Spikes") nchans (serverf (sprintf "Clusters%d" i))
                    j (min numblocksperazuretask (numblocks-j))
            }
            File.AppendAllLines(cmdfile,cmds)
    
let disptermcmd fnum minclussize chgroup nchans i =
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

let displevel2cmd fnum chgroup nchans =
    let nspikes = FileInfo(sprintf @"%s\%s\%s\ChGroup_%s\SpikeTimes" userpathlocal rat fnum chgroup).Length/8L|>uint64
    let cmd = sprintf @"C:\Titanic\Clusterer\TitanSpikeClusterer.exe"
    let serverf = sprintf @"%s/%s/%s/ChGroup_%s" userpathremote rat fnum chgroup
    let nlevels = 
        Directory.GetFiles(sprintf @"%s\%s\%s\ChGroup_%s" userpathlocal rat fnum chgroup,"Clusters*.large")
        |> Array.length
    if nlevels > 0 then
        printfn "%s Level2Worker %s %s %s 0 %d %d %d" cmd intdatahostip (sprintf "%s/Spikes" serverf) serverf nspikes nchans nlevels

let writeclus2indx fnum chgroup =
    let localf = sprintf @"%s\%s\%s\ChGroup_%s\Level2\%s" userpathlocal rat fnum chgroup
    let fname = localf "SpikeTimes"
    if File.Exists(fname) then
        let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
        if numblocks > 0UL then
            let numperwrite =100000000UL
            for offset in [0UL..numperwrite..1000UL*numblocks-1UL] do
                writeArray Local (localf "Clusters.indx") offset [|offset..offset+(min numperwrite (1000UL*numblocks-offset))-1UL|] |> Async.RunSynchronously

let dispclus2cmd fnum chgroup nchans =
    let cmd = sprintf @"%s\StartClustering.ps1'" cmdpath
    let serverf = sprintf @"%s/%s/%s/ChGroup_%s/Level2/%s" userpathremote rat fnum chgroup
    let fname = sprintf @"%s\%s\%s\ChGroup_%s\Level2\Clusters.indx" userpathlocal rat fnum chgroup
    if File.Exists(fname) then
        let numblocks = FileInfo(fname).Length/8L/1000L|>uint64
        printfn "%s %s %s %s 0 %d %d 0.01 0.1"  cmd intdatahostip (serverf "Spikes") (serverf "Clusters") (numblocks-1UL) nchans

let saveclus2cmdazure fnum chgroup nchans =
    let serverf = sprintf @"%s/%s/%s/ChGroup_%s/%s" userpathremote rat fnum chgroup
    let fname = sprintf @"%s\%s\%s\ChGroup_%s\Level2\Clusters.indx" userpathlocal rat fnum chgroup
    if File.Exists(fname) then
        let numblocks = FileInfo(fname).Length/8L/1000L|>int
        if numblocks > 0 then
            let cmds = seq {
                for j in 0..numblocksperazuretask..numblocks-1 do
                yield sprintf "%s %s %d %s 0.01 0.1 %d %d"  extdatahostip (serverf "Spikes") nchans (serverf "Clusters")
                    j (min numblocksperazuretask (numblocks-j))
            }
            File.AppendAllLines(cmdfile,cmds)

let computeoffslevel2 fnum chgroup = 
    let fsp = new FsPickler()
    let fname = sprintf @"%s\%s\%s\ChGroup_%s\Level2\ClusterSpikeNums" userpathlocal rat fnum chgroup
    if File.Exists(fname) then
        use fs = new FileStream(fname,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
        Seq.unfold (fun offset -> 
            if fs.Position >= fs.Length then None else
            let x = fsp.Deserialize<uint64[]>(fs)
            writeArray Local (sprintf "%s.bin" fname) offset x |> Async.RunSynchronously
            let newoffset = offset+(Array.length x|>uint64)
            ((fs.Position|>uint64,newoffset),newoffset)|>Some) 0UL
        |> Seq.toArray
        |> Array.unzip
        |> fun (xs,ys) ->
            writeArray Local (sprintf "%s.off" fname) 0UL xs |> Async.RunSynchronously
            writeArray Local (sprintf "%s.bin.off" fname) 0UL ys |> Async.RunSynchronously
    
let dispttl fnum =
    let fname = sprintf @"%s\%s\%s\TTLIns" userpathlocal rat fnum
    let numsamples = FileInfo(fname).Length/2L|>uint64
    let cmd = sprintf @"%s\StartTTL.ps1'" cmdpath
    Directory.CreateDirectory(sprintf @"%s\%s\%s\TTLChanges" userpathlocal rat fnum) |> ignore
    printfn "%s %s %s %d" cmd intdatahostip (sprintf @"%s/%s/%s" userpathremote rat fnum) numsamples
    
let ntotalblocks fnums = 
    fnums |> Array.map (fun fnum ->
        let esets = loadnTrodes (XElement.Load(sprintf @"%s\%s\%s\SnippeterSettings.xml" userpathlocal rat fnum).Element(xn "EIBElectrodePlacement"))
        esets |> Array.sumBy (fun (chgroup,_) ->
        let fname = sprintf @"%s\%s\%s\ChGroup_%s\Clusters1.indx" userpathlocal rat fnum chgroup
        if File.Exists(fname) then FileInfo(fname).Length/8L/1000L|>uint64 else 0UL
        )
    )

let savesortmeta chgroups localpath dir =
    let x = 
        chgroups |> Array.map (fun (chgroup,_) ->
            printfn "Doing %s" chgroup
            Directory.GetFiles(sprintf @"%s\ChGroup_%s\%s" localpath chgroup dir,"*.stimes")
            |> Array.map (fun fname -> 
                let ntotalspikes = (getSize Local fname |> Async.RunSynchronously)/8L|>uint64
                (FileInfo(fname).Name |> fun x -> x.[0..Seq.length x-1-Seq.length ".stimes"]),
                (getSpikeTimes Local fname 0UL 1 |> Seq.head,
                    getSpikeTimes Local fname (ntotalspikes-1UL) 1 |> Seq.head)))
    let fsp = new FsPickler()
    use ms = new MemoryStream()
    fsp.Serialize(ms,Array.zip (chgroups|>Array.map fst) x)
    let bytes = ms.ToArray()
    writeArray Local (sprintf @"%s\%s.meta" localpath dir) 0UL bytes |> Async.RunSynchronously        

let localratpath = sprintf @"%s\%s" userpathlocal rat
let ratpath = sprintf @"%s/%s" userpathremote rat

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
        chgroups |> Array.iter (fun (chgroup,_) ->
            printfn "%s" chgroup
            let f chain x = fpath (sprintf "ChGroup_%s/AutoSort/%s.%s" chgroup chain x)
            let chains =
                Directory.GetFiles(sprintf @"%s\%s\ChGroup_%s\AutoSort" localratpath fnum chgroup,"*.l2snums")
                |> Array.map (fun fname -> 
                    let ntotalspikes = (getSize Local fname |> Async.RunSynchronously)/8L|>uint64
                    (FileInfo(fname).Name |> fun x -> x.[0..Seq.length x-1-Seq.length ".l2snums"])
                )
            let l2spikenumss = 
                 chains |> Array.map (fun chain ->
                    getSpikeTimes extdatahost (f chain "l2snums") 0UL -1
                )
            let l1spikenumss = 
                getArraySubsets<uint64[]> extdatahost (fpath (sprintf "ChGroup_%s/Level2/ClusterSpikeNums" chgroup)) l2spikenumss
                |> Array.map (Array.concat>>Array.sort)
            let l1spiketimess =
                getSpikeTimeSubsets extdatahost (fpath (sprintf "ChGroup_%s/SpikeTimes" chgroup)) l1spikenumss
            Array.zip3 chains l1spikenumss l1spiketimess |> Array.iter (fun (chain,xs,ys) ->
                writeArray extdatahost (f chain "snums") 0UL xs |> Async.RunSynchronously
                writeArray extdatahost (f chain "stimes") 0UL ys |> Async.RunSynchronously
            )
        )
    )

let allmergers maxd fnums =
    fnums |> Array.iter (fun (fnum,chgroups) ->   
        printfn "%s" fnum
        let path =  sprintf @"%s/%s" ratpath fnum
        let fpath =  sprintf @"%s/%s" path
        let m = 
            fnum,
            chgroups |> Array.map (fun (chgroup,nchans) ->
                printfn "%s" chgroup
                let mergers = 
                    mergeChainsL2 maxd 10 extdatahost path chgroup (nchans)
                    |> Map.toArray |> Array.map (snd>>fst)
                Directory.CreateDirectory(sprintf @"%s\%s\ChGroup_%s\MergedChains" localratpath fnum chgroup) |> ignore
                printfn "Number of units : %d" (Array.length mergers)
                if Array.length mergers > 0 then
                    let f chain x = fpath (sprintf "ChGroup_%s/AutoSort/%s.%s" chgroup chain x)
                    let g i x = fpath (sprintf "ChGroup_%s/MergedChains/%d.%s" chgroup i x)
                    mergers |> Array.iteri (fun i (Node((s,_),_)) -> 
                        s |> Set.toArray |> Array.map (fun chain -> 
                            getSpikeTimes extdatahost (f chain "l2snums") 0UL -1,
                            Array.zip
                                (getSpikeTimes extdatahost (f chain "snums") 0UL -1)
                                (getSpikeTimes extdatahost (f chain "stimes") 0UL -1))
                        |> fun xs -> 
                            xs |> Array.map snd |> Array.concat |> Array.sortBy fst, 
                            xs |> Array.map fst |> Array.concat |> Array.sort
                        |> fun (xs,ys) -> 
                            writeArray extdatahost (g i "snums") 0UL (xs |> Array.map fst) |> Async.RunSynchronously
                            writeArray extdatahost (g i "stimes") 0UL (xs |> Array.map snd) |> Async.RunSynchronously
                            writeArray extdatahost (g i "l2snums") 0UL ys |> Async.RunSynchronously
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
        chgroups |> Array.iter (fun (chgroup,_) ->
            printfn "%s" chgroup
            if File.Exists(sprintf "%s\%s\ChGroup_%s\Level2\SpikeTimes" localratpath fnum chgroup) then
                let l1snums = getSpikeTimes extdatahost (fpath (sprintf @"ChGroup_%s/Level2/SpikeTimes" chgroup)) 0UL -1
                let sts = getSpikeTimeSubsets extdatahost (fpath (sprintf @"ChGroup_%s/SpikeTimes" chgroup)) [|l1snums|]
                writeArray extdatahost (fpath (sprintf @"ChGroup_%s/Level2/SpikeTimesRaw" chgroup)) 0UL
                    sts.[0] |> Async.RunSynchronously
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

//For all channel groups
//let fnums = 
//  [|"635596782733480422"|]
//  |> Array.map (fun fnum ->
//    let esets = loadnTrodes (XElement.Load(sprintf @"%s\%s\%s\SnippeterSettings.xml" userpathlocal rat fnum).Element(xn "EIBElectrodePlacement"))
//    fnum,(esets|>Array.map (fun (chgroup,xs) -> chgroup,Array.length xs))
//  )

////For select channel groups
let fnums = 
  [|"635596782733480422",[|"0"|]|]
  |> Array.map (fun (fnum,chgroups) ->
    let esets = loadnTrodes (XElement.Load(sprintf @"%s\%s\%s\SnippeterSettings.xml" userpathlocal rat fnum).Element(xn "EIBElectrodePlacement"))
    fnum,chgroups|>Array.map (fun chgroup -> 
        let _,xs = esets |> Array.find (fst>>(=)chgroup)
        chgroup,Array.length xs
    )
  )

for (fnum,esets) in fnums do
    //dispttl ()    
    //createDirs ()
    for chgroup,nchans in esets do
        //savecluscmdazure fnum chgroup nchans 1
        //dispcluscmd fnum chgroup nchans 4
        //disptermcmd fnum 15 chgroup nchans 4
        //displevel2cmd fnum chgroup nchans
        //printfn "%A" (fnum,chgroup)
        //writeclus2indx fnum chgroup
        //saveclus2cmdazure fnum chgroup nchans
        //dispclus2cmd fnum chgroup nchans
        //computeoffslevel2 fnum chgroup
        dispautosortcmd fnum chgroup nchans

saveautosortstimes fnums
saveautosortmeta fnums
allmergers 0.02 fnums //0.02 is the threshold for merging chains. Increase it to result in fewer mergers. Reasonable range is 0.0 - 0.1
savel2stimes fnums
savemergerdetails (fnums |> Array.map fst)
