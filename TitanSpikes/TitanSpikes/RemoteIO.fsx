#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\FsPickler\bin\Release\FsPickler.dll"

open Helper
open System.IO
open Nessos.FsPickler
let hostname = Remote "140.247.178.16:8080"
let fname = sprintf @"/root/data/rpoddar/badlands/635276396558304920/ChGroup_0/Clusters1.large"
let clusterspikenums = 
    let bytes = getBytes hostname fname 0UL -1|>Async.RunSynchronously
    use ms = new MemoryStream(bytes)
    printfn "%d" ms.Length
    let fsp = new FsPickler()
    Seq.unfold (fun _ -> 
        if ms.Position >= ms.Length then None else
        (fsp.Deserialize<uint64[]>(ms),())|>Some) ()
    |> Seq.toList
