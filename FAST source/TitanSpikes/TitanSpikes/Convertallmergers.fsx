#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\FsPickler\bin\Release\FsPickler.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeClusterer\TitanSpikeClusterer\bin\Release\TitanSpikeClusterer.exe"

open Helper
open System.IO
open Nessos.FsPickler
open ClusterTreeHelper

let hostname = Remote "140.247.178.94:5900"
let ratpath = sprintf @"/root/data/asheshdhawale/Data/Gunakari/%s"

let mergertreeold = 
    use ms = new MemoryStream(getArray<byte> hostname (ratpath "allmergers") 0UL -1)
    FsPickler().Deserialize<(string * (string * Tree<Set<string> * (int * float) option> []) []) []>(ms)

let mergertree = 
    mergertreeold |> Array.iter (fun ((fnum,_) as x) ->
        use ms = new MemoryStream()
        FsPickler().Serialize(ms,x)
        writeArray hostname (ratpath (sprintf @"%s/allmergers" fnum)) 0UL (ms.ToArray()) |> Async.RunSynchronously
    )
