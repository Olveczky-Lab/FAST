#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\FsPickler\bin\Release\FsPickler.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeClusterer\TitanSpikeClusterer\bin\Release\TitanSpikeClusterer.exe"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikesBehavior\bin\Release\TitanSpikesBehavior.exe"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\TitanSpikes.exe"

open System.IO
open Helper
open Nessos.FsPickler
open BehaviorHelper
open HighResTimingHelper

let hostname = Remote @"140.247.178.16:5900"
let fpath = sprintf @"/root/data/rpoddar/arches/%s"
let exptid = 25

let savehrdatabin =
    let clocks =
        use fs = new MemoryStream(getArray<byte> hostname (fpath (sprintf @"%dOpConHRClocks" exptid)) 0UL -1)
        (FsPickler()).Deserialize<((int64*int64)*int64[])[]>(fs)
    use fs = new MemoryStream(getArray<byte> hostname (fpath (sprintf @"%dOpConHRTimes" exptid)) 0UL -1)
    FsPickler().Deserialize<EventTime[]*DIEventTime[]>(fs)
    |> fst |> Array.choose (fun evt -> getSampleNum clocks evt.NICtrTime |> Option.map (fun (_,snum) -> [|evt.RCEID|>uint64;snum|>uint64|]))
    |> Array.concat
    |> writeArray hostname (fpath (sprintf @"%dOpConHRTimes.bin" exptid)) 0UL |> Async.RunSynchronously
