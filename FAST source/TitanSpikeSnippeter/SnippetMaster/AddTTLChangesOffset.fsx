#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
open Helper
open System.IO

let path = @"Z:\badlands\635276396558304920"
let hostname = "140.247.178.16:8080"
for chnum in [6;7] do
    let n = FileInfo(sprintf "%s\TTLChanges\Ch_%d" path chnum).Length/8L|>uint64
    let fname = sprintf "/root/data/rpoddar/badlands/635276396558304920/TTLChanges/Ch_%d" chnum 
    let nperread = 100000000UL
    for (offset,count) in [0UL..nperread..n-1UL]|>List.map(fun x -> x,min nperread (n-x)|>int) do
        let data = getTTLChanges hostname fname offset count
        for i in 0..Array.length data-1 do
            data.[i] <- data.[i]+3000UL
        writeArray hostname fname offset data |> Async.RunSynchronously

