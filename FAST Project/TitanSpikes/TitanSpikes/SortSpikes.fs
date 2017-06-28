#if COMPILED
module SortSpikes
#else
#load "SnippetHelper.fs"
#load "ClusterHelper.fs"
#load "SegmentationFusion.fs"
#load "SortingHelper.fs"
#load "PlotHelper.fs"
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open SnippetHelper
open ClusterHelper
open SegmentationFusion
open AgentHelper
open RP.HDF5.H5TypeProviderRuntime
open FSharp.Control
open System.IO
open SortingHelper

let minclussize = 5
let minstable = 2
let ntemp = 10
let numperclustering = 1000
let maxdist = 10.0

let noverlap = 10
let npersegfuse = 100
let startindx = 0

let readBlock numspikes chnum = Agent.Start(fun inbox -> 
    let chgroup = sprintf "%d" chnum
    let rec loop () = async{
        let! (reply:AsyncReplyChannel<_>,blocknum) = inbox.Receive()
        let files,islast = getfnames numspikes blocknum (npersegfuse+noverlap)
        let data =
            files |> Array.map (fun (spikesfname,clusfname,(start,count)) -> 
                let clus,offset,numspikes = 
                    getClusters clusfname chgroup
                        (start*numperclustering|>uint64) (count*numperclustering)
                let spikesraw = getSpikesRaw spikesfname chgroup (numspikes|>uint64) offset
                Array.init count (fun i -> 
                    clus.[i*numperclustering..(i+1)*numperclustering-1]
                    |>Array.map(fun (x,ys) -> x,ys.[0..ntemp-1])),spikesraw
            )
        let clus,spikesraw =
            let first = data.[0]
            if Array.length data = 1 then first else
            let totalspikes = data |> Array.sumBy (snd>>Array3D.length1)
            let tmp = Array3D.zeroCreate<int16> totalspikes (Array3D.length2 (first|>snd)) (Array3D.length3 (first|>snd))
            let clus =
                data |> Array.scan (fun (offset,_) (clus,raw) ->
                    for i in 0..Array3D.length1 raw-1 do
                        for j in 0..Array3D.length2 raw-1 do
                            for k in 0..Array3D.length3 raw-1 do
                                tmp.[i+offset,j,k] <- raw.[i,j,k]
                    let newclus = clus |> Array.map (Array.map (fun (a,b) -> (a+offset,b)))
                    (offset + (Array3D.length1 raw),newclus)
                ) (0,[||]) 
                |> fun xs -> xs.[1..] |> Array.map snd |> Array.concat
            clus,tmp
        reply.Reply(clus,spikesraw,islast)
        return! loop ()
    }
    loop ()
)

let allcs chnum =
    let numspikes = 
        let fname = sprintf @"Z:\badlands\635276396558304920_%d%s.h5"
        let spikesfname i = fname i ""
        let clusfname i = fname i "_Clusters"
        loadnumspikes (Array.init 1 (fun i -> (spikesfname i,clusfname i))) chnum numperclustering
    let reader = readBlock numspikes chnum
    let firstwindow = 0
    for blocknum in firstwindow..npersegfuse..(numspikes.[Array.length numspikes-1]|>snd) do 
        let clus,spikesraw,islast = reader.PostAndReply(fun reply -> reply,blocknum)
        printfn "%d" blocknum
//    let map (blocknum,clus,spikesraw,islast) = async{
//        return blocknum,SegFuseMap spikesraw clus minclussize minstable maxdist,islast
//    }
//    let reduce (prevSeg,cs) (blocknum,(stable,bpsol),islast) = 
//        let nextSeg,newcs = 
//            SegFuseReduce (stable|>Array.mapi (fun i x -> (i+blocknum),x)) bpsol prevSeg (if islast then 0 else noverlap-1)
//        printfn "Completed Block %d" blocknum
//        Some nextSeg,(List.append cs newcs)
//    mapreduce 16 map reduce (None,[]) 
//        (asyncSeq{
//            for blocknum in firstwindow..npersegfuse..(numspikes.[Array.length numspikes-1]|>snd) do 
//                let clus,spikesraw,islast = reader.PostAndReply(fun reply -> reply,blocknum)
//                yield blocknum,clus,spikesraw,islast
//        })
//    |> fun (lastSeg,cs) ->
//        lastSeg |> Option.get |> fst |> List.map (fun (_,c) ->
//            List.rev c
//        ) |> List.append cs

let cs = allcs 5