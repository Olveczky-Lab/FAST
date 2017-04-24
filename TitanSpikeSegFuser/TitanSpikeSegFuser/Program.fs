#if COMPILED
#else
#load "Helper.fs"
#load "SegmentationFusion.fs"
fsi.ShowDeclarationValues <- false
#time "on"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\bin\Release"
#endif
open RunCluster
open SegmentationFusion
open AgentHelper
open FSharp.Control
open System.IO
open Cluster

type Agent<'T> = MailboxProcessor<'T>

let ntemp = 10
let validclussize temp count = 
    match temp with
    | _ when temp <= 4 -> true
    | _ -> count >= 3

let numperclustering = 1000

let noverlap = 5
let npersegfuse = 50
let startindx = 0

let r () = 
    let hostname,spikesfname,spikelen,nchans,clusfname,ntemp,numperblock = 
        "140.247.178.16",@"/root/data/rpoddar/arches/635267094066850917/ChGroup_0/Spikes",64,4,
        @"/root/data/rpoddar/arches/tmp/ChGroup_0/Clusters",10,1000
    let clus = getSpikesAndClusters hostname spikesfname spikelen nchans clusfname ntemp numperblock 0 (npersegfuse+noverlap)
    let portnum = 5559
    let stable,l = SegFuseMap portnum clus validclussize
    printfn "NumClusters: %d" ((stable |> Array.sumBy (fst>>Array.length))/(Array.length stable))
    use ms = new MemoryStream()
    use bw = new BinaryWriter(ms)
    bw.Write(npersegfuse+noverlap)
    SegFuseReduce stable l bw None 0 true |> ignore
    
    ms.Seek(0L,SeekOrigin.Begin)|>ignore
    use br = new BinaryReader(ms)
    let clusters,links = deserialize br
    let cs = GetAllChains (links |> Array.mapi (fun i x -> clusters.[i+1]|>fst|>Array.length,x))
    dispSeq (cs|>List.filter(fun x -> List.length x > 1)|>List.map (List.map (fun (i,j) -> (i+blocknum,(clusters.[i]|>fst).[j])))) dispchain
    ()

let allcs outfile numspikes chgroup =
    use fs = new FileStream(outfile,FileMode.Create)
    use bw = new BinaryWriter(fs)
    let firstwindow = 0
    let nblocks = numspikes.[Array.length numspikes-1]|>snd
    bw.Write(nblocks)
    let map (blocknum,i,clus,spikesraw,islast) = async{
        return blocknum,SegFuseMap (13000+i%1000) spikesraw clus validclussize,islast
    }
    let reduce prevSeg (blocknum,(stable,links),islast) = 
        let nextSeg = SegFuseReduce stable links bw prevSeg (if islast then 0 else noverlap-1) islast
        printfn "Completed Block %d" blocknum
        printfn "NumClusters: %d" ((stable |> Array.sumBy (fst>>Array.length))/(Array.length stable))
        Some nextSeg
    mapreduce 8 map reduce None
        (asyncSeq{
            let blocks = [|firstwindow..npersegfuse..nblocks|]
            for i in 0..Array.length blocks-1 do 
                let clus,spikesraw,islast = reader.PostAndReply(fun reply -> reply,blocks.[i])
                yield blocks.[i],i,clus,spikesraw,islast
        })    

[<EntryPoint>]
let main argv = 
    match argv with
    | [|"BlockTimes" as oper; filepath;chgroup;first;last;_|]
    | [|"SegFuse" as oper;filepath;chgroup;first;last;_|]
    | [|"NumSpikes" as oper;filepath;chgroup;first;last|] ->
        let numspikes = getNumspikes filepath chgroup (first|>int) (last|>int)
        let ntotal = numspikes.[Array.length numspikes-1]|>snd
        printfn "Total Blocks\t%d" ntotal
        match oper with
        | "SegFuse" ->
            allcs argv.[5] numspikes chgroup |> ignore
        | "BlockTimes" ->
            let times = 
                Array.init ntotal (fun blocknum ->
                    let fnames,_ = getfnames numspikes blocknum 1
                    let (spikesfname,clusfname,(start,count)) = fnames.[0]
                    let spikenums = readData clusfname (sprintf "./ChGroup_%s/ClusteredSpikes" chgroup) [|(start*numperclustering|>uint64)+(numperclustering/2|>uint64)|] null [|1UL|] :?> uint64[]
                    let spiketimes = readData spikesfname (sprintf "./ChGroup_%s/SpikeTimes" chgroup) [|spikenums.[0]|] null [|1UL|] :?> int64[]
                    spiketimes.[0]
                )
            use fs = new FileStream(argv.[5],FileMode.Create)
            use bw = new BinaryWriter(fs)
            bw.Write(Array.length times)
            for t in times do
                bw.Write(t)
        | _ -> ()
    | _ -> printfn "Invalid Arguments"
    0 // return an integer exit code