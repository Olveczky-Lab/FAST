module RunCluster

open AgentHelper
open Cluster
open RP.HDF5.H5TypeProviderRuntime 

let getSpikes fname chgroup offset count =
    let path = sprintf @"./ChGroup_%s/" chgroup
    let dims = getDataSetDimensions fname (path + "Spikes")
    let adj_count = 
        let remaining = dims.[0] - offset
        if (remaining < count) then remaining else count
    readData fname (path + "Spikes") [|offset;0UL;0UL|] null [|adj_count;dims.[1];uint64 dims.[2]|] :?> int16[,,]

type ValidSpikes =
     | Indices of int[]
     | EndIndex of int option

type AgentReply =
     | Data of int16[,,]
     | Saved

let numtemp = 20

let cluster datafname channelgroup channelstouse initoffset = 
    async {
        let dim = 15
        let maxtemp =  0.2
        let tempstep = maxtemp/(numtemp|>float)
        let mintemp = tempstep
        let swcycles = 200
        let knn = 11
        let seed = 0

        let aS = agentSerial (0,0UL,[])
        let aP = agentProcessor ()

        let numtotalspikes = 
            (getDataSetDimensions datafname ("./ChGroup_" + channelgroup + "/SpikeTimes")).[0]

        let clustseq numperread numperclust =
            let rec getValidSpikes offset = seq {
                    if (offset < numtotalspikes) then
                        match (aS.PostAndReply(fun reply ->
                                                (fun x -> x, Data (getSpikes datafname channelgroup offset numperread)),reply)) with
                        | Data spikes ->
                            let numspikes = (Array3D.length1 spikes)
                            let rec loop i curvalid curfirst = seq {
                                if (i = numspikes) then yield (EndIndex curfirst) else 
                                    if (spikes.[i,31,0] < -256s) then
                                        let newvalid = i::curvalid
                                        if (List.length newvalid = numperclust) then 
                                            yield (Indices (newvalid|>List.rev|>List.toArray))
                                            yield! loop (i+1) [] None
                                        else
                                            yield! loop (i+1) newvalid (if (Option.isNone curfirst) then (Some i) else curfirst)
                                    else 
                                        yield! loop (i+1) curvalid curfirst
                            }
                            for v in loop 0 [] None do
                                match v with
                                | Indices xs -> yield (xs,spikes,offset)
                                | EndIndex (Some i) -> if (i > 0) then yield! getValidSpikes (offset+(i|>uint64))
                                | EndIndex None -> yield! getValidSpikes (offset+(numspikes|>uint64))
                        | _ -> failwith "Not Possible"
            }
            getValidSpikes initoffset
        
        let clustercomputations =
            clustseq 100000UL 1000
            |> Seq.mapi (fun blocknum (spikestocluster,spikes,offset) -> 
                            (async {
                                let clus = wave_clus dim mintemp (maxtemp+tempstep/2.0) tempstep swcycles knn seed 
                                                        (sprintf @"C:\EphysData\Sullivan\Ashesh\tmp_wc_%d" blocknum) 
                                                        spikes spikestocluster channelstouse
                                let numnewspikes = Array.length spikestocluster
                                let clustosave = Array2D.init numnewspikes numtemp 
                                                    (fun i temp -> clus.[temp].[i])
                                let writetofile n = 
                                    writeData datafname ("./ChGroup_" + channelgroup + "/Clusters" )
                                        [|n;0UL|]
                                        null
                                        clustosave
                                    writeData datafname ("./ChGroup_" + channelgroup + "/ClusteredSpikes") 
                                        [|n|]
                                        null
                                        (spikestocluster |> Array.map (fun i -> offset + (i|>uint64)))
                                    aP.Post CompletedProcessing
                                    printfn "Completed Block %d" blocknum
                                    n+(numnewspikes|>uint64)
                                
                                let rec saveall blocknumtosave n xs =
                                    match xs with
                                    | (xblocknum,savefunc)::ys when (xblocknum = blocknumtosave) ->
                                            saveall (blocknumtosave+1) (savefunc n) ys
                                    | _ -> (blocknumtosave,n,xs)
                                aS.PostAndReply(fun reply ->
                                                    (fun (blocknumtosave,n,xs) -> 
                                                        let newxs = (blocknum,writetofile)::xs |> List.sortBy fst
                                                        let r = saveall blocknumtosave n newxs
                                                        r,Saved),reply)
                                |> ignore
                            })
                        )   
        for comp in clustercomputations do
            do! AsyncLoopUntil 1000 (fun () -> aP.PostAndReply (fun reply -> GetNumInProgress reply) < 20)
            aP.Post(ProcessData comp)
        do! AsyncLoopUntil 2000 (fun () -> (aP.PostAndReply (fun reply -> GetNumInProgress reply)) = 0)
    }

let datafname = @"C:\EphysData\Sullivan\Ashesh\635025802044762119.h5"
cluster datafname "1" [|1|] 0UL |> Async.RunSynchronously
