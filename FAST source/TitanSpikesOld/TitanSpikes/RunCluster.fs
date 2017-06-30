#if COMPILED
module RunCluster
#else

#load "AgentHelper.fs"
#load "ThrottlingAgent.fs"
#load "Helper.fs"
#load "HDF5Helper.fs"
#load "Cluster.fs"
#load "RemoveNoise.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open Cluster
open AgentHelper
open ThrottlingAgent
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

let cluster getfeatures datafname channelgroup first count non_noise numperclust = 
    let dim = 10
    let tempstep = 0.01
    let mintemp = tempstep
    let maxtemp =  0.2
    let swcycles = 300
    let knn = 11
    let seed = 0
    let spike_max_indx = 31
    let initoffset = (getDataSetDimensions datafname ("./ChGroup_" + channelgroup + "/ClusteredSpikes")).[0]
    printfn "InitOffset: %d" initoffset
    let aS = agentSerial (0,initoffset,[])
    let aP = ThrottlingAgent(8)


    let clustseq numperread =
        let rec getValidSpikes offset = seq {
                if (offset < (first+count-1UL)) then
                    let numtoread = min numperread (first+count-offset)
                    match (aS.PostAndReply(fun reply ->
                                            ProcessDataWithReply
                                                ((fun x -> x, Data (getSpikes datafname channelgroup offset numtoread)),reply))) with
                    | Data spikes ->
                        let numspikes = (Array3D.length1 spikes)
                        let rec loop i curvalid curfirst = seq {
                            if (i = numspikes) then yield (EndIndex curfirst) else 
                                if (non_noise (offset-first+(i|>uint64)) (fun a b -> spikes.[i,a,b])) then
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
        getValidSpikes first
        
    let clustercomputations =
        clustseq 100000UL
        |> Seq.mapi (fun blocknum (spikestocluster,spikes,offset) -> 
                        (async {
                            let numspikes = Array.length spikestocluster
                            let features = getfeatures spikes spikestocluster
                            let clus = wave_clus mintemp (maxtemp+tempstep/2.0) tempstep swcycles knn seed 
                                                    (sprintf @"C:\EphysData\Sullivan\Ashesh\temp\%015d" blocknum) 
                                                    features
                            let ntemp = Array.length (clus.[0])
                            let writetofile n = 
                                writeData datafname ("./ChGroup_" + channelgroup + "/Clusters" )
                                    [|n;0UL|]
                                    null
                                    (Array2D.init numspikes ntemp (fun i j -> clus.[i].[j]))
                                writeData datafname ("./ChGroup_" + channelgroup + "/ClusteredSpikes") 
                                    [|n|]
                                    null
                                    (spikestocluster |> Array.map (fun i -> offset + (i|>uint64)))
                                if (blocknum%100 = 0) then
                                    printfn "Completed %d" (offset + (spikestocluster.[numspikes-1]|>uint64))
                                n+(numspikes|>uint64)
                                
                            let rec saveall blocknumtosave n xs =
                                match xs with
                                | (xblocknum,savefunc)::ys when (xblocknum = blocknumtosave) ->
                                        saveall (blocknumtosave+1) (savefunc n) ys
                                | _ -> (blocknumtosave,n,xs)
                            aS.Post( ProcessData
                                        (fun (blocknumtosave,n,xs) -> 
                                            let newxs = (blocknum,writetofile)::xs |> List.sortBy fst
                                            let r = saveall blocknumtosave n newxs
                                            r))
                        })
                    )   
    
    let numblocks =
        clustercomputations
        |> Seq.fold (fun cnt comp -> 
                            aP.DoWork(comp)
                            cnt+1) 0
    printfn "NumBlocks %d" numblocks
    aS.PostAndReply(fun reply -> WaitForCompletion ((fun (n,_,_) -> n >= numblocks),reply))

open RP.HDF5
let initfile datafname channelgroup = 
    use file = new H5File(datafname,FileOpen(true),None)
    use dset = file.OpenDataSet("./ChGroup_" + channelgroup + "/ClusteredSpikes")
    dset.Extend([|0UL|])
    use dset = file.OpenDataSet("./ChGroup_" + channelgroup + "/Clusters")
    dset.Extend([|0UL;20UL|])

open HDF5Helper
open RemoveNoise
let f0,s0,s1 = 635025802044762119L,8682375247L,8790114304L
let fname = (sprintf "%s\%d.h5" @"C:\EphysData\Sullivan\Ashesh\Bairagi" f0)

let chgroup = "26"
//initfile fname chgroup
//let non_noise = loadNonNoise fname chgroup s0 |> Array.map (fun cc -> cc <= 3)
let spikeoffset,spikecount = Option.get <| getSpikesIndex s0 s1 fname chgroup
printfn "%d %s %d %d" f0 chgroup spikeoffset spikecount
let featurefunc spikesraw curspikes = 
    let waveletfeatures = getFeatures spikesraw curspikes [|getwavelets [|0;1|] 64|]
                          |> cullfeaturesks 20
    let ampfeatures = getFeatures spikesraw curspikes [|getspikeamp [|(31,0);(32,1)|]>>(Array.map (fun x -> x*2.0))|]
    Array.zip waveletfeatures ampfeatures |> Array.map (fun (x,y) -> Array.append x y)
cluster featurefunc fname chgroup spikeoffset spikecount (fun i spike -> (spike 31 0) < -512s) 200
    