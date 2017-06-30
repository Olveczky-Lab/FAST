module ClusterPicker

open RP.HDF5.H5TypeProviderRuntime
open Helper

let getSpikesAndClusters fname chgroup offset numclustered = 
    let chstr = sprintf "/ChGroup_%s/" chgroup
    let chstrclus = chstr+"Clusters"
    let chstrclustered = chstr+"ClusteredSpikes"
    let chstrspikes = chstr+"Spikes"
    let dimsclus = getDataSetDimensions fname chstrclus
    let dimsspikes = getDataSetDimensions fname chstrspikes
    let numtemp = dimsclus.[1] |> int
    let spikelen = (dimsspikes.[1]|>int)*(dimsspikes.[2]|>int)
    let clus,offset,numspikes = 
            let clustered = readData fname chstrclustered [|offset|] null [|numclustered|>uint64|] :?> uint64[]
            let clusraw = readData fname chstrclus [|offset;0UL|] null [|numclustered|>uint64;dimsclus.[1]|] :?> uint16[,]                    
            (Array.zip (clustered|>Array.map (fun spikenum -> spikenum-clustered.[0] |> int)) 
                       (Array.init numclustered (fun i -> Array.init numtemp (fun temp -> clusraw.[i,temp])))),
            (clustered.[0]),
            (((clustered.[numclustered-1]-clustered.[0])|>int)+1)
    let chnums,spikes,spikesraw = getSpikes fname chgroup (numspikes|>uint64) (numspikes|>uint64) offset
    spikes,clus

let sortSpikes offset numclusteredspikes (spike_max_indx,chnum) (clus:(int*uint16[])[]) (spikes:(int64*int16[,])[]) = 
    let numtemp = clus.[0] |> snd |> Array.length
    let clus_amps =     
        Array.init numtemp (fun temp -> 
                                Seq.init numclusteredspikes 
                                         (fun i -> 
                                            let (spikenum,clu) = clus.[offset+i]
                                            let ((_,s)) = spikes.[spikenum]
                                            (clu.[temp],s.[spike_max_indx,chnum]))
                                |> Seq.groupBy fst
                                |> Seq.sortBy fst
                                |> Seq.map (fun (clunum,amps) -> 
                                                    let cnt = (Seq.length amps)
                                                    cnt,(amps |> Seq.map (snd>>float) |> Seq.sum)/(float cnt))
                                |> Seq.toArray
                                )
    let r = 
        clus_amps |> Seq.mapi (fun i clu -> i,Seq.zip clu ([|0us..((Array.length clu)|>uint16)-1us|]) 
                                                |> Seq.tryFind (fun ((cnt,amp),clunum) -> amp < -641.0 && cnt < 800 && cnt > 30))
                    |> Seq.tryFind (fun (i,x) -> Option.isSome x)
    match r with
    | Some (i,Some (_,c)) -> Some (i,c)
    | _ -> None

let label numperclustering (sorting:(int*uint16) option[]) (clus:(int*uint16[])[]) numspikes = 
    let isreal i =     
        match sorting.[i/numperclustering] with
        | Some (temp,clunum) -> (clus.[i] |> snd).[temp] = clunum
        | None -> false

    let lab = Array.zeroCreate<int> numspikes
    for i in 0..(Array.length clus)-1 do
        lab.[clus.[i]|>fst] <- if (isreal i) then 1 else 2
    lab
