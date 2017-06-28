module ClusterHelper

open RP.HDF5.H5TypeProviderRuntime
open HDF5Helper
open System.IO

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
    spikes,clus,spikesraw

let getStableClusters ntemp clus =
    let minclussize = 8
    let rec rtree (temp,clu,count) (firsttemp,xs) = seq {
        let newxs = ((clu,count)::xs)
        let fatemap (temp,clu) =
            if (temp = ntemp-1) then [] else
                if (temp = -1) then clus else
                    clus |> Array.filter (fun (_,xs) -> Array.get xs temp = clu)
                |> Seq.countBy (fun (_,xs) -> Array.get xs (temp+1))
                |> Seq.filter (fun (_,cnt) -> cnt >=minclussize)
                |> Seq.sortBy fst
                |> Seq.toList
        match fatemap (temp,clu) with
        | [clunum,clucount] -> yield! rtree (temp+1,clunum,clucount) (firsttemp,newxs)
        | fates -> yield firsttemp,newxs|>List.rev
                   for (clunum,clucount) in fates do 
                        yield! rtree (temp+1,clunum,clucount) (temp+1,[])
    }
    rtree (-1,0us,Array.length clus) (-1,[])

let countClusters clus = 
    let ntemp = clus |> Seq.head |> snd |> Array.length
    Array.init ntemp (fun i ->  clus |> Seq.countBy (fun (_,c) -> c.[i]))

type spikeclusterlabel =
| GoodCluster
| BadCluster
| UnClustered

let label numperclustering (sorting:(int*uint16)list[]) (clus:(int*uint16[])[]) numspikes = 
    let classify i =     
        let isgood (temp,clunum) = if (temp = -1) then true else (clus.[i] |> snd).[temp] = clunum
        let rec l xs =
            match xs with
            | clu::ys -> if isgood clu then true else l ys
            | [] -> false
        if (l sorting.[i/numperclustering]) then GoodCluster else BadCluster
    let lab = Array.init numspikes (fun i -> UnClustered)
    for i in 0..(Array.length clus)-1 do
        lab.[clus.[i]|>fst] <- classify i
    lab

let inline loadSorting fname = 
    File.ReadAllLines(fname)
    |> Array.map (fun str -> 
        str.Split(' ').[1..]
        |> Array.map (fun x -> 
                        match x.Split(',') with
                        | [|x;y|] -> (x|>int),(y|>uint16)
                        | _ -> failwith "Invalid Format")
        |> Array.toList)

let inline saveSorting fname s =
    s
    |> Array.mapi (fun i x ->
        x |> List.map (fun (temp,clunum) -> sprintf "%d,%d" temp clunum)
        |> String.concat " "
        |> fun x -> sprintf "%d %s" i x)
    |> fun x ->  File.WriteAllLines(fname,x)

               