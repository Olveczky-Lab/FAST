module ClusterHelper

open Helper
open RunCluster
open System
open System.IO
open Nessos.FsPickler
open IntelIPP

let getFirstAndLast spikenumss = 
    spikenumss |> Array.map (fun spikenums -> 
        let n = Array.length spikenums
        spikenums.[0],spikenums.[n-1]
    ) 
    |> fun x -> Array.minBy fst x|>fst,Array.maxBy snd x|>snd

let getSpikeSubsets hostname sfname stfname nf spikenumss = 
    let numperread = 100000UL
    let firstindx,lastindx = getFirstAndLast spikenumss
    let rec loadloop state curoffset = 
        let numtoread = min numperread (lastindx-curoffset+1UL)            
        let curspikes = getArray2D<int16> nf hostname sfname curoffset (numtoread|>int)
        let curspiketimes = getSpikeTimes hostname stfname curoffset (numtoread|>int)
        let maxindx = curoffset + (Array2D.length1 curspikes|>uint64)
        let newstate = 
            state |> Array.map (fun x ->
                let rec update (spikenums,spikes) =
                    match spikenums with
                    | h::t when h < maxindx ->
                        let i = h-curoffset|>int
                        update (t,(curspiketimes.[i],curspikes.[i,0..])::spikes)
                    | _ -> (spikenums,spikes)
                update x
            )
        if (newstate |> Array.map (fst>>List.isEmpty>>not) |> Array.exists id) then
            loadloop newstate (curoffset+numtoread)
        else newstate
    loadloop (spikenumss |> Array.map (fun x -> x|>Array.toList,[])) firstindx
    |> Array.map (snd>>List.rev>>List.toArray)

let getSpikesAndClusters hostname sfname stfname nf clusfname ntemp numperblock firstblock numblocks =
    let clusraw = getClusters hostname clusfname ntemp numperblock firstblock numblocks
    let clusindx = getClustersIndx hostname (sprintf "%s.indx" clusfname) numperblock firstblock numblocks
    let spikes = getSpikeSubsets hostname sfname stfname nf [|clusindx|]
    Array.init numblocks (fun i ->
        Array.init numperblock (fun j ->
            let m = i*numperblock + j
            spikes.[0].[m],clusraw.[m,0..]
        )
    )

let getClusSpikes clus tempnum clunum =
    clus
    |> Array.choose (fun (spike,xs:uint16[]) -> 
        if tempnum = -1 then Some spike else
        if xs.[tempnum]=clunum then Some spike else None)

let inline avgShape shapes = 
    let nspikes = Array.length shapes
    let nf = shapes.[0] |> snd |> Array.length
    let sf = shapes |> Array.sumBy fst
    Array.init nf (fun i -> 
        Array.init nspikes (fun j -> (shapes.[j]|>fst)*(shapes.[j]|>snd).[i]) |> Array.sum
        |> fun x -> x/sf
    )

let getSpikesEx hostname sfname stfname nf clusfname l2cfname ntemp numperblock firstblock numblocks = 
    let clusraw = getClusters hostname clusfname ntemp numperblock firstblock numblocks
    let clusindx = getClustersIndx hostname (sprintf "%s.indx" clusfname) numperblock firstblock numblocks
    let clusterspikenums = 
        let bytes = getBytes hostname l2cfname 0UL -1|>Async.RunSynchronously
        use ms = new MemoryStream(bytes)
        let fsp = new FsPickler()
        for i in 0..firstblock*numperblock-1 do
            fsp.Deserialize<uint64[]>(ms) |> ignore
        Array.init (numperblock*numblocks) (fun i -> fsp.Deserialize<uint64[]>(ms))
    let firstindx,lastindx = getFirstAndLast clusterspikenums
    printfn "%d\t%d" firstindx lastindx
    let allspikes = getSpikeSubsets hostname sfname stfname nf [|[|firstindx..lastindx|]|]
    Array.init numblocks (fun i ->
        Array.init numperblock (fun j ->
            let m = i*numperblock + j
            clusterspikenums.[m]|>Array.map (fun x -> x - firstindx|>int)
        )
    ),allspikes.[0]
