module ClusterHelper

open Helper
open RunCluster
open System
open System.IO
open Nessos.FsPickler
open IntelIPP

let getArrayReader<'a> hostname fname =
    let fsp = new FsPickler()
    let offsets = getSpikeTimes hostname (sprintf "%s.off" fname) 0UL -1
    Array.length offsets,
    fun first num ->
        let offset = if first = 0 then 0UL else offsets.[first-1]
        let bytes = getBytes hostname fname offset (offsets.[first+num-1]-offset|>int)|>Async.RunSynchronously
        use ms = new MemoryStream(bytes)
        Array.init num (fun _ -> fsp.Deserialize<'a>(ms))

let getFirstAndLast spikenumss = 
    spikenumss |> Array.choose (fun spikenums -> 
        let n = Array.length spikenums
        if n > 0 then 
            Some (spikenums.[0],spikenums.[n-1])
        else None
    ) 
    |> fun x -> 
        if Array.length x = 0 then None else Some (Array.minBy fst x|>fst,Array.maxBy snd x|>snd)

let getSubsets numperread spikeloadfun spikenumss = 
    match getFirstAndLast spikenumss with
    | None -> spikenumss |> Array.map (fun _ -> [||])
    | Some (firstindx,lastindx) ->
        let rec loadloop state curoffset = 
            let numtoread = min numperread (lastindx-curoffset+1UL)            
            let subspikefun = spikeloadfun curoffset (numtoread|>int)
            let maxindx = curoffset + numtoread
            let newstate = 
                state |> Array.map (fun x ->
                    let rec update (spikenums,spikes) =
                        match spikenums with
                        | h::t when h < maxindx ->
                            let i = h-curoffset|>int
                            update (t,subspikefun i::spikes)
                        | _ -> (spikenums,spikes)
                    update x
                )
            if (newstate |> Array.map (fst>>List.isEmpty>>not) |> Array.exists id) then
                let newoffset = 
                    newstate |> Array.choose (fun (rem,_) -> 
                        match rem with
                        | h::t -> Some h
                        | _ -> None
                    ) |> Array.min
                loadloop newstate newoffset
            else newstate
        loadloop (spikenumss |> Array.map (fun x -> x|>Array.toList,[])) firstindx
        |> Array.map (snd>>List.rev>>List.toArray)

let getSpikeTimeSubsets hostname stfname spikenumss =
    let numperread = 1000000UL
    let spikeloadfun offset numtoread =
        let curspiketimes = getSpikeTimes hostname stfname offset numtoread
        (fun i -> curspiketimes.[i])
    getSubsets numperread spikeloadfun spikenumss

let getArraySubsets<'a> hostname fname spikenumss =
    let numperread = 100000UL
    let _,reader = getArrayReader<'a> hostname fname
    let spikeloadfun offset numtoread =
        let xs = reader (offset|>int) numtoread
        fun i -> xs.[i]
    getSubsets numperread spikeloadfun spikenumss

let getSpikeSubsets hostname sfname stfname nf spikenumss = 
    let numperread = 100000UL
    let spikeloadfun offset numtoread =
        let curspiketimes = getSpikeTimes hostname stfname offset numtoread
        let curspikes = getArray2D<int16> nf hostname sfname offset numtoread
        (fun i -> curspiketimes.[i],curspikes.[i,0..])
    getSubsets numperread spikeloadfun spikenumss

let getSpikeAmpSubsets hostname sfname stfname nf nchans spikenumss =
    let numperread = 100000UL
    let spikeloadfun offset numtoread =
        let curspiketimes = getSpikeTimes hostname stfname offset numtoread
        let curspikes = getArray2D<int16> nf hostname sfname offset numtoread
        (fun i -> curspiketimes.[i],curspikes.[i,31*nchans..32*nchans-1])
    getSubsets numperread spikeloadfun spikenumss

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

let getClusterSpikeNums hostname fname numperblock firstblock numblocks =
    let _,reader = getArrayReader<uint64[]> hostname fname
    reader (firstblock*numperblock) (numblocks*numperblock)

let getMedianOffset hostname fname ts =
    let numspikes,reader = getArrayReader<uint64[]> hostname fname
    let getspiketime i =
         (reader (i|>int) 1).[0] |> fun x -> x.[Array.length x/2]
    ts |> Array.map (fun t ->
        match binSearch (fun i -> compare t (getspiketime i)) 0L ((numspikes|>int64)-1L) with
        | Exact x
        | LargerThan x -> x+1L
        | TooSmall -> 0L
        |> uint64    
    )

let getSpikesEx hostname sfname stfname nf nchans l2cfname numperblock firstindx lastindx = 
    printfn "%d" (lastindx-firstindx+1UL)
    let allspikeamps = getSpikeAmpSubsets hostname sfname stfname nf nchans [|[|firstindx..lastindx|]|]
    let [|firstblock;lastblock|] = 
        getMedianOffset hostname l2cfname [|firstindx;lastindx|]|> Array.map (int>>fun x -> x/numperblock)
    let numblocks = lastblock-firstblock+1
    let csnums = getClusterSpikeNums hostname l2cfname numperblock firstblock numblocks
    Array.Parallel.init numblocks (fun i ->
        Array.init numperblock (fun j ->
            let m = i*numperblock + j
            csnums.[m]|>Array.filter (fun x -> x >= firstindx && x <= lastindx) |> Array.map (fun x -> x - firstindx|>int)
        )
    ),allspikeamps.[0],firstblock,numblocks
