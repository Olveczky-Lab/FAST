module RunCluster

open Cluster
open Helper
open System.IO
open System

type AgentReply =
     | Data of int16[,,]
     | Saved

let getClusters hostname fname ntemp numperblock firstblock numblocks =
    let offset = (firstblock|>uint64)*(numperblock|>uint64)
    getArray2D<uint16> ntemp hostname fname offset (numblocks*numperblock)

let getClustersIndx hostname fname numperblock firstblock numblocks =
    let offset = (firstblock|>uint64)*(numperblock|>uint64)
    getArray<uint64> hostname fname offset (numblocks*numperblock)

let getCurSpikes hostname spikesfname nf spikenums =
    let nspikes = Array.length spikenums
    let framesize = nf*sizeof<int16>
    let lastindx = spikenums.[Array.length spikenums-1]
    let numperread = 100000UL
    let spikes = Array2D.zeroCreate<int16> nspikes nf
    let rec loop curoffset (curspikes:int16[,]) i = 
        if i < nspikes then
            let maxindx = curoffset + (Array2D.length1 curspikes|>uint64)
            if (spikenums.[i] < maxindx) then
                let m = spikenums.[i]-curoffset|>int
                Buffer.BlockCopy(curspikes,m*framesize,spikes,i*framesize,framesize)
                loop curoffset curspikes (i+1)
            else
                let newoffset = spikenums.[i]
                let numtoread = min numperread (lastindx-newoffset+1UL)
                let newspikes = getArray2D<int16> nf hostname spikesfname newoffset (numtoread|>int)
                loop newoffset newspikes i
    loop 0UL (Array2D.zeroCreate 0 nf) 0
    spikes

let cluster (tempstep,mintemp,maxtemp) featurefunc hostname spikesfname nf clusfname tmppath firstblock numblocks numperblock = 
    let swcycles = 300
    let knn = 11
    let seed = 0

    if not(Directory.Exists(tmppath)) then Directory.CreateDirectory(tmppath)|>ignore

    let clustercomputation blocknum spikes =
        let numspikes = Array2D.length1 spikes
        let features = featurefunc spikes (Array.init numspikes id)
        let clus = wave_clus mintemp maxtemp tempstep swcycles knn seed 
                                (sprintf @"%s\%015d" tmppath blocknum) 
                                features false
        let ntemp = Array.length (clus.[0])
        writeArray hostname clusfname ((blocknum|>uint64)*(numperblock|>uint64)) (Array2D.init numspikes ntemp (fun i j -> clus.[i].[j])) |> Async.RunSynchronously
    
    [|firstblock..firstblock+numblocks-1|]
    |> Array.Parallel.iter (fun blocknum ->
        let spikenums = getClustersIndx hostname (sprintf "%s.indx" clusfname) numperblock blocknum 1
        let spikes = getCurSpikes hostname spikesfname nf spikenums
        clustercomputation blocknum spikes
    )
    