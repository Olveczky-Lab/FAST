#if COMPILED
module GetTerminalClusters
#else
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open ClusterHelper
open ClusterTreeHelper
open RunCluster
open Helper
open Cluster
open SequentialClusterHelper
open Nessos.FsPickler
open System.IO
open System.Collections.Generic

let chgroup,spikelen,nchans = "15",64,4
let ntemp = 15
let hostname = Remote "140.247.178.16:8080"
let numperblock,firstblock,numblocks = 1000,14165,1000
let minclussize = 15

let sfname,stfname,clusfname,outfname = 
    let f = sprintf @"/root/data/rpoddar/badlands/635276396558304920/ChGroup_%s/%s" chgroup
    f "Spikes", f "SpikeTimes", f "Clusters1", f "Clusters2"

let numspikes,_ = 
    let fsp = new FsPickler()
    let numperload = 100
    [|firstblock..numperload..firstblock+numblocks-1|] |> Array.fold (fun (smalloffset,(largeoffset,largeoffsetb)) blocknum ->
        let small,large = 
            loadSmallAndLargeClusters 15 hostname sfname stfname clusfname numperblock ntemp spikelen nchans 
                blocknum (min numperload (firstblock+numblocks-blocknum))
        writeArray hostname (sprintf @"%s.indx" outfname) smalloffset small|> Async.RunSynchronously
        use ms = new MemoryStream()
        for c in large do
            fsp.Serialize(ms,c)|>ignore
        let bytes = ms.ToArray()
        writeArray hostname (sprintf "%s.large" clusfname) largeoffsetb bytes |> Async.RunSynchronously
        writeArray hostname (sprintf "%s.large.off" clusfname) largeoffset [|largeoffsetb|] |> Async.RunSynchronously
        printfn "Completed block %d" blocknum
        smalloffset+(Array.length small|>uint64),(largeoffset+(List.length large|>uint64),largeoffsetb+(Array.length bytes|>uint64))
    ) (0UL,(0UL,0UL))
