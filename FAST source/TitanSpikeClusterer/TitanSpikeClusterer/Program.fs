#if COMPILED
#else
#load "Cluster.fs"
#load "RunCluster.fs"

#time "on"

#endif

open RunCluster
open Cluster
open Helper
open ZMQMapReduce
open SequentialClusterHelper
open System.IO
open Nessos.FsPickler
open AutoSortHelper
open ClusterTreeHelper
open SegmentationFusion
open ClusterHelper

let clusterws () =
    let worker,server = GetWorkerAndServer()
    (fun keys endpoint ->
        server 30 0 keys endpoint |> printfn "%A"
    ),
    (fun f endpoint workername ->
        worker (fun key -> 
            f key
            fun v -> v,[||]
        ) endpoint workername
    )

let getterminalws () =
    let fsp = new FsPickler()
    let worker,server = GetWorkerAndServer()
    let numperload = 10
    (fun firstblock numblocks endpoint ->
        server 30 (0UL,(0UL,0UL)) 
            ([firstblock..numperload..firstblock+numblocks-1]|>List.map (fun blocknum -> 
                blocknum,min numperload (firstblock+numblocks-blocknum)))
            endpoint |> printfn "%A"
    ),
    (fun minclussize hostname sfname stfname clusfname outfname numperblock ntemp spikelen nchans endpoint workername ->
        worker (fun (blocknum,numblocks) ->
            let small,large = 
                loadSmallAndLargeClusters minclussize hostname sfname stfname clusfname numperblock ntemp spikelen nchans 
                    blocknum numblocks
            fun (smalloffset,(largeoffset,largeoffsetb)) ->
                writeArray hostname (sprintf @"%s.indx" outfname) smalloffset small|> Async.RunSynchronously
                use ms = new MemoryStream()
                let largeoffsets =
                    large |> Array.map (fun c ->
                        fsp.Serialize(ms,c)|>ignore
                        (ms.Position |> uint64)+largeoffsetb
                    ) 
                let bytes = ms.ToArray()
                writeArray hostname (sprintf "%s.large" clusfname) largeoffsetb bytes |> Async.RunSynchronously
                writeArray hostname (sprintf "%s.large.off" clusfname) largeoffset largeoffsets |> Async.RunSynchronously
                printfn "Completed block %d" blocknum
                (smalloffset+(Array.length small|>uint64),(largeoffset+(Array.length large|>uint64),largeoffsetb+(Array.length bytes|>uint64))),[||]
            ) endpoint workername
    )

let getautosortws hostname l2sfname l2stfname spikelen nchans l2clusfname l2clustemp maxtempnum numperblock =
    let worker,server = GetWorkerAndServer()
    (fun numblocks outpath outprefix endpoint ->
        if numblocks > 1 then    
            let npersegfuse,noverlap = 10,5
            let keys = 
                [0..npersegfuse-noverlap..(max 0 (numblocks-noverlap-1))]
                |> fun xs ->
                    let n = List.length xs
                    xs |> List.mapi (fun i a ->
                        a,(min numblocks (a+npersegfuse))-1, if i < n-1 then noverlap-1 else 0
                    )
            let links = 
                server 30 ([],None) keys endpoint 
                |> fun (alllinks,lastlinks) ->
                    (lastlinks |> Option.get)::alllinks |> List.rev |> List.toArray |> Array.concat
            let chainnodes,nchains = 
                Array.append links [|[||]|] |> Array.fold (fun (chainnodes,state) links -> 
                    let state,nodes = ExtendRealChain state links
                    (nodes::chainnodes),state
                ) ([],(0,Map.empty))
                |> fun (a,(b,_)) -> 
                    a |> List.rev,
                    b
            printfn "%d chains found" nchains
            use ms = new MemoryStream()
            chainnodes |> List.fold (fun offsets nodes ->
                FsPickler().Serialize(ms,nodes)
                (ms.Position |> uint64)::offsets
            ) [] |> List.rev |> List.toArray
            |> writeArray hostname (sprintf @"%s/%slinks.off" outpath outprefix) 0UL |> Async.RunSynchronously
            writeArray hostname (sprintf @"%s/%slinks" outpath outprefix) 0UL (ms.ToArray()) |> Async.RunSynchronously
    ),
    (fun () ->
        let map (firstblock,lastblock,nNextSeg) = 
            let clustertrees,clus = 
                loadClusterTree hostname l2sfname l2stfname spikelen nchans l2clusfname l2clustemp numperblock firstblock (lastblock-firstblock+1)
            let clustertrees =
                let rec loop (Node(t,sts)) =                
                    Node(t,sts|>Array.filter (fun (Node((_,clu),_)) -> clu.Temp <= maxtempnum)|> Array.map loop)
                clustertrees |> Array.Parallel.map (fun clustertree ->
                    loop clustertree
                )
            let numberedclusters = 
                clustertrees |> Array.Parallel.map (NumberPaths>>(fun (_,pathlist,clus) -> pathlist,clus))
            let validclu i j = (numberedclusters.[i]|>snd).[j] |>snd |> getClusAmp nchans > 75.
            SegFuseMap -1 l2clustemp 
                (numberedclusters |> Array.map (fun (pathlist,clus) -> 
                    //Remove root node
                    let nc = Array.length clus
                    clus|>Array.map snd|>fun x -> x.[0..nc-2],pathlist|>List.map (List.tail)
                )) |> Array.mapi (fun i xs -> xs |> Array.filter (fun (a,b) -> validclu i a && validclu (i+1) b)),
            nNextSeg
        let reduce (alllinks,prevlinks) (curlinks,nNextSeg) =
            let links,nextlinks = SegFuseReduce curlinks prevlinks nNextSeg
            links::alllinks,Some nextlinks
        worker (fun key -> 
            let v = map key
            fun state -> reduce state v,[||]
        )
    )

let getautosortsaverws hostname l2sfname l2stfname spikelen nchans l2clusfname l2clustemp maxtempnum numperblock outpath outprefix =
    let worker,server = GetWorkerAndServer()
    let _,nodesreader = getArrayReader<(int*int) list> hostname (sprintf @"%s/%slinks" outpath outprefix)
    (fun numblocks endpoint ->
        if numblocks > 1 then    
            server 30 Map.empty [0..numblocks-1] endpoint |> ignore
    ),
    (fun () ->
        let map blocknum = 
            let clustertrees,clus = 
                loadClusterTree hostname l2sfname l2stfname spikelen nchans l2clusfname l2clustemp numperblock blocknum 1
            let clustertrees =
                let rec loop (Node(t,sts)) =                
                    Node(t,sts|>Array.filter (fun (Node((_,clu),_)) -> clu.Temp <= maxtempnum)|> Array.map loop)
                clustertrees |> Array.Parallel.map (fun clustertree ->
                    loop clustertree
                )
            let numberedclusters = 
                clustertrees |> Array.Parallel.map (NumberPaths>>(fun (_,pathlist,clus) -> pathlist,clus))
            let shapes = clus |> Array.map (Array.map (fst>>snd)) |> Array.concat
            let clustertrees = clustertrees|> Array.map (mapTree (fun (_,x) _ -> x))
            let nodes = 
                nodesreader blocknum 1 |> fun x -> x.[0]
                |> List.map (fun (clusternum,chainnum) -> 
                    (numberedclusters.[0]|>snd).[clusternum]|>fst,chainnum)
            let stragglerMap = 
                getStragglersFromChainNodes nchans numperblock shapes clustertrees 
                    ([|0,nodes|>List.map fst|])
                |> Array.map (fun ((spikenum,(_,clupath)),_) -> clupath,spikenum)
                |> Seq.groupBy fst |> Seq.map (fun (key,xs) -> key,xs|>Seq.map snd|>Seq.toArray)|>Map.ofSeq
            nodes |> List.map (fun (clupath,chainnum) ->
                match Map.tryFind clupath stragglerMap with
                | Some x -> x
                | None -> [||]
                |> Array.append (getNode clustertrees.[0] clupath).Items
                |> Array.sort |> fun xs -> chainnum,(xs |> Array.map (fun x -> 
                    (blocknum|>uint64)*(numperblock|>uint64) + (x|>uint64)))
            )
        let reduce offsetMap chainedspikes =
            chainedspikes |> List.fold (fun newMap (chainnum,spikenums) ->
                let offset = 
                    match Map.tryFind chainnum offsetMap with
                    | Some x -> x
                    | None -> 0UL
                //Write to file
                writeArray hostname (sprintf "%s/%s%d.l2snums" outpath outprefix chainnum) offset spikenums |> Async.RunSynchronously
                Map.add chainnum (offset+(Array.length spikenums|>uint64)) newMap
            ) Map.empty
        worker (fun key -> 
            let v = map key
            fun state -> reduce state v,[||]
        )
    )

[<EntryPoint>]
let main argv =
    match argv with
    | [|"ClusterWorkerSingleBlock";tempdir;hostname;datafname;nchans;outfname;mintemp;maxtemp;blocknum|] ->
        let nchans = nchans|>int
        let spikelen = 64
        let featurefunc (spikesraw:int16[,]) spikestocluster =
            spikestocluster |> Array.map (fun i -> scalespike nchans spikesraw.[i,0..])
        cluster (mintemp|>float,mintemp|>float,maxtemp|>float) featurefunc 
            (Remote hostname) datafname ((spikelen|>int)*(nchans|>int)) outfname tempdir
            (blocknum|>int) 1 1000
        0
    | [|"ClusterWorkerBlockRange";tempdir;hostname;datafname;nchans;outfname;mintemp;maxtemp;firstblock;numblocks|] ->
        let nchans = nchans|>int
        let spikelen = 64
        let firstblock = firstblock|>int
        let numblocks = numblocks|>int
        let featurefunc (spikesraw:int16[,]) spikestocluster =
            spikestocluster |> Array.map (fun i -> scalespike nchans spikesraw.[i,0..])
        for blocknum in firstblock..firstblock+numblocks-1 do
            cluster (mintemp|>float,mintemp|>float,maxtemp|>float) featurefunc 
                (Remote hostname) datafname ((spikelen|>int)*(nchans|>int)) outfname tempdir
                (blocknum|>int) 1 1000
        0
    | [|"ClusterWorker";hostname;datafname;nchans;outfname;mintemp;maxtemp;endpoint;workername|] ->
        let nchans = nchans|>int
        let spikelen = 64
        let featurefunc (spikesraw:int16[,]) spikestocluster =
            spikestocluster |> Array.map (fun i -> scalespike nchans spikesraw.[i,0..])
        let f blocknum = 
            cluster (mintemp|>float,mintemp|>float,maxtemp|>float) featurefunc 
                (Remote hostname) datafname ((spikelen|>int)*(nchans|>int)) outfname @"C:\temp\temp"
                blocknum 1 1000
        let _,worker = clusterws ()
        worker f endpoint workername
        0
    | [|"ClusterServer";firstblock;lastblock;endpoint|] ->
        let server,_ = clusterws ()
        let fb = firstblock|>int
        let lb = lastblock|>int
        server [fb..lb] endpoint
        0        
    | [|"TerminalWorker";minclussize;hostname;sfname;stfname;clusfname;outfname;nchans;endpoint;workername|] ->
        let nchans = nchans|>int
        let _,worker = getterminalws ()
        worker (minclussize|>int) (Remote hostname) sfname stfname clusfname outfname 1000 15 64 nchans endpoint workername
        0
    | [|"TerminalServer";numblocks;endpoint|] ->
        let server,_ = getterminalws ()
        server 0 (numblocks|>int) endpoint
        0        
    | [|"Level2Worker";hostname;sfname;l2fname;firstspike;nspikes;nchans;nlevels|] ->
        let nchans = nchans|>int
        let f = sprintf "%s/%s" l2fname
        processlevel2 (Remote hostname) sfname (f @"Level2/Spikes") (f @"Level2/SpikeTimes") 
            (f @"Level2/ClusterSpikeNums") (f @"Level2/Clusters") 
            (firstspike|>uint64) (nspikes|>uint64) (Array.init (nlevels|>int) (fun i -> 
                f (sprintf @"Clusters%d.large" (i+1))
            ))
            15 50. 1000 15 64 (nchans|>int)
        0
    | [|"AutoSortServer";hostname;fpath;nchans;l2clustemp;maxtempnum;numblocks;endpoint|] ->
        let nchans = nchans|>int
        let spikelen = 64
        let f = sprintf "%s/%s" fpath
        let server,_ = 
            getautosortws (Remote hostname) (f @"Level2/Spikes") (f @"Level2/SpikeTimes") spikelen nchans (f @"Level2/Clusters") (l2clustemp|>int) 
                (maxtempnum|>int) 1000
        server (numblocks|>int) (f @"AutoSort") "" endpoint
        0
    | [|"AutoSortWorker";hostname;fpath;nchans;l2clustemp;maxtempnum;endpoint;workername|] ->
        let nchans = nchans|>int
        let spikelen = 64
        let f = sprintf "%s/%s" fpath
        let _,worker = 
            getautosortws (Remote hostname) (f @"Level2/Spikes") (f @"Level2/SpikeTimes") spikelen nchans (f @"Level2/Clusters") (l2clustemp|>int) 
                (maxtempnum|>int) 1000
        worker () endpoint workername
        0
    | [|"AutoSortSaverServer";hostname;fpath;nchans;l2clustemp;maxtempnum;numblocks;endpoint|] ->
        let nchans = nchans|>int
        let spikelen = 64
        let f = sprintf "%s/%s" fpath
        let server,_ = 
            getautosortsaverws (Remote hostname) (f @"Level2/Spikes") (f @"Level2/SpikeTimes") spikelen nchans (f @"Level2/Clusters") (l2clustemp|>int) 
                (maxtempnum|>int) 1000 (f @"AutoSort") ""
        server (numblocks|>int) endpoint
        0
    | [|"AutoSortSaverWorker";hostname;fpath;nchans;l2clustemp;maxtempnum;endpoint;workername|] ->
        let nchans = nchans|>int
        let spikelen = 64
        let f = sprintf "%s/%s" fpath
        let _,worker = 
            getautosortsaverws (Remote hostname) (f @"Level2/Spikes") (f @"Level2/SpikeTimes") spikelen nchans (f @"Level2/Clusters") (l2clustemp|>int) 
                (maxtempnum|>int) 1000 (f @"AutoSort") ""
        worker () endpoint workername
        0
    | _ -> 
        printfn "%A" argv
        failwith "Invalid Arguments"
