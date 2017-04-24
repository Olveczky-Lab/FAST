module AutoSortHelper

open Cluster
open ClusterHelper
open ClusterTreeHelper
open SegmentationFusion
open Helper
open Nessos.FsPickler
open System.IO

let chainClusterTrees ntemp nchans clustertrees =
    let numblocks = Array.length clustertrees
    if numblocks = 1 then [] else
    let numberedclusters = 
        clustertrees |> Array.Parallel.map (NumberPaths>>(fun (_,pathlist,clus) -> pathlist,clus))
    let portnum = 5632
    let r = 
        let npersegfuse,noverlap = 10,5
        [|0..npersegfuse-noverlap..(max 0 (numblocks-noverlap-1))|]
        |> Array.map (fun a ->
            a,(min numblocks (a+npersegfuse))-1
        )
        |> fun xs -> 
            //Hack to deal with ZMQ bug when invoked in large numbers in parallel
            let n = Array.length xs
            let nperchunk = 100
            let ys = 
                [|0..nperchunk..n-1|] 
                |> Array.map (fun i -> 
                    xs.[i..(min n (i+nperchunk))-1]
                    |> Array.Parallel.map (fun (a,b) ->
                        SegFuseMap (portnum+a) ntemp 
                            (numberedclusters.[a..b] |> Array.map (fun (pathlist,clus) -> 
                                //Remove root node
                                let nc = Array.length clus
                                clus|>Array.map snd|>fun x -> x.[0..nc-2],pathlist|>List.map (List.tail)
                ))))
                |> Array.concat
            ys |> Array.mapi (fun i x ->
                x,if i<n-1 then noverlap-1 else 0
            )
        |> Array.fold (fun (alllinks,prevlinks) (curlinks,nNextSeg) ->
            let links,nextlinks = SegFuseReduce curlinks prevlinks nNextSeg
            links::alllinks,Some nextlinks
        ) ([],None)
        |> fun (alllinks,lastlinks) ->
            (lastlinks |> Option.get)::alllinks |> List.rev |> List.toArray |> Array.concat
    let validclu i j = (numberedclusters.[i]|>snd).[j] |>snd |> getClusAmp nchans > 75.
    let numclusts = numberedclusters |> Array.map (snd>>Array.length)
    let chains =
        GetAllChains numclusts.[0] (r |> Array.mapi (fun i x ->             
            numclusts.[i+1],
            x |> Array.filter (fun (a,b) -> validclu i a && validclu (i+1) b
            )
        ))
    chains
    |> List.map (fun x -> 
        x |>List.map (fun (i,j) -> i,(numberedclusters.[i]|>snd).[j]|>fst))
    |> List.filter (fun x -> List.length x > 1)

let loadClusterTree hostname sfname stfname spikelen nchans clusfname clustemp numperblock firstblock numblocks = 
    let clus = 
        let raw = getSpikesAndClusters hostname sfname stfname (spikelen*nchans) clusfname clustemp numperblock firstblock numblocks
        raw |> Array.Parallel.map (Array.map (fun ((st,spk),clu) -> 
                ((st|>float)/30000.,
                    spk|>Array.map (fun x -> 0.195*(x|>float))
                    |>scalespike nchans
                ),
                clu
            ))
    let shapes = clus |> Array.map (Array.map (fst>>snd)) |> Array.concat
    let clustertrees = 
        clus 
        |> Array.Parallel.mapi (fun blocknum xs -> 
            let xspikes = xs |>Array.mapi (fun i ((st,sp),clu) -> blocknum*numperblock + i,clu)
            getStableTree shapes xspikes|>NumberNodes
        )
    clustertrees,clus

let getStragglersFromChainNodes nchans numperblock shapes clustertrees chainNodes =
    let stragglers = 
        chainNodes |> Array.Parallel.map (fun (blocknum,xs) ->
            if Seq.length xs > 0 then
                let o = blocknum*numperblock
                let goodclusters =
                    xs |> Seq.map (fun clupath -> clupath,clupath |> getNode (Array.get clustertrees blocknum))
                    |> Seq.toArray
                goodclusters |> Array.map (fun (_,x) -> x.Items)
                |> Array.concat |> Set.ofArray |> Set.difference ([|o..o+numperblock-1|]|>Set.ofArray)
                |> Set.toArray
                |> Array.map (fun x -> goodclusters |> Array.map (fun (clupath,y) -> 
                    (x,(blocknum,clupath)),TILinkValue nchans 1 y.Shape (Array.get shapes x)) |> Array.maxBy snd)
                |> Array.filter (fun (s,x) -> x > 0.)
            else [||]
        ) |> Array.concat
    stragglers

let getStragglers nchans numperblock shapes clustertrees chains =
    getStragglersFromChainNodes nchans numperblock shapes clustertrees
        (chains |> List.concat |> Seq.groupBy fst |> Seq.sortBy fst |> Seq.toArray |> Array.map (fun (a,b) -> a,b|>Seq.map snd))

let saveChainSort hostname l2snumfname stfname firstblock numperblock outpath outprefix stragglers clustertrees chains = 
    let stragglermap = 
        stragglers |> Array.map fst |> Seq.groupBy snd |> Seq.map ((fun (k,v) -> 
            k,v|>Seq.map fst|>Seq.toArray)) |> Seq.toArray |> Map.ofArray
    let allspiketimes = getSpikeTimes hostname stfname 0UL -1
    chains |> List.iteri (fun i xs -> 
        xs |> List.map (fun (blocknum,clupath) -> 
            let curstragglers = 
                match stragglermap |> Map.tryFind (blocknum,clupath) with
                | Some x -> x
                | None -> [||]
            let allclustersinchain = 
                getNode (Array.get clustertrees blocknum) clupath 
                |> fun x -> Array.append curstragglers x.Items
                |> Array.map (fun i -> i+firstblock*numperblock)
            allclustersinchain)|>List.toArray|>Array.concat|>Array.sort
        |> fun allclustersinchain ->            
            writeArray hostname (sprintf "%s/%s%d.l2snums" outpath outprefix i) 0UL (allclustersinchain|>Array.map uint64) |> Async.RunSynchronously
    )

let calcAndSaveChainSort hostname l2sfname l2stfname l2snumfname stfname outpath outprefix spikelen nchans 
    l2clusfname l2clustemp maxtempnum numperblock firstblock numblocks =
    let clustertrees,clus = 
        loadClusterTree hostname l2sfname l2stfname spikelen nchans l2clusfname l2clustemp numperblock firstblock numblocks
    let clustertrees =
        let rec loop (Node(t,sts)) =                
            Node(t,sts|>Array.filter (fun (Node((_,clu),_)) -> clu.Temp <= maxtempnum)|> Array.map loop)
        clustertrees |> Array.Parallel.map (fun clustertree ->
            loop clustertree
        )
    let chains = chainClusterTrees l2clustemp nchans clustertrees
    let shapes = clus |> Array.map (Array.map (fst>>snd)) |> Array.concat
    let clustertrees = clustertrees|> Array.map (mapTree (fun (_,x) _ -> x))
    let stragglers = getStragglers nchans numperblock shapes clustertrees chains
    saveChainSort hostname l2snumfname stfname firstblock numperblock outpath outprefix stragglers clustertrees chains

let loadSpikeTimes,loadSpikeNums =
    let loadfun x ((t0,t1) as trange) hostname path (chgroup,cells) = 
        let fpath = sprintf "%s/%s" path
        cells |> Set.map (fun cell ->
            let fname s = fpath (sprintf "ChGroup_%s/AutoSort/%s.%s" chgroup cell s)
            let firstindx,lastindx = getSubSetindx hostname (fname "stimes") trange
            let n = lastindx-firstindx+1UL|>int   
            if (n>0) then getSpikeTimes hostname (fname x) firstindx n |> Array.map int64 |> Some else None
        ) |> Set.toArray |> Array.choose id |> Array.concat |> Array.sort |> Array.map int64
    loadfun "stime",loadfun "snums"

open Nessos.FsPickler
open System.IO
open System.Xml.Linq

let blockrange x = 
    Array.get x 0 |> fst,
    Array.get x (Array.length x-1)|>fst
let getOverlapDistL2 imax nchans chains1 chains2 =
    let inline getoverlap (t0,t1) (s0,s1) = max t0 s0, min t1 s1
    let (t0,t1),(s0,s1) = blockrange chains1,blockrange chains2
    let to0,to1 = getoverlap (t0,t1) (s0,s1)
    let filt g = g |> Array.choose (fun (t,x) -> if t >= to0 && t <= to1 then Some x else None)
    let getgitem i g = g |> Array.tryFind (fun (t,_) -> t = i) |> Option.map snd
    let getdist (n1,a1) (n2,a2) = min n1 n2, TAligned nchans imax a1 a2
    let dist =
        let xs = 
            seq{
                //compute distance for overlapping periods
                for a1,a2 in Array.zip (filt chains1) (filt chains2) do
                    yield getdist a1 a2
                //compute distance for adjacent periods
                match getgitem t1 chains1, getgitem (t1+1) chains2 with
                | Some a1, Some a2 -> yield getdist a1 a2
                | _ -> ()
                match getgitem (s1+1) chains1, getgitem s1 chains2 with
                | Some a1, Some a2 -> yield getdist a1 a2
                | _ -> ()
            } |> Seq.toArray
        if xs |> Array.length = 0 then None else
        let i = xs |> Seq.countBy (snd>>fst) |> Seq.maxBy snd |> fst
        let d = 
            if (xs |> Array.length = 1) then xs.[0] |> snd |> snd else
            let n = (xs |> Array.sumBy fst)*2/10
            let xs = xs |> Array.sortBy (snd>>snd)
            xs |> Array.scan (fun acc (m,(_,d)) -> acc+m) 0
            |> Array.findIndex (fun a -> a > n)
            |> fun j -> xs.[j-1]|>snd|>snd
        (i,d) |> Some
    dist

let rec mergeLoop maxd imax nchans allchains dists = 
    if Array.length dists = 0 then allchains else
    let (c1,c2),(i0,dist) = dists |> Array.maxBy (snd>>snd)
    printfn "%A %A %d %.4f" c1 c2 i0 dist
    if dist < maxd then allchains else
    let (chains1,x1),(chains2,x2) = 
        let f x = Map.find x allchains
        f c1, f c2
    let n1,n2 = Array.length x1,Array.length x2
    let i1,i2 = if n1 > n2 then 0,i0 else -i0,0
    let x1,x2 =
        let f i x = x |> Array.map (fun (t,(n,spk)) -> (t,(n,translate nchans imax spk i)))
        f i1 x1,f i2 x2 
    let (t0,t1),(s0,s1) = blockrange x1,blockrange x2     
    let merged = 
        [|for t in (min t0 s0)..(max t1 s1) -> 
            let xs = 
                let f x (t0,t1) = if t0 <= t  && t <= t1 then Array.get x (t-t0) |> snd |> Some else None
                [|f x1 (t0,t1);f x2 (s0,s1)|] |> Array.choose id
            t,
            (xs |> Array.sumBy fst,
             xs |> Array.map (fun (n,s) -> n|>float,s) |> avgShape)
        |]
    let newchains = Set.union c1 c2
    let allchains = 
        allchains |> Map.remove c1 |> Map.remove c2 
    let newnode = (Node((newchains,Some (i0,dist)),[|chains1;chains2|]),merged)
    let dists = 
        let newdists =
            [|for c,(_,x) in allchains |> Map.toSeq -> 
                getOverlapDistL2 imax nchans merged x |> Option.map (fun d ->
                (newchains,c),d)|] |> Array.choose id 
        Array.append (dists |> Array.filter (fun ((a,b),_) -> a <> c1 && a <> c2 && b <> c1 && b <> c2)) newdists
    let allchains = allchains |> Map.add newchains newnode
    mergeLoop maxd imax nchans allchains dists

let mergeChainsL2 maxd imax hostname path chgroup nchans =
    let spikelen = 64
    let fpath = sprintf @"%s/%s" path
    let autosorts = 
        let fsp = new FsPickler()
        use ms = new MemoryStream(getArray<byte> hostname (fpath "AutoSort.meta") 0UL -1)
        fsp.Deserialize<(string*((string*(uint64*uint64))[]))[]>(ms)
    let chfpath = sprintf "%s/%s" (fpath (sprintf "ChGroup_%s" chgroup))
    let chains =
        autosorts |> Array.find (fst>>((=)chgroup)) |> (fun (_,xs) -> 
            xs |> Array.map(fun (cell,_) ->
                let l2snumfname = 
                    chfpath (sprintf "AutoSort/%s.l2snums" cell)
                (Node((Set.singleton cell,None),[||]),
                    getSpikeTimes hostname l2snumfname 0UL -1)
            ))
    let allchains = 
        let sfname,stfname = chfpath "Level2/Spikes",chfpath "Level2/SpikeTimes"
        let snums = chains |> Array.map snd
        getSpikeSubsets hostname sfname stfname (spikelen*nchans) snums
        |> Array.zip snums |> Array.map (fun (xs,ys) ->
            ys |> Array.Parallel.map (fun (_,spk) -> spk|>Array.map (fun x -> 0.195*(x|>float))|>scalespike nchans)
            |> Array.zip xs
            |> Seq.groupBy (fun (snum,_) -> snum/1000UL|>int) |> Seq.toArray |> Array.sortBy fst |> Array.map (fun (block,xs) ->
                let xs = xs |> Seq.map (fun (_,x) -> 1.0,x) |> Seq.toArray
                block, (Array.length xs, xs |> avgShape)
            )
        ) |> Array.zip (chains|>Array.map fst)
    let chainssi = allchains |> Array.mapi (fun i (Node((x,_),_),y) -> i,(x,y))
    let dists = 
        seq {
            for i,(chains1,x1) in chainssi do
                for j,(chains2,x2) in chainssi.[i+1..] do
                    match getOverlapDistL2 imax nchans x1 x2 with
                    | None -> ()
                    | Some d -> yield (chains1,chains2), d
        } |> Seq.toArray       
    mergeLoop maxd imax nchans (Array.zip (chainssi|>Array.map (snd>>fst)) allchains|>Map.ofArray) dists
