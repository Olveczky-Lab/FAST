module SegmentationFusion

open System.IO
open Cluster
open ZMQRPC

let assemblyFile = @"D:\Rajesh\Ephys\Data Analysis\FSharp\GLPK\GLPK\bin\x64\Release\GLPK.dll"
let typeName = "GLPK.GLPK"
let methodName = "BinProgMin"

let BinProgMin portnum (f:float[]) (A:int[]*int[]*float[]) (b:float[]) (d:bool) : bool[] =
    CallMethodInNewProcess assemblyFile typeName methodName portnum [|f:>obj;A:>obj;b:>obj;d:>obj|]

let overlapConstraints paths offset = seq{
        for path in (paths|>List.filter (fun xs -> List.length xs > 1)) do            
            yield (path |> List.map (fun x -> x+offset,1.),1.)
    }

let outConstraints links coffset loffset = seq{
    for x,ys in links|>Seq.groupBy (snd>>fst) do
        yield ((x+coffset,-1.)::[for (i,_) in ys -> loffset+i,1.],0.)
    }

let inConstraints links coffset loffset = seq{ 
    for y,xs in links|>Seq.groupBy (snd>>snd) do
        yield ((y+coffset,-1.)::[for (i,_) in xs -> loffset+i,1.],0.)
    }

let ClusterValuesAndConstraints ntemp offset (xs,xtree) =
    let costs = seq {
        yield! (xs |> Seq.map (ClusterValue ntemp))
    }
    let constraints = seq {
        yield! overlapConstraints xtree offset
    }
    costs|>Seq.toArray,constraints|>Seq.toArray

let LinkValuesAndConstraints xs ys coffset loffset =
    let nx = Array.length xs
    let ny = Array.length ys
    let links,costs =         
        seq {
        for i in 0..nx-1 do
            for j in 0..ny-1 do
                let v = LinkValue xs.[i] ys.[j]
                if v > 0. then yield (i,j),v
        } |> Seq.toArray |> fun x -> Array.mapi (fun i (x,_) -> i,x) x, Array.map snd x
    let constraints = seq {
        yield! outConstraints links coffset loffset
        yield! inConstraints links (nx+coffset) loffset
    }
    links|>Array.map snd,
    (costs,constraints|>Seq.toArray)

let LinkClusterConstraints links1 links2 nx coffset loffset = 
    seq{
        let f l o = l |> Array.mapi (fun i x -> (i+o,x))
        let l1 = f links1 loffset
        let l2 = f links2 (loffset+Array.length l1)
        for i in 0..nx-1 do
            yield ((coffset+i,1.)::
                [for (j,_) in l1 |> Array.filter (fun (_,(_,x)) -> x = i) -> (j,-1.0)]@
                [for (j,_) in l2 |> Array.filter (fun (_,(x,_)) -> x = i) -> (j,-1.0)]),0.
    } |> Seq.toArray

let SegFuseMap portnum clus validclussize =
    let nclust = Array.length clus 
    let ntemp = Array.get clus.[0] 0 |> snd |> Array.length
    let spikefeatures,curspikes =
        let spikelen,nchans = 
            let spk = clus.[0].[0]|>fst
            Array2D.length1 spk, Array2D.length2 spk
        let curspikes = clus |> Array.map (Array.map fst) |> Array.concat
        curspikes |> Array.map (fun i -> 
             Array.init (spikelen*nchans) (fun j -> 0.195*(spikesraw.[i,j%spikelen,j/spikelen]|>float))),
        curspikes |> Array.mapi (fun i x -> x,i) |> Map.ofArray

    let clusters =
        clus |> Array.map (fun curclus ->
            let validCluster clu = 
                let shape = avgShape clu
                Some {clu with Shape=shape}
            getStable validclussize validCluster curclus |> List.map NumberPaths
            |> List.fold (fun (offset,tree,xs) (nx,curtree,curxs) ->
                (offset+nx),
                (curtree|>List.map (List.map (fun x -> x+offset)))::tree,
                (curxs::xs)
            ) (0,[],[])
            |> fun (nx,tree,xs) -> (xs|>List.rev|>Array.concat),(tree|>List.rev|>List.concat)
        )
    let nc = clusters |> Array.sumBy (fst>>Array.length)
    let links,values,cs = 
        let c = 
            clusters
            |> Seq.scan (fun (_,offset) x ->
                Some (ClusterValuesAndConstraints ntemp  offset x),
                offset + Array.length (x|>fst)
            ) (None,0) |> Seq.choose fst
            |> Seq.toArray
        let l = 
            clusters
            |> Seq.pairwise
            |> Seq.scan (fun (coffset,loffset,_) ((a,_),(b,_)) -> 
                let links,(vs,cs) = LinkValuesAndConstraints a b coffset loffset
                coffset+Array.length a,loffset + Array.length links,
                (Some (links,(vs,cs)))
            ) (0,nc,None) |> Seq.choose (fun (_,_,a) -> a) |> Seq.toArray
        let links = l |> Array.map fst
//        let linkclustercs = 
//            Array.append [|[||]|] (Array.append links [|[||]|])
//            |> Seq.pairwise
//            |> Seq.zip clusters
//            |> Seq.scan (fun (coffset,loffset,_) ((a,_),(l1,l2)) -> 
//                let tmp = LinkClusterConstraints l1 l2 (Array.length a) coffset loffset
//                coffset+Array.length a,loffset + Array.length l1,
//                (Some tmp)
//            ) (0,nc,None) |> Seq.choose (fun (_,_,a) -> a) |> Seq.toArray
//            |> Array.concat
        links,
        Array.append (c|>Array.map fst) (l|>Array.map (snd>>fst)) |> Array.concat |> Array.map (fun x -> -x),
        Array.append (c|>Array.map snd) (l|>Array.map (snd>>snd)) |> Array.concat //|> Array.append linkclustercs

    let constraints = 
        cs
        |> Array.map fst
        |> Array.mapi (fun i c -> c |> List.map (fun (j,v) -> (i,j,v)) |> List.toArray)
        |> Array.concat
        |> fun x ->
            x |> Array.map (fun (x,_,_) -> x),
            x |> Array.map (fun (_,x,_) -> x),
            x |> Array.map (fun (_,_,x) -> x)
    let rhs = cs |> Array.map snd
    let bpsol =
        if (Array.length values > 0) then BinProgMin portnum values constraints rhs false
        else [||]
    let l = 
        links |> Seq.scan (fun (_,offset) ls ->
            Seq.zip bpsol.[offset..offset+(Array.length ls)-1] ls |> Seq.filter fst |> Seq.map snd |> Seq.toArray |> Some,
            offset+(Array.length ls)
        ) (None,nc) |> Seq.choose fst |> Seq.toArray
    clusters,l

let inline serializeStableClusters (bw:BinaryWriter) (xs,tree) links =
    let inline serializeInt (x:int) = bw.Write(x)
    let inline serializeFloat (x:float) = bw.Write(x)
    let inline serializeBool (x:bool) = bw.Write(x)
    let inline serializeSeq f xs =
        bw.Write(Seq.length xs)
        for x in xs do
            f x
    let inline serializeStableCluster clu = 
        bw.Write(clu.Temp)
        bw.Write(clu.Cluster)
        bw.Write(clu.Count)
        bw.Write(clu.Stability)
        serializeSeq serializeFloat clu.Shape
    
    serializeSeq serializeStableCluster xs
    serializeSeq (serializeSeq serializeInt) tree
    match links with
    | Some links ->
        serializeSeq (fun (a,b) -> 
            serializeInt a
            serializeInt b) links
    | _ -> ()

let inline deserializeStableClusters (br:BinaryReader) last =
    let inline deserializeInt () = br.ReadInt32()
    let inline deserializeFloat () = br.ReadDouble()
    let inline deserializeBool () = br.ReadBoolean()
    let inline deserializeSeq f () =
        Array.init (br.ReadInt32()) (fun _ -> f ())
    let inline deserializeStableCluster () = 
        makeStableCluster (br.ReadInt32()) (br.ReadUInt16()) (br.ReadInt32()) (br.ReadInt32())
            (deserializeSeq deserializeFloat ())
    deserializeSeq deserializeStableCluster (),
    deserializeSeq (deserializeSeq deserializeInt) (),
    if last then None else Some (deserializeSeq (fun _ -> deserializeInt (),deserializeInt ()) ())
    
let inline deserialize (br:BinaryReader) =
    let n = br.ReadInt32() 
    let tmp = seq{
        for i in 0..n-2 do
            let x,y,z = deserializeStableClusters br false
            yield (x,y),z
        let x,y,z = deserializeStableClusters br true
        yield (x,y),z
    } 
    tmp |> Seq.toArray |> (fun xs ->
        xs|>Array.map fst, xs|>Array.choose snd
    )

let SegFuseReduce clusters l (bw:BinaryWriter) prevSeg nNextSeg islast =
    let consensus a b =
        let r = Set.intersect (a|>Set.ofArray) (b|>Set.ofArray) |> Set.toArray
        r
    let curlinks,nextlinks =
        let n = Array.length l
        l.[0..n-nNextSeg-1],l.[n-nNextSeg..]
    let links = 
        match prevSeg with
        | None ->
            curlinks
        | Some prevlinks -> 
            let nprev = Array.length prevlinks
            Array.append (Array.map2 consensus prevlinks curlinks.[0..nprev-1]) curlinks.[nprev..]
    Seq.zip links clusters |> Seq.iter (fun (a,b) -> 
        serializeStableClusters bw b (Some a))
    if islast then 
        serializeStableClusters bw (Array.get clusters (Array.length clusters-1)) None
    nextlinks
    
let ExtendChain (a,curcs,cs) (ny,links:(int*int)[]) = 
    let extendChain j chain =
        (a,j)::chain
    let add xs x =
        (x|>List.rev)::xs
    let nx = Array.length curcs
    let newcurcs = Array.init ny (fun i -> [(a,i)])
    curcs
    |> Array.choose (fun (((_,j)::_) as c)->
        match (links|>Array.tryFind (fun (a,b) -> a=j)) with
        | Some (_,newj) ->          
            newcurcs.[newj] <- extendChain newj c
            None //Extend an existing chain
        | None ->
            Some c //Terminate existing chain
    ) 
    |> fun newcs ->
        a+1,
        newcurcs,
        newcs |> Array.toList |> List.fold add cs

let GetAllChains links =
    let nfirst = Array.get links 0 |> fst
    links
    |> Array.fold ExtendChain 
        (1,
        Array.init (nfirst) (fun j -> [0,j]),
        []
        ) 
    |> fun (_,lastSeg,cs) -> 
        lastSeg |> Array.map List.rev
        |> Array.toList |> List.append cs

let dispchain c =
    c 
    |> Seq.iter (fun (i,clu) -> 
        let ch,amp = clu.Shape |> Array.mapi (fun i x -> i,x) |> Array.maxBy (snd>>abs)
        printfn "%6d %4d % 2d %7.1f %3d %2.0f" 
            i clu.Count clu.Temp 
            amp ch
            (ClusterValue 10 clu*100.))

let dispblock (clusters,_) =
    clusters |> Seq.mapi (fun i c -> (i,c)) |> dispchain

let displinks xs ys =
    printf "%5s " ""
    for j in 0..(Array.length ys-1) do
        printf "%7d " j
    printfn ""
    for i in 0..(Array.length xs-1) do
        printf "%5d " i
        for y in ys do
            printf "%+7.3f " (LinkValue xs.[i] y)
        printfn ""

let dispblockandlinks i clusters =
    dispblock (Array.get clusters i)
    displinks(clusters.[i]|>fst) (clusters.[i+1]|>fst)
    dispblock (Array.get clusters (i+1))

let dispSeq xs f = 
    xs |> Seq.iter (fun c ->
    printfn ""
    f c)

