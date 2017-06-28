module SequentialClusterHelper

open Cluster
open RunCluster
open ClusterHelper
open ClusterTreeHelper
open Helper
open System.IO
open Nessos.FsPickler

let inline getvar (ref:float[]) data =
    ref |> Array.mapi (fun i r -> data |> Array.sumBy (fun xs -> 
        let x = ((Array.get xs i)-r)
        x*x
    )) |> Array.sum

let getscore var allspikes ref1 ref2 xs =
    let clusspikeshapes xs = 
        xs |> Array.map (fun i -> Array.get allspikes i)
    let shapes = clusspikeshapes xs
    let nf = shapes.[0]|>Array.length|>float
    let var1 = var ref1 shapes
    let var2 = var ref2 shapes
    (var1-var2)/nf|>sqrt

let mergeClusters xs =
    match xs with
    | [x] -> x
    | _ ->
        let totalcnt = xs |> List.sumBy (fst>>Array.length)
        let shape = xs |> List.toArray |> Array.map (fun (items,xs) -> Array.length items|>float,xs) |> avgShape
        xs|>List.map fst|>Array.concat,shape
let mergeBad scorefun minscore allspikes ((_,ref) as p) xs =
    let getscore = scorefun allspikes
    let scores = xs |> Array.map (fun (items,shape) ->  getscore ref shape items)
    let good,bad = Array.zip scores xs |> Array.toList |> List.partition (fun (score,x) -> score > minscore)
    let good = good |> List.map snd
    if (List.length good = 0) then [p]
    else
        let badgood =
            bad |> List.map (fun (score,((items,shape) as b)) -> 
                let i,minscore = good |> List.mapi (fun i (_,ref) -> i,getscore ref shape items) |> List.minBy snd
                (if minscore < score then i else -1),b
            )
            |> fun x i -> x |>List.choose (fun (j,b) -> if i=j then Some b else None)
        let newgood =
            good |> List.mapi (fun i x -> 
                x::(badgood i)|>mergeClusters
            )
        let badbad = badgood -1
        if List.length badbad = 0 then newgood else (badbad|>mergeClusters)::newgood            
let loadSmallAndLargeClusters minclussize hostname sfname stfname clusfname numperblock ntemp spikelen nchans firstblock numblocks =
    let clusindx = getClustersIndx hostname (sprintf "%s.indx" clusfname) numperblock firstblock numblocks
    let clus = 
        let clusraw = getClusters hostname clusfname ntemp numperblock firstblock numblocks
        let spikes = getSpikeSubsets hostname sfname stfname (spikelen*nchans) [|clusindx|]
        Array.Parallel.init numblocks (fun i ->
            Array.init numperblock (fun j ->
                let m = i*numperblock + j
                spikes.[0].[m]|>snd|>Array.map (fun x -> 0.195*(x|>float))|>scalespike nchans,
                clusraw.[m,0..]
            )
        )
    let allspikes = 
        clus |> Array.map (Array.map fst) |> Array.concat
    clus 
    |> Array.Parallel.mapi (fun blocknum xs -> 
        let xspikes = xs |>Array.mapi (fun i (_,clu) -> blocknum*numperblock + i,clu)
        getStableTree allspikes xspikes
        |> fun (Node(t,_) as x) ->
            let rec loop (Node(t,sts)) =
                if Array.length sts = 0 then [|t.Items,t.Shape|] else
                sts |> Array.map loop |> Array.concat
                |> mergeBad (getscore getvar) 20. allspikes (t.Items,t.Shape)
                |> List.toArray
            let small,large = 
                loop x |>Array.map(fun (items,shape) -> items |> Array.map (fun i -> clusindx.[i]))
                |>Array.partition (fun items -> 
                    Array.length items < minclussize
                )
            small |> Array.concat |> Array.sort,
            large |> Array.map Array.sort
    ) 
    |> fun x -> 
        x |> Array.map fst |> Array.concat,
        x |> Array.map snd

let inline updateClusShape maxindx curoffset (curspikes:int16[,]) n (curshape,spikenums) =
    let cur,next = spikenums |> Array.partition (fun a -> a < maxindx)
    cur |> Array.fold (fun curshape spikenum -> 
        curshape |> Array.mapi (fun i x -> x+(curspikes.[spikenum-curoffset|>int,i]|>float)/n)
    ) curshape,
    next

let processBlockLevel maxindx curoffset (curspikes:int16[,]) (prevdone,prevclus) =
    prevclus
    |> Array.Parallel.map (fun ((clu,n),(curshape,spikenums)) ->
        (clu,n),updateClusShape maxindx curoffset (curspikes:int16[,]) n (curshape,spikenums)            
    )
    |> Array.fold (fun (curdone,curclus) (((clu,_),(shape,xs)) as cur) -> 
        if Array.length xs = 0 then (clu,(shape|>Array.map int16))::curdone,curclus
        else curdone,cur::curclus
    ) (prevdone,[]) |> fun (a,b) -> a,b|>List.toArray

let rec processCurData minclussize ((numblocks,reader) as blocklevels) maxindx curoffset (curspikes:int16[,]) (blocknum,x) =
    let prevdone,prevclus = x
    let nf = Array2D.length2 curspikes
    let nperread = 10
    let newblocknum,x =
        if blocknum < numblocks then 
            let numread = min nperread (numblocks-blocknum)
            let largeclus = 
                reader blocknum numread
                |>Array.concat
                |>Array.filter (fun xs -> Array.length xs >= minclussize)
            let allclus = 
                Array.append prevclus (largeclus |> Array.map (fun xs ->
                    let n = Array.length xs
                    let subxs = 
                        if n > minclussize then 
                            Array.init minclussize (fun i -> xs.[i*n/minclussize])
                        else xs
                    ((xs,xs.[n/2]),Array.length subxs|>float),(Array.zeroCreate<float> nf,subxs)    
                ))
            blocknum+numread,(prevdone,allclus)
        else blocknum,x
    let (newdone,newclus) as y = processBlockLevel maxindx curoffset (curspikes:int16[,]) x
    if Array.length newclus = 0 && newblocknum>blocknum then
        processCurData minclussize blocklevels maxindx curoffset (curspikes:int16[,]) (newblocknum,y)
    else (newblocknum,y)

type Agent<'T> = MailboxProcessor<'T>
type AgentMessage<'a,'b> =
    | Data of 'a
    | End of AsyncReplyChannel<'b>

let processlevel2 hostname sfname l2sfname l2stfname l2cfname l2clusfname firstspike nspikes blocklevels 
        minclussize minclusamp numperblock ntemp spikelen nchans =
    let fsp = new FsPickler()
    let numperread = 1000000UL
    let nf = spikelen*nchans
    let nlevels = Array.length blocklevels
    let blocklevels = blocklevels |> Array.map (fun fname -> getArrayReader<uint64[][]> hostname fname)
    blocklevels |> Array.map fst |> printfn "%A"
    let agent = 
        Agent.Start(fun inbox ->
            let rec loop ((n,byteoffset,levels) as state) = async {
                let! msg = inbox.Receive()
                match msg with
                | Data (curoffset,curspikes) ->
                    printfn "Received %d" curoffset
                    let maxindx = curoffset + (Array2D.length1 curspikes|>uint64)
                    let updatedlevels = levels |> Array.mapi (fun i x -> 
                        processCurData minclussize blocklevels.[i] maxindx curoffset (curspikes:int16[,]) x)
                    let minx = 
                        updatedlevels |> Array.map (snd>>snd>>(Array.map (fst>>fst>>snd))) |> Array.concat |> (fun xs -> 
                            match Array.length xs with
                            | 0 -> maxindx
                            | _ -> xs |> Array.min
                        )
                    let newlevels = 
                        updatedlevels |> Array.map (fun (b,(xs,ys)) -> 
                            let xs1,xs2 = 
                                xs |> List.partition (fun ((_,t),_) -> t < minx)
                            xs1,(b,(xs2,ys))
                        ) 
                    let towrite = 
                        newlevels |> Array.map (fst>>List.toArray) |> Array.concat |> Array.sortBy (fst>>snd)
                        |> Array.filter (fun (_,shape) -> 
                            shape.[31*nchans..32*nchans-1]|>Array.map abs|>Array.max|>float|>fun x -> x*0.195>minclusamp
                        )
                    writeArray hostname l2sfname n (Array2D.init (Array.length towrite) nf (fun i j -> (towrite.[i]|>snd).[j])) |> Async.RunSynchronously
                    writeArray hostname l2stfname n (towrite |> Array.map (fst>>snd)) |> Async.RunSynchronously
                    use ms = new MemoryStream()
                    for (c,_),_ in towrite do
                        fsp.Serialize(ms,c)|>ignore
                    let bytes = ms.ToArray()
                    writeArray hostname l2cfname byteoffset bytes |> Async.RunSynchronously
                    return! loop (n+(Array.length towrite|>uint64),byteoffset+(Array.length bytes|>uint64),newlevels |> Array.map snd)                    
                | End reply -> reply.Reply(state)
            }
            loop (0UL,0UL, Array.init nlevels (fun _ -> 0,([],[||])))
        )
    ([firstspike..numperread..firstspike+nspikes-1UL]
    |> List.map (fun x -> x,min numperread (firstspike+nspikes-x)))
    |> List.iter (fun (curoffset,numtoread) ->
        printfn "%d" curoffset 
        let curspikes = getArray2D<int16> nf hostname sfname curoffset (numtoread|>int)            
        agent.Post(Data (curoffset,curspikes))
    )
    agent.PostAndReply(fun reply -> End reply) |> printfn "%A"


