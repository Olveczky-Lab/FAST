module SequentialClusterHelper

open Cluster
open RunCluster
open ClusterHelper
open ClusterTreeHelper

let inline getvar ref data =
    ref |> Array.mapi (fun i r -> data |> Array.sumBy (fun xs -> 
        let x = (Array.get xs i)-r
        x*x
    )) |> Array.sum
let mergeClusters xs =
    match xs with
    | [x] -> x
    | _ ->
        let totalcnt = xs |> List.sumBy (fst>>Array.length)
        let shape = xs |> List.toArray |> Array.map (fun (items,xs) -> Array.length items|>float,xs) |> avgShape
        xs|>List.map fst|>Array.concat,shape
let mergeBad minscore allspikes ((_,ref) as p) xs =
    let clusspikeshapes xs = 
        xs |> Array.map (fun i -> Array.get allspikes i)
    let getscore ref1 ref2 xs =
        let shapes = clusspikeshapes xs
        let nf = shapes.[0]|>Array.length|>float
        let var1 = getvar ref1 shapes
        let var2 = getvar ref2 shapes
        (var1-var2)/nf|>sqrt
    let scores = xs |> List.map (fun (items,shape) ->  getscore ref shape items)
    let good,bad = List.zip scores xs |> List.partition (fun (score,x) -> score > minscore)
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
                if List.length sts = 0 then [t.Items,t.Shape] else
                sts |> List.map loop |> List.concat |> mergeBad 20. allspikes (t.Items,t.Shape)
            let small,large = 
                loop x |>List.map(fun (items,shape) -> items |> Array.map (fun i -> clusindx.[i])) 
                |>List.partition (fun items -> 
                    Array.length items < minclussize
                )
            small |> List.toArray |> Array.concat |> Array.sort,
            large
    ) 
    |> fun x -> 
        x |> Array.map fst |> Array.concat,
        x |> Array.map snd |> Array.toList |> List.concat

