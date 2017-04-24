module SegFuseHelper

type Tree<'S> = Node of 'S*(Tree<'S> list)

let getClusterTree validclussize clus =
    let ntemp = Seq.head clus |> snd |> Array.length
    let rec rtree (temp,clu,count) (firsttemp,xs) = 
        let newxs = ((clu,count)::xs)
        let fatemap (temp,clu) =
            if (temp = ntemp-1) then [] else
                if (temp = -1) then clus else
                    clus |> Array.filter (fun (_,xs) -> Array.get xs temp = clu)
                |> Seq.countBy (fun (_,xs) -> Array.get xs (temp+1))
                |> Seq.sortBy fst
                |> Seq.toList
        match fatemap (temp,clu) with
        | [clunum,clucount] when clucount=count -> rtree (temp+1,clunum,clucount) (firsttemp,newxs)
        | (clunum,clucount)::fates -> 
            Node ((firsttemp,newxs|>List.rev),
                rtree (temp+1,clunum,clucount) (temp+1,[])::
                [for (clunum,clucount) in (fates|>Seq.filter (fun (_,cnt) -> validclussize (temp+1) cnt)) -> 
                    rtree (temp+1,clunum,clucount) (temp+1,[])])
        | [] -> Node ((firsttemp,newxs|>List.rev),[])
    rtree (-1,0us,Array.length clus) (-1,[])

let rec mapTree f tree = 
    match tree with 
    | Node (x,subtrees) -> 
        let s = subtrees|>List.map (mapTree f)
        Node (f x s,s)

let foldTree nodeF combineF leafV tree = 
    let rec Loop trees cont =   
        match trees with   
        | Node(x,sub)::tail -> 
            Loop sub (fun accSub -> 
              let resNode = nodeF x accSub
              Loop tail (fun accTail ->
                cont(combineF resNode accTail) ))
        | [] -> cont leafV
    Loop  [tree] (fun x -> x)     

let chooseTree f tree =
    let rec prune tree =
        match tree with 
        | Node (x,subtrees) ->
            let r = subtrees |> List.map prune |> List.concat
            match f x r with
            | Some y -> [Node(y,r)] 
            | None -> r
    prune tree

let filterTree f = chooseTree (fun x r -> if f x r then Some x else None)

let getClusSpikes clus tempnum clunum =
    clus
    |> Array.choose (fun (spike,xs:uint16[]) -> 
        if tempnum = -1 then Some spike else
        if xs.[tempnum]=clunum then Some spike else None)

let inline avgShape shapes = 
    let nspikes = Array.length shapes
    let nf = shapes.[0] |> Array.length
    Array.init nf (fun i -> 
        Array.init nspikes (fun j -> shapes.[j].[i]) |> Array.average
    )

type StableCluster = {
    Temp:int;
    Cluster:uint16;
    Count:int;
    Stability:int;
    Shape:float[]
}

let makeStableCluster tempnum clunum cnt stab shape =
    {Temp=tempnum;Cluster=clunum;Count=cnt;Shape=shape;Stability=stab}

let getStable validclussize clus = 
    getClusterTree validclussize clus 
    |> mapTree (fun (tempnum,xs) subtrees ->
        let (clunum,cnt) = xs |> List.head
        let totalcnt = 
            let curcnt = xs |> List.sumBy snd
            match subtrees with
            | [] -> 0
            | _ -> subtrees |> List.map (fun (Node(s,_)) -> s.Stability) |> List.max
            + curcnt
        makeStableCluster tempnum clunum cnt totalcnt [||]
    )
    
let inline vecdist s1 s2 =  
    let b = max (s1.Shape |> Array.max) (s2.Shape |> Array.max)
    let a = max (s1.Shape |> Array.min) (s2.Shape |> Array.min)
    let inline t x = (x-a)/(b-a)
    Array.map2 (fun xm ym ->
        let xmt,ymt = t xm, t ym
        (xmt-ymt)*(xmt-ymt)
    ) s1.Shape s2.Shape |> Array.average |> sqrt

let ClusterValue ntemp x =
    let s = (x.Stability|>float)/(x.Count*(ntemp+1)|>float)
    s*s*2.0*(x.Shape |> Array.maxBy abs |> abs)/200.

let LinkValue x y =
    let k,s,b = 0.03,0.005,0.02
    let a = exp (-((vecdist x y)-k)/s)
    a/(1.+a)-b

let NumberPaths tree = 
    tree
    |> foldTree (fun a (nb,b,xs) ->
        b 
        |>List.map (fun x -> nb::x)
        |>fun x -> nb+1,x,a::xs) (fun (nc,c,cxs) (nb,b,bxs) -> 
        match nb with
        | 0 -> c
        | _ -> List.append (c|>List.map (List.map ((+)nb))) b
        |> fun x -> (nb+nc,x,cxs@bxs)) (0,[[]],[])
    |> fun (cnt,paths,nodes) -> cnt,paths,nodes|>List.rev|>List.toArray

