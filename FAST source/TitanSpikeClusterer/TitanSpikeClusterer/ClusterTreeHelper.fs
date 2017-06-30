module ClusterTreeHelper

open ClusterHelper

type Tree<'S> = Node of 'S*(Tree<'S>[])

let getClusterTree clus =
    let ntemp = Seq.head clus |> snd |> Array.length
    let fatemap (temp,(clu:('a*'b[])[])) =
        if (temp = ntemp-1) then [||] else
        clu
        |> Seq.groupBy (fun (_,x) -> x.[temp+1]) |> Seq.map (snd>>Seq.toArray)
        |> Seq.toArray
    let rec rtree (temp,clu) = 
        Node ((temp,clu|>Array.map fst), fatemap (temp,clu) |> Array.map (fun xs -> rtree (temp+1,xs)))
    rtree (-1,clus)

let dispTree tree =
    let rec rdispTree padding tree = 
        match tree with 
        | Node (x,subtrees) -> 
            printfn "%s%A" padding x
            subtrees |> Array.iter (rdispTree (sprintf " %s" padding))
    rdispTree "" tree
    
let rec mapTree f tree = 
    match tree with 
    | Node (x,subtrees) -> 
        let s = subtrees|>Array.map (mapTree f)
        Node (f x s,s)

let collapseTree t =    
    let merge xs = 
        let good,bad = xs |> List.partition snd
        if (List.length bad = 0) then good else ((bad|>List.map fst|>List.concat),false)::good
    let rec collapse (Node((t,good),sts)) =
        if Array.length sts = 0 then [(t,good)] else
            sts |> Array.map collapse |> List.concat
            |> merge |> List.map (fun (x,curgood) -> (x,curgood || good))
    t |> mapTree (fun (x,good) _ -> [x],good) |> collapse

let foldTree nodeF combineF leafV tree = 
    let rec Loop trees cont =   
        match trees with   
        | [||] -> cont leafV
        | _ -> 
            let (Node(x,sub)),tail = trees.[0],trees.[1..] 
            Loop sub (fun accSub -> 
              let resNode = nodeF x accSub
              Loop tail (fun accTail ->
                cont(combineF resNode accTail) ))
    Loop  [|tree|] (fun x -> x)     

let chooseTree f tree =
    let rec prune tree =
        match tree with 
        | Node (x,subtrees) ->
            let r = subtrees |> Array.map prune |> Array.concat
            match f x r with
            | Some y -> [|Node(y,r)|] 
            | None -> r
    prune tree

let filterTree f = chooseTree (fun x r -> if f x r then Some x else None)

type StableCluster = {
    Temp:int;
    Items:int[]
    Stability:int;
    Shape:float[];
    Var:float;
}

let stab x = (x.Stability|>float)/(x.Items|>Array.length|>float)

let getClusAmp nchans clu = Array.init nchans (fun i -> clu.Shape.[31*nchans+i]|>abs) |> Array.max
    
let makeStableCluster tempnum items stab shape =
    {Temp=tempnum;Items=items;Shape=shape;Stability=stab;Var=0.}

let getStableTree features clus = 
    getClusterTree clus
    |> fun x ->
        let rec loop (Node(t,sts)) =
            match sts|>Array.map loop with
            | [|Node(st,ssts)|] -> Node(t,ssts)
            | newsts -> Node(t,newsts)
        loop x
    |> mapTree (fun (tempnum,xs) subtrees ->
        let cnt = xs |> Array.length
        let totalcnt,shape = 
            match subtrees with
            | [||] -> cnt, xs |> Array.map (fun x -> (1.0,Array.get features x)) |> avgShape
            | _ -> 
                (subtrees |> Array.map (fun (Node(s,_)) -> s.Stability) |> Array.max |> fun x -> x+cnt),
                (subtrees |> Array.map (fun (Node(s,_)) -> s.Items|>Array.length|>float,s.Shape) |> avgShape)
        makeStableCluster tempnum xs totalcnt shape
    )
    
let inline vecdist s1 s2 =  
    let b = max (s1 |> Array.max) (s2 |> Array.max)
    let a = max (s1 |> Array.min) (s2 |> Array.min)
    let inline t x = (x-a)/(b-a)
    Array.map2 (fun xm ym ->
        let xmt,ymt = t xm, t ym
        (xmt-ymt)*(xmt-ymt)
    ) s1 s2 |> Array.average |> sqrt

let ClusterValue ntemp x =
    let s = (x.Stability|>float)/(x.Items|>Array.length|>float)
    s*s*2.0*(x.Shape |> Array.maxBy abs |> abs)/200.

let LinkValue x y =
    let k,s,b = 0.03,0.005,0.02
    let a = exp (-((vecdist x y)-k)/s)
    a/(1.+a)-b

let translate nchans imax (s:float[]) i =
    if abs i > imax then failwith "i > imax"
    let spikelen = (Array.length s)/nchans
    let news = Array.zeroCreate (spikelen*nchans)
    for ch in 0..nchans-1 do
        for j in 0..spikelen-2*imax-1 do
            news.[(j+imax)*nchans+ch] <- s.[(j+imax+i)*nchans+ch]
    news
let TAligned nchans imax x y =
    let shift = translate nchans imax
    [|-imax..imax|]|>Array.Parallel.map (fun i -> 
        let s1,s2 = shift x 0,shift y i
        i,LinkValue s1 s2
    )|>Array.maxBy snd

let TILinkValue nchans imax x y = TAligned nchans imax x y |> snd

let NumberNodes tree =
    let rec loop i (Node(x,sts)) =
        Node(([i],x),sts|>Array.mapi (fun j st -> loop j st) |> Array.map (mapTree (fun (is,x) _ -> i::is,x)))
    loop 0 tree
    |> mapTree (fun (is,x) _ -> List.tail is,x)

let NumberPaths tree = 
    tree
    |> foldTree 
        (fun a (nb,b,xs) ->
            b 
            |>List.map (fun x -> nb::x)
            |>fun x -> nb+1,x,a::xs
        ) 
        (fun (nc,c,cxs) (nb,b,bxs) -> 
            match nb with
            | 0 -> c
            | _ -> List.append (c|>List.map (List.map ((+)nb))) b
            |> fun x -> (nb+nc,x,cxs@bxs)
        ) 
        (0,[[]],[])
    |> fun (cnt,paths,nodes) -> 
        cnt,paths,nodes|>List.rev|>List.toArray

let rec setTree (Node(x,sts)) path newt =
    match path with 
    | [] -> newt
    | i::tail -> 
        sts.[i] <- setTree sts.[i] tail newt
        Node(x,sts)

let rec getTree ((Node(_,sts)) as tree) path =
    match path with 
    | [] -> tree
    | i::tail -> 
        getTree sts.[i] tail

let getNode tree path = getTree tree path |> (fun (Node(x,_)) -> x)