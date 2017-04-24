module ClusterTreeHelper

open ClusterHelper

type Tree<'S> = Node of 'S*(Tree<'S> list)

let getClusterTree clus =
    let ntemp = Seq.head clus |> snd |> Array.length
    let fatemap (temp,(clu:('a*'b[])[])) =
        if (temp = ntemp-1) then [] else
        clu
        |> Seq.groupBy (fun (_,x) -> x.[temp+1]) |> Seq.map (snd>>Seq.toArray)
        |> Seq.toList
    let rec rtree (temp,clu) = 
        Node ((temp,clu|>Array.map fst), fatemap (temp,clu) |> List.map (fun xs -> rtree (temp+1,xs)))
    rtree (-1,clus)

let dispTree tree =
    let rec rdispTree padding tree = 
        match tree with 
        | Node (x,subtrees) -> 
            printfn "%s%A" padding x
            subtrees |> List.iter (rdispTree (sprintf " %s" padding))
    rdispTree "" tree
    
let rec mapTree f tree = 
    match tree with 
    | Node (x,subtrees) -> 
        let s = subtrees|>List.map (mapTree f)
        Node (f x s,s)

let collapseTree t =    
    let merge xs = 
        let good,bad = xs |> List.partition snd
        if (List.length bad = 0) then good else ((bad|>List.map fst|>List.concat),false)::good
    let rec collapse (Node((t,good),sts)) =
        if List.length sts = 0 then [(t,good)] else
            sts |> List.map collapse |> List.concat
            |> merge |> List.map (fun (x,curgood) -> (x,curgood || good))
    t |> mapTree (fun (x,good) _ -> [x],good) |> collapse

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

type StableCluster = {
    Temp:int;
    Items:int[]
    Stability:int;
    Shape:float[];
}

let stab x = (x.Stability|>float)/(x.Items|>Array.length|>float)

let makeStableCluster tempnum items stab shape =
    {Temp=tempnum;Items=items;Shape=shape;Stability=stab}

let getStableTree features clus = 
    getClusterTree clus
    |> fun x ->
        let rec loop (Node(t,sts)) =
            match sts|>List.map loop with
            | [Node(st,ssts)] -> Node(t,ssts)
            | newsts -> Node(t,newsts)
        loop x
    |> mapTree (fun (tempnum,xs) subtrees ->
        let cnt = xs |> Array.length
        let totalcnt,shape = 
            match subtrees with
            | [] -> cnt, xs |> Array.map (fun x -> (1.0,Array.get features x)) |> avgShape
            | _ -> 
                (subtrees |> List.map (fun (Node(s,_)) -> s.Stability) |> List.max |> fun x -> x+cnt),
                (subtrees |> List.map (fun (Node(s,_)) -> s.Items|>Array.length|>float,s.Shape) |> List.toArray |> avgShape)
        makeStableCluster tempnum xs totalcnt shape
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
    let s = (x.Stability|>float)/(x.Items|>Array.length|>float)
    s//s*s*2.0*(x.Shape |> Array.maxBy abs |> abs)/200.

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

