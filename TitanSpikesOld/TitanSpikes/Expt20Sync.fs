module Expt20Sync

open Microsoft.FSharp.Data.TypeProviders
open System.Xml.Linq

[<Literal>]
let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
type dbschema = SqlDataConnection<cstr>
let inline xn s = XName.Get(s)
type RatControlEvent = dbschema.ServiceTypes.SMRatEventsResult
let db = new dbschema.ServiceTypes.OpConASHESHDHAWALE1(cstr)

let exptid = 20
let numditoskip = 0

let printseq xs = xs |> Seq.iter (printfn "%A")

type RCEvent = {
    RatEventsID : int
    CPUTime : int64
    RCEventType: byte
    Details: XElement
}

type NIEvent = {
    EventTimesID : int
    NICtrTime : int64
    NIEventType : byte
}

let nievts = 
    let rawvals =
        query {
            for evt in db.EventTimes do
            where (evt.ExptID = exptid)
            sortBy (evt.EventTimesID)
            select {EventTimesID = evt.EventTimesID; NICtrTime = evt.NICtrTime-129158L; NIEventType = evt.EventType}
        }
    rawvals
    |> Seq.pairwise
    |> Seq.scan (fun (offset) (a,b) -> 
                    if (b.NICtrTime-a.NICtrTime < 0L) then (offset+4294967296L) else (offset)) 0L
    |> Seq.zip rawvals
    |> Seq.map (fun (a,b) -> {a with NICtrTime=a.NICtrTime+b})
    |> Seq.toArray

let goodnievts = 
    nievts |> Seq.scan (fun dinum evt -> if (evt.NIEventType = 4uy) then dinum+1 else dinum) 0 |> Seq.skip 1
    |> Seq.zip nievts
    |> Seq.choose (fun (evt,dinum) -> if (evt.NIEventType < 4uy) || (evt.NIEventType < 11uy && dinum > numditoskip) then Some evt else None)
    |> Seq.toArray

let rcevts =
    query {
        for evt in db.RatControlEvents do
        where (evt.ExptID = exptid && evt.EventType <= 4uy)
        sortBy (evt.RatEventsID)
        select {RatEventsID = evt.RatEventsID; CPUTime = (evt.CPUTime-4758L)*100L; RCEventType = evt.EventType; Details = evt.Details}
    }
    |> Seq.toArray

open System.Collections.Generic

type ContinuationBuilder() = 
    member this.Return(x) = (fun k -> k x) 
    member this.ReturnFrom(x) = x 
    member this.Bind(m,f) = (fun k -> m (fun a -> f a k)) 
    member this.Delay(f) = (fun k -> f () k) 
let K = new ContinuationBuilder()

let inline align maxoffset cost gapcostx gapcosty xs ys =
    let cache = Dictionary<_,_>()
    let nx = Array.length xs
    let ny = Array.length ys
    let rec aligncost ((ix,iy) as i) = K {
        match cache.TryGetValue(i) with
        | true, r -> return r
        | _ ->
            let! c = K {
                match (ix,iy) with
                | _ when (ix - iy) |> abs > maxoffset -> return None
                | _ when ix = nx -> return Some (ys.[iy..ny-1]|>Seq.sumBy gapcosty,([iy..ny-1]|>List.map (fun indx -> (None,Some ys.[indx]))))
                | _ when iy = ny -> return Some (xs.[ix..nx-1]|>Seq.sumBy gapcostx,([ix..nx-1]|>List.map (fun indx -> (Some xs.[indx],None))))
                | _ ->
                    let! c1tmp = aligncost (ix+1,iy+1)                    
                    let c1 = c1tmp |> Option.map (fun (cold,gold) -> cost xs.[ix] ys.[iy] + cold,(Some xs.[ix],Some ys.[iy])::gold)
                    let! c2tmp = aligncost (ix,iy+1)
                    let c2 = c2tmp |> Option.map (fun (cold,gold) -> gapcosty ys.[iy] + cold,((None,Some ys.[iy])::gold))
                    let! c3tmp = aligncost (ix+1,iy)
                    let c3 = c3tmp |> Option.map (fun (cold,gold) -> gapcostx xs.[ix] + cold,((Some xs.[ix],None)::gold))
                    let c = [c1;c2;c3] |> List.choose id
                    if List.isEmpty c then return None else
                    return Some (List.minBy fst c)
            }
            cache.Add(i,c)
            return c
    }
    aligncost (0,0) id

let costtypes,alignedtypes = 
    align 10
        (fun rce ni -> if rce.RCEventType = ni then 0. else 1.) 
        (fun rce -> 10.)
        (fun ni -> 10.)
        (rcevts)
        (goodnievts |> Array.map (fun x -> x.NIEventType))
    |> Option.get

let costtimes, alignedtimes =
    let diff xs = xs |> Seq.pairwise |> Seq.map (fun (a,b) -> b-a) |> Seq.toArray
    align 10
        (fun x y -> abs (x-y) |> float)
        (fun x -> 1000000.)
        (fun y -> 1000000.)
        (rcevts |> Array.map (fun rce -> rce.CPUTime) |> diff)
        (goodnievts |> Array.map (fun x -> x.NICtrTime) |> diff)
    |> Option.get |> fun (x,y) -> x,(Some 0L,Some 0L)::y

let cumsum xs =
    xs |> Seq.scan (fun (prevx,lastx) x -> 
            match x with
            | Some a -> (Some (a+lastx),a+lastx)
            | None -> (None,lastx)) (Some 0L,0L)
    |> Seq.map fst

let r =
    Seq.unfold (fun (xs,ys) ->
        match (xs,ys) with
        | hx::tx,hy::ty -> 
            match (hx|>fst,hy|>fst) with
            | Some _, Some _ | None,None -> Some ((hx,hy),(tx,ty))
            | Some _, None -> Some (((None,None),hy),(xs,ty))
            | None, Some _ -> Some ((hx,(None,None)),(tx,ys))
        | [],hy::ty -> Some (((None,None),hy),([],ty))
        | hx::tx,[] -> Some ((hx,(None,None)),(tx,[]))
        | [],[] -> None
    ) 
        (alignedtypes,alignedtimes) 

r
|> Seq.mapi (fun i x -> (i,x))
|> Seq.filter (fun (i,((rce,nitype),(cputime,nitime))) -> 
    match (rce,nitype,cputime,nitime) with
    | Some _,Some _,Some _,Some _ -> false
    | _ -> true
)
|> Seq.map fst
|> printseq

r
|> Seq.skip (200290 - 25)
|> Seq.take 50
|> Seq.iter (fun ((rce,nitype),(cputime,nitime)) -> 
    let inline opts opt = 
        match opt with
        | Some x -> sprintf "%d" x
        | None -> " "
    let rceid = rce |> Option.map (fun x -> x.RatEventsID)
    let rcevtype = rce |> Option.map (fun x -> x.RCEventType)
    printfn "%s\t(%s,%s)\t(%s,%s)" (rceid|>opts) (rcevtype|>opts) (nitype|>opts) (cputime|>opts) (nitime|>opts))


let timemm = 
    alignedtimes
    |> Seq.mapi (fun i x -> (i,x))
    |> Seq.choose (fun (i,(x,y)) -> 
                    match (x,y) with
                    | None, Some _ -> Some (i,+1)
                    | Some _, None -> Some (i,-1)
                    | _ -> None)

timemm |> Seq.map fst
|> Seq.skip 8
|> Seq.take 4
|> Seq.iter (fun i ->
    printfn "Mismatch at %d" i
    alignedtimes
    |> Seq.skip (i-25)
    |> Seq.take 50
    |> Seq.iter (fun (x,y) ->
        match (x,y) with
        | Some a, Some b -> printfn "%d,%d" a b
        | Some a, None -> printfn "%d, " a
        | None, Some b -> printfn " ,%d" b
        | _ -> failwith "Not Possible")
)

let typemm = 
    alignedtypes
    |> Seq.mapi (fun i x -> (i,x))
    |> Seq.choose (fun (i,(x,y)) -> 
                    match (x,y) with
                    | None, Some _ -> Some (i,+1)
                    | Some _, None -> Some (i,-1)
                    | _ -> None)

typemm |> Seq.map fst
|> Seq.skip 1
|> Seq.take 1
|> Seq.iter (fun i ->
    printfn "Mismatch at %d" i
    alignedtypes
    |> Seq.skip (i-25)
    |> Seq.take 50
    |> Seq.iter (fun (x,y) ->
        match (x,y) with
        | Some a, Some b -> printfn "%d,%d" a.RCEventType b
        | Some a, None -> printfn "%d, " a.RCEventType
        | None, Some b -> printfn " ,%d" b
        | _ -> failwith "Not Possible")
)

Seq.zip 
    (nievts |> Array.filter (fun x -> x.NIEventType <4uy)) 
    (rcevts |> Array.filter (fun x -> x.RCEventType <4uy)) 
|> Seq.mapi (fun i x -> (i,x))
|> Seq.filter (fun (i,(ni,rce)) -> (rce.RCEventType <> ni.NIEventType))
|> Seq.iter (fun (i,(ni,rce)) -> printfn "%d\t%d,%d" i ni.NIEventType rce.RCEventType)