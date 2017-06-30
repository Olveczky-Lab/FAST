module HighResTimingHelper

open DynProgAlign
open BehaviorHelper
open System
open Helper

let inline diff xs = xs |> Seq.pairwise |> Seq.map (fun (a,b) -> b - a) |> Seq.toArray

let repairephyssync period (maxjitter:float) maxq xs =
    let maxlookahead = 200000
    let rec r lastxs ys xs lookahead =
        let last = List.head ys
        match xs with
        | x::tail ->
            match lookahead with
            | _ when lookahead > maxlookahead ->
                ys |> List.rev, lastxs
            | _ ->
                let q = (x-last)/period
                let mj = maxjitter*(q|>float)|>int64
                match q with
                | _ when q = 0L || x-last-(q*period) > mj ->
                    if q > int64 maxq then ys |> List.rev, lastxs
                    else r lastxs ys tail (lookahead+1)
                | _ -> 
                    let offset = (x-last)%period
                    let newys = 
                        if offset > mj then failwith (sprintf "q=%d" q)
                        List.init (q-1L|>int) (fun i -> if (i<(offset|>int)) then 1L else 0L)
                        |> List.scan (fun acc odd -> acc-(odd+period)) x
                    r tail (newys@ys) tail 0
        | [] -> ys |> List.rev, lastxs
    let rec repairall remaining currepaired =
        match remaining with
        | head::tail -> 
            let repaired,newremaining = 
                r tail [head] tail 0
            repairall newremaining (repaired::currepaired)
        | [] -> currepaired |> List.rev
    repairall xs []

let inline getSampleNum clocks t =
    match (clocks |> Seq.tryFind (fun (_,clock) -> 
                                    let nclk = Array.length clock
                                    t >= clock.[0] && t <= clock.[nclk-1])) with
    | None -> None
    | Some ((file,offset),clock) ->        
        let nclk = Array.length clock
        if (t < clock.[0] || t > clock.[nclk-1]) then None else
        let nearestindx = 
            match (Array.BinarySearch(clock,t)) with
            | x when x >= 0 -> x
            | x -> (~~~x)-1    
        let a = offset+(1L<<<15)*(int64 nearestindx)
        let b = 
            if (nearestindx = nclk-1) then 0L else
            let period = ((clock.[nearestindx+1]-clock.[nearestindx])|>float)
            let b = ((t-clock.[nearestindx])|>float)/period*((1L<<<15)|>float) |> int64
            b
        Some (file,(a+b))

let inline alignxs (xs:int64[]) (ys:int64[]) =                 
    align 
        (fun _ _ -> false,false)
        (fun a b -> 
            xs.[a]-ys.[b]|>abs
            |>fun x -> if x < 100L || (x|>float)/(max xs.[a] ys.[b]|>float) < 0.01 then Some x else None
        ) 
        (fun _ -> 1000L|>Some) (fun _ -> 1000L|>Some) (Array.length xs) (Array.length ys)

let inline getAlignoffset maxi xs ys =
    let rec loop i =
        if i > maxi || i + Array.length ys > Array.length xs then None else
        let curxs = Array.sub xs i (Array.length ys)
        let r = alignxs curxs ys
        match r with
        | None -> loop (i+1)
        | Some (cost,_) when cost > 5000L -> loop (i+1)
        | Some (_,x) -> 
            i+(x|>List.findIndex (snd>>Option.isSome))|>Some
    loop 0

let updatetimes (cputimes,hrtimes) =
    let dcpu = cputimes |> Array.map snd |> diff
    let dhr = hrtimes |> Array.map (fun (x,_) -> x/30L) |> diff
    let n = [Array.length dcpu;Array.length dhr;20] |> List.min
    match getAlignoffset 200 dcpu dhr.[0..n-1] with
    | Some o -> cputimes.[o..Array.length cputimes-1],hrtimes
    | None -> 
        match getAlignoffset 200 dhr dcpu.[0..n-1] with
        | Some o -> cputimes,hrtimes.[o..Array.length hrtimes-1]
        | None -> failwith "Can't Align Beginning"

let aligntimes cstr exptid allcputimes hostname fname fnum = 
    let db = db(cstr)
    let exptstarttime,_ = db.getExptTimeExtent exptid
    let cputimes,hrtimes = 
        let inline cpu2hr t = 
            ((exptstarttime+TimeSpan.FromMilliseconds(t|>float))-DateTime(fnum)).TotalMilliseconds
            |> max 0. |> fun x -> x*30.0007235|>int64
        let allhrtimes = 
            let first,last = 
                getSubSetindx hostname fname 
                    ((cpu2hr ((Array.get allcputimes 0|>snd)-30000L*300L))|>uint64,
                    (cpu2hr ((allcputimes.[Array.length allcputimes-1]|>snd)+30000L*300L))|>uint64)
            if last < first then [||] else
            let all = getArray<uint64> hostname fname first ((last-first|>int)+1) |> Array.map int64
            Array.init ((Array.length all)/2) (fun i -> all.[i*2],all.[i*2+1])
        let firsthrtime = (allhrtimes.[0]|>fst)-(30000L*300L)
        let lasthrtime = (allhrtimes.[Array.length allhrtimes-1]|>fst)+(30000L*300L)
        allcputimes |> Array.filter (fun (_,t) -> 
            let hr = cpu2hr t
            hr >= firsthrtime && hr <= lasthrtime
        ),
        allhrtimes
    let cputimes,hrtimes =
        let cputimes,hrtimes = updatetimes (cputimes,hrtimes)
        let cputimes,hrtimes = updatetimes (cputimes|>Array.rev,hrtimes|>Array.rev)
        cputimes|>Array.rev,hrtimes|>Array.rev
    let f (xs:int64[]) = 
        xs |> Seq.pairwise |> Seq.fold (fun (n,(xs,ys,zs)) (a,b) ->
            n+1,
            if b-a > 2L then [],n::ys,(xs|>List.map (fun i -> i,n))@zs else n::xs,ys,zs
        ) (0,([],[],[]))
        |> fun (_,(_,good,bad)) -> 
            good |> List.rev |> List.toArray |> Array.map (fun i -> i,xs.[i+1]-xs.[i]),
            bad
        //xs |> diff |> Array.mapi (fun i x -> i,x) |> Array.filter (fun (_,x) -> x > 2L) 
    let dcpu,bad = cputimes |> Array.map snd |> f
    let dhr,_ = hrtimes |> Array.map (fun (x,_) -> x/30L) |> f
    let inline misaligned a b = (a-b|>abs)>2L && (a-b|>abs|>float)/(max a b|>float) > 0.01 
    let rec alignglitch ix iy a na b nb = seq {
        if ix+na >= Array.length dcpu || iy+nb >= Array.length dhr then () else
        if misaligned a b then
            if (a < b) then 
                yield! alignglitch ix iy (a+(dcpu.[ix+na]|>snd)) (na+1) b nb else 
                yield! alignglitch ix iy a na (b+(dhr.[iy+nb]|>snd)) (nb+1)
        else 
            if na > 1 || nb > 1 then 
                printfn "%A" ((dcpu.[ix..ix+na-1]|>Array.map snd),(dhr.[iy..iy+nb-1]|>Array.map snd))
                printfn "%d %d" ix iy
                printfn "%d %d" na nb
            yield! Seq.zip (dcpu.[ix..ix+na-1]|>Array.map fst) (dhr.[iy..iy+nb-1]|>Array.map fst)
            let ix = ix+na
            let iy = iy+nb
            yield! alignglitch ix iy (dcpu.[ix]|>snd) 1 (dhr.[iy]|>snd) 1
    }
    let aligned =
        alignglitch 0 0 (dcpu.[0]|>snd) 1 (dhr.[0]|>snd) 1 |> Seq.toArray
        |> Array.map (fun (ix,iy) -> cputimes.[ix]|>fst,hrtimes.[iy]) |> Map.ofSeq
    aligned |> fun x -> bad |> List.fold (fun x (i,j) -> 
        match Map.tryFind (cputimes.[j]|>fst) x with
        | None -> x
        | Some a -> Map.add (cputimes.[i]|>fst) a x) x

let inline tryInterpolate xs ys x =
    let nx = Array.length xs
    if (x < xs.[0]) || (x > xs.[nx - 1]) then None else
    match (System.Array.BinarySearch(xs,x)) with
    | i when i >= 0 -> Array.get ys i
    | i -> 
        let i0 = (~~~i)-1
        ys.[i0] + (x-xs.[i0])*(ys.[i0+1]-ys.[i0])/(xs.[i0+1]-xs.[i0])
    |> Some

let CPUTimeToSampleNum cputimes nictrtimes clocks (t:int64) =
    let nictrtime = tryInterpolate cputimes nictrtimes t 
    nictrtime |> Option.bind (getSampleNum clocks)
