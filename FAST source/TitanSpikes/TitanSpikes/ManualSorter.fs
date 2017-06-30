#if COMPILED
module ManualSorter
#else
#load "DynProgAlign.fs"
#load "HighResTimingHelper.fs"
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open Helper
open System.IO
open System.Xml.Linq
open Nessos.FsPickler
open AutoSortHelper
open System
open BehaviorHelper
open TwoTapHelper
open HighResTimingHelper

let minclussize = 15
let minoverlap = 15
let spikelen = 64
let imax = 5
let maxd = 0.
let hostname = Remote "140.247.178.94:5900"
let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
let exptid = 79
let db2tap = db2tap(cstr,"TwoTapTrialAndErrorV2")
let localpath = sprintf @"Y:\Data\Gandhar\%s"
let serverpath = sprintf @"/root/data/asheshdhawale/Data/Gandhar/%s"

let sms =
    let clocks =
        use fs = new FileStream(localpath (sprintf @"%dOpConHRClocks" exptid),FileMode.Open)
        (FsPickler()).Deserialize<((int64*int64)*int64[])[]>(fs)

    let ditimes = 
        use fs = new FileStream(localpath (sprintf @"%dOpConHRTimes" exptid),FileMode.Open)
        (FsPickler()).Deserialize<DIEventTime[]>(fs)

    let cputimes = ditimes |> Array.map (fun x -> x.EventTime.CPUTime)
    let nictrtimes = ditimes |> Array.map (fun x -> x.EventTime.NICtrTime)

    db2tap.getTwoTapSMs exptid
    |> List.choose (fun (smid,_) ->
        let first,last = db2tap.getSMCPUTime(exptid,smid)
        let f = CPUTimeToSampleNum cputimes nictrtimes clocks
        match f first,f last with
        | Some (f1,t1), Some (f2,t2) when f1=f2 -> (smid,(f1,(t1-30000L*300L,t2+30000L*300L)))|>Some
        | _ -> None
    )
    |> Seq.groupBy (snd>>fst)
    |> Seq.toArray 
    |> Array.map (fun (fnum,xs) -> fnum,xs|>Seq.map (fun (smid,(_,trange)) -> smid,trange) |> Seq.toArray)
    |> Map.ofArray

sms |> Map.iter (fun fnum sms ->
    let fnumserverpath = serverpath (sprintf "%d" fnum)
    let fnumlocalpath x = localpath (sprintf "%d\%s" fnum x)
    let esets =
        use ms = new MemoryStream(getArray<byte> hostname (sprintf "%s/SnippeterSettings.xml" fnumserverpath) 0UL -1)
        loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))

    let mergers =
        esets |> Array.map (fun (chgroup,chans) ->
            let nchans = Array.length chans
            let x = 
                printfn "Starting ChGroup %s" chgroup
                sms |> Array.map (fun (smid,(t0,t1)) ->
                    printfn "Starting %d" smid
                    let merger = getMerger hostname fnumserverpath spikelen minclussize minoverlap imax maxd (t0|>uint64,t1|>uint64) chgroup nchans
                    merger)
                |> Array.concat
            chgroup,x
            )
        |> Map.ofArray

    use ms = new MemoryStream()    
    mergers |> fun x -> FsPickler().Serialize(ms,x)
    writeArray Local (fnumlocalpath "sessionmergers") 0UL (ms.ToArray()) |> Async.RunSynchronously
)

sms |> Map.iter (fun fnum sms ->
    let fnumserverpath = serverpath (sprintf "%d" fnum)
    let fnumlocalpath x = localpath (sprintf "%d\%s" fnum x)
    let taptimes = 
        use ms = new MemoryStream(getBytes hostname (sprintf @"%s/Behavior/%dLeverPressOpCon.tmap" fnumserverpath exptid) 0UL -1 |> Async.RunSynchronously)
        (new FsPickler()).Deserialize<Map<int,(int64*int64)>>(ms)


    let getGoodSpikeTimes ((t0,t1) as trange) = 
        let alltaps = taptimes |> Map.toArray |> Array.map (snd>>fst) |> Array.filter (fun x -> x >= (t0|>int64) && x <= (t1|>int64))
        let spiketimes = 
            getMergedCells trange hostname fnumserverpath
            |> Array.map (fun x -> x,loadSpikeTimes trange hostname fnumserverpath x)
            |> Array.sortBy (snd>>Array.length) |> Array.rev

        let rec coincidences n (xs:int64 list) (ys:int64 list) =
            match xs,ys with
            | hx::tx,hy::ty when (hx-hy)|>abs <= 5L -> coincidences (n+1) tx ty
            | hx::tx,hy::ty when hx > hy -> coincidences n xs ty
            | hx::tx,hy::ty when hx < hy -> coincidences n tx ys
            | _ -> n

        let goodspiketimes =
            let x = 
                spiketimes
                |> Array.filter (fun (_,sts) ->
                    let numtapswithspikes = 
                        alltaps |> Array.Parallel.choose (fun tt ->
                            sts |> Array.tryFind (fun st -> (st-tt)|>abs < 5L*30000L)
                        ) |> Array.length
                    numtapswithspikes > (Array.length alltaps)*9/10
                )
            let ncells = Array.length x
            let cs =
                Array.Parallel.init (ncells*ncells) (fun indx ->
                    let f i  = x.[i]|>snd|>Array.toList
                    let i,j = indx/ncells,indx%ncells
                    if i>=j then (i,j),0. else
                    let xi,xj = f i,f j
                    let n = coincidences 0 xi xj
                    (i,j),(n|>float)/(min (List.length xi) (List.length xj) |> float)
                )
                |> Array.choose (fun ((i,j),c) -> if c > 0.5 then Some (Set.ofList [i;j]) else None)
            mergeSets cs [|0..ncells-1|]
            |> List.map (fun set -> set |> Set.map (fun i -> x.[i]) |> Set.toArray |> Array.sortBy (fst>>fst>>int) |> Seq.head)
            |> List.toArray
        goodspiketimes

    let goodcells =
        Array.map (fun (smid,(t0,t1)) ->
            printfn "%d" smid
            let sts = getGoodSpikeTimes (t0|>uint64,t1|>uint64)
            printfn "%d" (Array.length sts)
            smid,sts |> Array.map fst
        ) sms
    let fsp = new FsPickler()
    use ms = new MemoryStream()
    fsp.Serialize(ms,goodcells)
    writeArray Local (fnumlocalpath "sessiongoodcells") 0UL (ms.ToArray()) |> Async.RunSynchronously
)
