#if COMPILED
module SaveHighResTimingOpCon
#else
#load "DynProgAlign.fs"
#load "HighResTimingHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open BehaviorHelper
open HighResTimingHelper
open System.IO
open Nessos.FsPickler

let inline aosnum2hrt s = s*(hrclockspeed|>int64)/(40000L)
//let cstr = "Data Source=140.247.178.203,5900;Initial Catalog=OpConRPODDARTwoTap;User ID=rajeshpoddar;Password=htsn0f00d!"
let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
let db = db(cstr)
let exptid = 113
let fpath = sprintf @"Y:\Data\Gorakh\%s"

let hrtimes,ditimes = 
    use fs = new FileStream(fpath (sprintf @"%dOpConHRTimes" exptid),FileMode.Open)
    FsPickler().Deserialize<EventTime[]*DIEventTime[]>(fs)

let aotimes = 
    let curaostart::rest = hrtimes |> Array.filter (fun x -> x.EventType = 1uy) |> Array.toList
    db.getAOTimes exptid
    |> Array.fold (fun (((curaostart,rest) as state),ys) (rceid,x,cput) -> 
        let curaostart,rest as state = 
            match rest with
            | h::t when h.RCEID < rceid -> h.NICtrTime,t
            | _ -> state
        state,{RCEID=rceid;NICtrTime=curaostart + (aosnum2hrt x);CPUTime=cput;EventType=9uy}::ys
    ) ((curaostart.NICtrTime,rest),[])
    |> snd
    |> List.rev |> List.toArray

let clocks =
    use fs = new FileStream(fpath (sprintf @"%dOpConHRClocks" exptid),FileMode.Open)
    FsPickler().Deserialize<((int64*int64)*int64[])[]>(fs)

let saveAlignedTimes =
    hrtimes |> Array.append aotimes |> Array.map (fun x -> (x.RCEID,x.EventType),x.NICtrTime|>getSampleNum clocks)
    |> Array.choose (fun (rceid,x) ->
        match x with
        | Some (f,t) -> Some (rceid,f,t)
        | _ -> None
    )
    |> Seq.groupBy (fun (_,f,_) -> f)
    |> Seq.toArray
    |> Array.map (fun (f,xs) -> f,xs|>Seq.toArray|>Array.map (fun ((rceid,_),_,b) -> rceid,b) |> Map.ofArray)
    |> Array.iter (fun (f,map) -> 
        printfn "%d %d" f map.Count
        use fs = new FileStream(fpath (sprintf @"%d\Behavior\%dAlignedTimesOpCon.tmap" f exptid),FileMode.Create)
        FsPickler().Serialize(fs,map)
        use fs = new FileStream(fpath (sprintf @"%d\Behavior\%dAlignedTimesOpCon.tmap.txt" f exptid),FileMode.Create)
        use sw = new StreamWriter(fs)
        map |> Map.toArray |> Array.sortBy fst |> Array.iter (fun (rceid,t) ->
            sw.Write(rceid)
            sw.Write(' ')
            sw.Write(t)
            sw.WriteLine()
        )
    )
