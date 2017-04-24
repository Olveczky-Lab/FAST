#if COMPILED
module LoadTwoTapData
#else
#load "DynProgAlign.fs"
#load "HighResTimingHelper.fs"
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open Linq.NullableOperators
open System
open BehaviorHelper
open TwoTapHelper
open Nessos.FsPickler
open System.IO
open Helper
open HighResTimingHelper

//let cstr = 
//    "Data Source=140.247.178.203,5900;Initial Catalog=OpConRPODDARTwoTap;User ID=rajeshpoddar;Password=htsn0f00d!",
//    "TwoTapTrialAndErrorV2"
let cstr = 
    "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!",
    "TwoTapTrialAndErrorV2"
let hostname = Remote "140.247.178.94:5900"
let fpath = sprintf @"/root/data/asheshdhawale/Data/Gandhar/%s"
//let hostname = Remote "140.247.178.16:5900"
//let fpath = sprintf @"/root/data/rpoddar/arches/%s"

let sms = 
    use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConHRTwoTapSMTimes") 0UL -1)
    FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms)
    
let savedata = 
    let data = 
        sms |> Array.map (fun ((exptid,smid),_) -> 
            printfn "%d %d" exptid smid
            (exptid,smid),loaddata cstr exptid smid
        )
    use ms = new MemoryStream()
    FsPickler().Serialize(ms,data)
    writeArray hostname (fpath (sprintf @"OpConTwoTapTrials")) 0UL (ms.ToArray()) |> Async.RunSynchronously

let savehrdata =
    let trials =
        use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConTwoTapTrials") 0UL -1)
        FsPickler().Deserialize<((int * int) * (Trial [] * Map<int,int64>) option) []>(ms)
        |> Array.choose (fun (a,b) -> b |> Option.map (fun x -> a,x))
    let hrtimes =
        trials |> Seq.groupBy (fst>>fst) |> Seq.map (fun (exptid,xs) -> 
            let clocks =
                use fs = new MemoryStream(getArray<byte> hostname (fpath (sprintf @"%dOpConHRClocks" exptid)) 0UL -1)
                (FsPickler()).Deserialize<((int64*int64)*int64[])[]>(fs)
            let hrtimes = 
                use fs = new MemoryStream(getArray<byte> hostname (fpath (sprintf @"%dOpConHRTimes" exptid)) 0UL -1)
                FsPickler().Deserialize<EventTime[]*DIEventTime[]>(fs)
                |> fst |> Array.toList
            exptid,
            let rceids = 
                xs |> Seq.map (fun ((_,smid),(ts,_)) ->
                    printfn "%d %d" exptid smid
                    ts |> Array.map (fun trial -> seq{
                        yield! (trial.Taps |> List.map (fun (a,b) -> [a;b]) |> List.concat)
                        match trial.Reward with
                        | Some x -> 
                            match x.PulseTrain with
                            | Some x ->
                                yield x.PokeRCEID
                                yield x.PulseRCEID
                            | _ -> ()
                        | _ -> ()
                    }) |> Seq.concat        
                ) |> Seq.concat |> Seq.toList |> List.sort
            let rec loop map rceids (evtimes:EventTime list) =
                match rceids,evtimes with
                | hrce::trce,hevt::tevt when hrce = hevt.RCEID ->
                    loop (map |> Map.add hrce (getSampleNum clocks hevt.NICtrTime |> Option.get |> snd)) trce tevt
                | hrce::trce,hevt::tevt when hrce > hevt.RCEID ->
                    loop map rceids tevt
                | hrce::trce,hevt::tevt when hrce < hevt.RCEID ->
                    loop map trce evtimes
                | _ -> map
            loop Map.empty rceids hrtimes
        ) |> Map.ofSeq
    use ms = new MemoryStream()
    FsPickler().Serialize(ms,hrtimes)
    writeArray hostname (fpath (sprintf @"OpConTwoTapHRTimes")) 0UL (ms.ToArray()) |> Async.RunSynchronously
