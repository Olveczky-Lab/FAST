#if COMPILED
module HighResBehavior
#else
#load "DynProgAlign.fs"
#load "HighResTimingHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif 

open BehaviorHelper
open TwoTapHelper
open HighResTimingHelper
open Nessos.FsPickler
open System.IO
open System
open Helper
open ClusterHelper
open System.Xml.Linq

let hostname = Remote "140.247.178.94:5900"
let fpath = sprintf @"/root/data/asheshdhawale/Data/Gorakh/%s"

let savesms () =
    //let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
    let cstr = "Data Source=140.247.178.203,5900;Initial Catalog=OpConRPODDARTwoTap;User ID=rajeshpoddar;Password=htsn0f00d!"
    let db2tap = db2tap(cstr,"TwoTapTrialAndErrorV2")
    let expts = [113]
    let sms = 
        seq {
            for exptid in expts do
                printfn "%d" exptid
                let clocks =
                    use fs = new MemoryStream(getArray<byte> hostname (fpath (sprintf @"%dOpConHRClocks" exptid)) 0UL -1)
                    (FsPickler()).Deserialize<((int64*int64)*int64[])[]>(fs)

                let hrtimes,ditimes = 
                    use fs = new MemoryStream(getArray<byte> hostname (fpath (sprintf @"%dOpConHRTimes" exptid)) 0UL -1)
                    FsPickler().Deserialize<EventTime[]*DIEventTime[]>(fs)

                let cputimes = ditimes |> Array.map (fun x -> x.EventTime.CPUTime)
                let nictrtimes = ditimes |> Array.map (fun x -> x.EventTime.NICtrTime)

                let sms =
                    db2tap.getTwoTapSMs exptid
                    |> List.choose (fun (smid,_) ->
                        printfn "%d" smid
                        db2tap.getSMCPUTime(exptid,smid) |> Option.map (fun (first,last) ->
                            let f = CPUTimeToSampleNum cputimes nictrtimes clocks
                            match f first,f last with
                            | Some (f1,t1), Some (f2,t2) when f1=f2 -> ((exptid,smid),(f1,(t1,t2)))|>Some
                            | _ -> None
                        )
                    ) |> List.toArray
                yield! sms
        } |> Seq.toArray |> Array.choose id
    use ms = new MemoryStream()
    FsPickler().Serialize(ms,sms)
    writeArray hostname (fpath (sprintf @"OpConHRTwoTapSMTimes")) 0UL (ms.ToArray()) |> Async.RunSynchronously

let sms = 
    use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConHRTwoTapSMTimes") 0UL -1)
    FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms)

let savegoodcells () =
    let goodcells = 
        sms |> Seq.groupBy (fun ((exptid,smid),(fnum,(t0,t1))) -> fnum) 
        |> Seq.toArray |> Array.map (fun (fnum,xs) -> 
            let nchansall =
                use ms = new MemoryStream(getArray<byte> hostname (fpath (sprintf @"%d/SnippeterSettings.xml" fnum)) 0UL -1)
                loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
                |> Array.map (fun (chgroup,chans) -> chgroup,Array.length chans) |> Map.ofArray
            let cells = 
                use fs = new MemoryStream(getArray<byte> hostname (fpath (sprintf "%d/MergedChains.meta" fnum)) 0UL -1)
                FsPickler().Deserialize<(string*((string*(uint64*uint64))[]))[]>(fs)        
            xs |> Seq.toArray |> Array.map (fun ((exptid,smid),(fnum,(t0,t1))) ->
                printfn "======%d,%d======" exptid smid
                (exptid,smid),
                cells |> Array.map (fun (chgroup,xs) -> 
                    let nchans = nchansall.[chgroup]
                    let spkindxs =
                        xs |> Array.choose (fun (cell,(s0,s1)) ->
                            if (s0|>int64) < t0 && (s1|>int64) > t1 then
                                let stfname,snumfname =
                                    let f x = fpath (sprintf "%d/ChGroup_%s/MergedChains/%s.%s" fnum chgroup cell x)
                                    f "stimes",f "snums"
                                let firstindx,lastindx = getSubSetindx hostname stfname (t0|>uint64,t1|>uint64)
                                let n = lastindx-firstindx+1UL|>int   
                                if n = 0 then None else
                                    let maxn = 1000
                                    printfn "%s %s %d" chgroup cell n
                                    getSpikeTimes hostname snumfname firstindx n |> fun xs ->
                                        xs |> Seq.pairwise |> Seq.iter (fun (a,b) -> if a > b then printfn "Oops %s" cell)
                                        if n > maxn then 
                                            Array.init maxn (fun i -> xs.[i*n/maxn])
                                        else xs
                                    |> fun x -> Some ((cell,n),x)
                            else None
                        )
                    let stfname,sfname =
                        let f x = fpath (sprintf "%d/ChGroup_%s/%s" fnum chgroup x)
                        f "SpikeTimes", f "Spikes"
                    chgroup,
                    getSpikeSubsets hostname sfname stfname (64*nchans) (spkindxs|>Array.map snd)
                    |> Array.map (Array.Parallel.map (fun (_,spk) -> 
                        Array.init nchans (fun ch -> spk.[31*nchans+ch]|>int|>abs) |> Array.max |> fun x -> (x|>float)*0.195)
                        >>Array.average
                        >>fun amp -> amp
                    ) |> Array.zip (spkindxs|>Array.map fst)
                )
            )
        ) |> Array.concat
    use ms = new MemoryStream()
    FsPickler().Serialize(ms,goodcells)
    writeArray hostname (fpath "sessionmergedchains") 0UL (ms.ToArray()) |> Async.RunSynchronously

let goodcells =     
    use ms = new MemoryStream(getArray<byte> hostname (fpath "sessionmergedchains") 0UL -1)
    FsPickler().Deserialize<((int * int) * (string * ((string * int) * float) []) []) []>(ms)