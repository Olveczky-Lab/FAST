#if COMPILED
module HighResTimingRhythm
#else
#load "DynProgAlign.fs"
#load "HighResTimingHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif 

open System.IO
open System
open HighResTimingHelper
open Helper
open BehaviorHelper
open Nessos.FsPickler
open RP.Controls
open PlotHelper
open System.Windows.Media

//Rhythm TTL Channels in Raj's Boxes
//0 = clock with half period of 32768 samples
//1 = OpCon Trigger
//2 = Water Valve
//3 = ? 
//4 = Poke Sensor
//5 = Cherry Switch
//6 = High Speed Camera 1
//7 = High Speed Camera 2
//8 = Low Speed Camera

//Rhythm TTL Channels in Ashesh's Boxes
//0 = Poke Sensor
//1 = Cherry Switch
//2 = ?
//3 = OpCon Trigger
//4 = Water Valve
//5 = High Speed Camera 1
//6 = High Speed Camera 2

let hostname = Remote "140.247.178.16:5900"
let fnum = 635267094066850917L
let rat = "arches"
let rhythmleverch = 1
let fname = sprintf @"/root/data/rpoddar/%s/%d/TTLChanges/Ch_%d" rat fnum rhythmleverch

let plotttl =
    let x = getTTLChanges hostname fname 0UL -1
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    let inline f x = (x|>float)/30000.
    let l = 
        seq {
            for i in 0..(Array.length x)/2-1 do 
                let x1,x2 = f x.[i*2],f x.[i*2+1] 
                yield (x1,0.),(x1,1.),Colors.Black
                yield (x1,1.),(x2,1.),Colors.Black
                yield (x2,1.),(x2,0.),Colors.Black
        } |> Seq.toArray |> getLineList
    let xrange = Range(0.,f x.[Array.length x-1])
    let yrange = Range(-0.25,1.25)
    addSubPlot2 wnd xrange yrange xrange yrange [l] |> ignore
    addXAxis wnd xrange Colors.Black "" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore

let cstr = "Data Source=140.247.178.203,5900;Initial Catalog=OpConRPODDARTwoTap;User ID=rajeshpoddar;Password=htsn0f00d!"
//let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
let exptid = 25
let outfname = sprintf @"/root/data/rpoddar/%s/%d/Behavior/%dLeverPressOpCon.tmap" rat fnum exptid
let fpath = sprintf @"Z:\%s\%s" rat

let (hrtimes,ditimes),allcputimes = 
    let hrtimes,ditimes =
        use fs = new FileStream(fpath (sprintf @"%dOpConHRTimes" exptid),FileMode.Open)
        (FsPickler()).Deserialize<EventTime[]*DIEventTime[]>(fs)
    (hrtimes,ditimes),
    hrtimes |> Array.choose (fun x -> 
        if (x.EventType < 4uy) then Some (x.RCEID,x.CPUTime) else None
    )

let aligned = 
    aligntimes cstr exptid allcputimes hostname fname fnum

let nictrtime,samplenum = 
    let rceid,(samplenum,_) = aligned |> Map.toSeq |> Seq.head
    let nictrtime = hrtimes |> Array.find (fun x -> x.RCEID = rceid) |> fun x -> x.NICtrTime
    printfn "%A" (nictrtime,samplenum)
    nictrtime,samplenum

let save () =
    let r = aligntimes cstr exptid allcputimes hostname fname fnum
    use ms = new MemoryStream()
    FsPickler().Serialize(ms,r)
    writeArray hostname outfname 0UL (ms.ToArray()) |> Async.RunSynchronously

let tmap () = 
    use ms = new MemoryStream(getArray<byte> hostname outfname 0UL -1)
    FsPickler().Deserialize<Map<int,(int64*int64)>>(ms)
