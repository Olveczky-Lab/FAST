#if COMPILED
module HighResTimingOpCon
#else
#load "DynProgAlign.fs"
#load "HighResTimingHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open BehaviorHelper
open HighResTimingHelper
open Nessos.FsPickler
open System.IO
open System
open System.Text.RegularExpressions
open System.Windows
open RP.Controls
open PlotHelper
open System.Windows.Media
open DynProgAlign

//let cstr = "Data Source=140.247.178.203,5900;Initial Catalog=OpConRPODDARTwoTap;User ID=rajeshpoddar;Password=htsn0f00d!"
let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
let db = db(cstr)
let exptid = 79
let fpath = sprintf @"Y:\Data\Gandhar\%s"

//If doesn't align then investigate
let inline plotalignment x1 x2 =     
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    let xrange = Range(0.0,Array.length x1 |> float)
    let l c o x = getLine c o 1. (x|>Array.map float)
    let yrange = Range(0.0,2000.)
    addSubPlot wnd xrange yrange [l Colors.Blue 0. x1;l Colors.Red 0. x2;] |> ignore
    addXAxis wnd xrange Colors.Black "" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore

let getnormts (hrtimes:EventTime[]) i1 i2 n =        
    let xs = hrtimes.[i1..i1+n-1] |> Array.map (fun t -> (t.CPUTime - hrtimes.[i1].CPUTime))
    let ys = hrtimes.[i2..i2+n-1] |> Array.map (fun t -> (t.NICtrTime - hrtimes.[i2].NICtrTime)/((hrclockspeed|>int64)/1000L))
    xs,ys

let hrtimes,ditimes =         
    let x = db.getHRTimes(exptid,1000000000)
    use fs = new FileStream(fpath (sprintf @"%dOpConHRTimes" exptid),FileMode.Create)
    FsPickler().Serialize(fs,x)
    x
//    use fs = new FileStream(fpath (sprintf @"%dOpConHRTimes" exptid),FileMode.Open)
//    FsPickler().Deserialize<EventTime[]*DIEventTime[]>(fs)

let checkhrtimes =
    let xs,ys = getnormts hrtimes 0 0 (Array.length hrtimes) 
    plotalignment (xs|>diff) (ys|>diff)

let correcthrtimes =
    let alignchunk i1 i2 n =
        let xs,ys = getnormts hrtimes i1 i2 n
        let stop a b = 
            let x1 = xs.[a]-ys.[b+3]|>float
            let x2 = xs.[a+3]-ys.[b]|>float
            let m = max xs.[a] ys.[b]|>float
            x1/m > 0.0001, x2/m < -0.0001
        let cost a b =
            let y = (xs.[a+1]-xs.[a])-(ys.[b+1]-ys.[b])|>abs
            Some y            
        align stop cost
            (fun _ -> 5000L|>Some) (fun _ -> 5000L|>Some) (Array.length xs-3) (Array.length ys-3)
    let n = hrtimes |> Array.length
    let rec alignall i1 i2 aligns =
        printfn "%d %d" i1 i2
        let maxnchunk = 10000
        let nchunk = maxnchunk |> min (n-i1) |> min (n-i2)
        let noverlap = 1000
        let x = 
            alignchunk i1 i2 nchunk |> Option.get |> snd |> List.toArray |> Array.map (fun (a,b) ->
                a |> Option.map ((+) i1),
                b |> Option.map ((+) i2)
            )
        if nchunk = maxnchunk then
            let i1,i2,alignment =
                let rec loop i = 
                    match x.[i] with
                    | Some a, Some b -> i,a,b
                    | _ -> loop (i-1)
                let i,i1,i2 = loop (x |> Array.findIndex (fun (a,_) -> a = Some (i1+nchunk-noverlap)))
                i1,i2,x.[0..i-1]
            alignall i1 i2 (Array.append aligns alignment)
        else (Array.append aligns x)
    let alignment = alignall 0 0 [||]
    alignment |> Array.iter (fun (a,b) -> 
        match (a,b) with
        | Some x, Some y -> ()
        | Some x, None -> printfn "%d XXXX" x
        | None, Some y -> printfn "XXXX %d" y
        | _ -> failwith "Not Possible"
    )

    let hrtimes = 
        alignment |> Array.choose (fun x ->
            match x with
            | (Some a,Some b) -> Some {hrtimes.[a] with NICtrTime=hrtimes.[b].NICtrTime}
            | _ -> None
        )
    let ditimes =
        let m = ditimes |> Array.map (fun x -> x.EventTime.RCEID,x) |> Map.ofArray
        hrtimes |> Array.filter (fun x -> x.EventType = 4uy)
        |> Array.map (fun x -> {Map.find x.RCEID m with EventTime=x})
    let xs,ys = getnormts hrtimes 0 0 (Array.length hrtimes) 
    plotalignment (xs|>diff) (ys|>diff)

let rawsynctimes = 
    ditimes |> Array.filter (fun x -> x.Channel = 2) |> Array.map (fun x -> x.EventTime.NICtrTime)
    |> diff
    |> Seq.countBy id
    |> Seq.sortBy (fun (a,_) -> a-109225L|>abs)
    |> Seq.toArray
printfn "%A" rawsynctimes
//Pick the lowest value that's close to 109225 from rawsynctimes
let interval = 109221L

let synctimes = 
    ditimes |> Array.filter (fun x -> x.Channel = 2) |> Array.map (fun x -> x.EventTime.NICtrTime)
    |> Seq.toList 
    |> repairephyssync interval 5. 65  |> List.filter (fun xs -> List.length xs > 1) |> List.map List.toArray |> List.toArray

let autoclocks = 
    let start,_ = db.getExptTimeExtent exptid
    let getSyncStart xs = Array.get xs 0 |> fun y -> start+TimeSpan.FromMilliseconds(y/((hrclockspeed|>int64)/1000L)|>float)
    Directory.GetDirectories(fpath "")
    |> Array.map (fun x -> DirectoryInfo(x).Name)
    |> Array.filter (fun x -> Regex.IsMatch(x,@"^\d+\Z"))
    |> Array.map (int64) |> Array.sort
    |> Array.map (fun x -> 
        x,
        synctimes
        |> Array.map (fun y -> y,(System.DateTime(x)-(y|>getSyncStart)).TotalSeconds|>abs)
        |> Array.minBy (snd)
    )
    |> Array.choose (fun (fnum,(clock,x)) -> if x < 300. then Some ((fnum,(1L<<<15)),clock) else None)

let checkclocks =
    autoclocks |> Array.map (fun ((f,_),xs) -> 
        f,System.DateTime(f),Array.length xs,FileInfo(fpath (sprintf "%d\TTLIns" f)).Length/2L/32768L
    )
    |> Seq.iter (printfn "%A")

let manualclocks = [||]
//    let fnum = 635267094066850917L
//    let nictrtime,samplenum = 105038L, 10078404004L
//    let curclock = synctimes.[0]
//    let i,nearestsynctime = 
//        curclock |> Array.mapi (fun i x -> i,x) |> Array.minBy (fun (_,x) -> x-nictrtime|>abs)
//    let nearestsamplenum = samplenum + ((nearestsynctime-nictrtime)*30L/100L)
//    let offset = nearestsamplenum - (1L<<<15)*(i|>int64)
//    [|(fnum,offset),curclock|]

let clocks = Array.append autoclocks manualclocks |> Array.sortBy fst

let saveclocks = 
    use fs = new FileStream(fpath (sprintf @"%dOpConHRClocks" exptid),FileMode.Create)
    FsPickler().Serialize(fs,clocks)
