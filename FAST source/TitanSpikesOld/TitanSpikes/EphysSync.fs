module EphysSync

open System
open BehaviorHelper

let repairephyssync xs =
    let maxq = 10
    let maxlookahead = 10
    let rec r lastxs ys xs lookahead =
        let last = List.head ys
        match xs with
        | x::tail ->
            match lookahead with
            | _ when lookahead > maxlookahead ->
                ys |> List.rev, lastxs
            | _ ->
                let q = (x-last)/109225L
                match q with
                | _ when q = 0L || x-last-(q*109225L) > q ->
                    if q > int64 maxq then ys |> List.rev, lastxs
                    else r lastxs ys tail (lookahead+1)
                | _ -> 
                    let offset = (x-last)%109225L
                    let newys = 
                        if offset > q then failwith (sprintf "q=%d" q)
                        List.init (q-1L|>int) (fun i -> if (i<(offset|>int)) then 1L else 0L)
                        |> List.scan (fun acc odd -> acc-(odd+109225L)) x
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
    | Some (file,clock) ->        
        let nclk = Array.length clock
        if (t < clock.[0] || t > clock.[nclk-1]) then None else
        let nearestindx = 
            match (Array.BinarySearch(clock,t)) with
            | x when x >= 0 -> x
            | x -> (~~~x)-1    
        let a = (1L<<<15)*(int64 (nearestindx+1))
        let b = 
            if (nearestindx = nclk-1) then 0L else
            let period = ((clock.[nearestindx+1]-clock.[nearestindx])|>float)
            let b = ((t-clock.[nearestindx])|>float)/period*((1L<<<15)|>float) |> int64
            b
        Some (file,(a+b))

let inline getSampleNumFromRCEID clocks nictrtimes rceid =
    getSampleNum clocks ((getDIEventTimeFromRCEID nictrtimes rceid).NICtrTime)

open System.Xml.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Collections
open Linq.NullableOperators
open System.Data.Linq.SqlClient
open System.IO
open BehaviorHelper

let getClocks exptid offset fnums =

    let nictrtimes = getDIEventTimes exptid offset

    let synctimes = nictrtimes |> List.filter (fun x -> x.Channel = 2) |> List.map (fun x -> x.EventTime.NICtrTime)
                    |> Seq.toList |> repairephyssync |> List.filter (fun xs -> List.tail xs |> List.isEmpty |> not)

    let exptstarttime,exptduration = getExptTimeExtent exptid                                                                            
    let clocks = 
        fnums
        |> Array.choose (fun fnum -> 
                        let tstart = (DateTime(fnum)-exptstarttime).TotalMilliseconds|>int64
                        if (tstart < 0L || tstart > exptduration) then None else 
                        let clk =
                            synctimes |> List.map (fun clk -> (clk, tstart*100L-List.head clk |> abs))
                            |> List.minBy snd
                            |>fst
                        Some (fnum,clk|>List.toArray))
        |> Array.sortBy (fun (fnum,clk) -> clk.[0])
    clocks, nictrtimes

    
open Helper
open PlotHelper
open RP.Controls
open System.Windows.Media

let plotdata exptduration clocks rawsynctimes =
    let inline getintervals xs =
        xs |> Seq.pairwise |> Seq.map (fun (a,b) -> b-a)

    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    wnd.Title <- "NiCtrTimes"
    let xrange = Range(0.0,(exptduration|>float)/1000.0)
    addTimeXAxis wnd xrange |> ignore
    let plotpoints color yrange xdata ydata =
        addSubPlot wnd xrange yrange (getPoints 5.0f color (Seq.zip (xdata|>Seq.map (fun x -> float x/100000.0)) (ydata|>Seq.map float)) |> Seq.singleton |> Seq.cast) |> ignore    
    let yrange = Range(0.0,1000000.0)
    addYAxis wnd yrange Colors.Green "Clock Intervals" |> ignore

    let yrangeunit = Range (0.0,1.0)
    clocks
    |> Array.iter (fun (_,xs:int64[]) -> 
                    addSubPlot wnd xrange yrangeunit (getLine Colors.Black (((xs.[0])|>float)/100000.0) 0.0 [|0.0;1.0|]|> Seq.singleton |> Seq.cast) |> ignore
                    let fxs = xs |> Seq.map float
                    plotpoints Colors.Green yrange xs (getintervals xs))
    let yrange = Range(0.0,1000000.0)
    addYAxis wnd yrange Colors.Blue "Raw Sync Times" |> ignore    
    plotpoints Colors.Blue yrange rawsynctimes (getintervals rawsynctimes)
