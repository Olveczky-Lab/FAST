#if COMPILED
module BehaviorData
#else

#load "MathHelper.fs"
#load "Helper.fs"
#load "HDF5Helper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"

#load "BehaviorHelper.fs"
#load "EphysSync.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open System.Xml.Linq
open System
open BehaviorHelper
open Linq.NullableOperators
open System.Linq

let db = db ()

type RewardPulseTrain =
    {
        PokeRCEID: int
        PulseRCEID: int
        PulseWidth: int
        Frequency: int
        NumPulses: int
    }

type Reward =
    {
        ToneRCEID: int
        SampleNum: uint64
        PulseTrain:  RewardPulseTrain option
    }

type Trial =
    {
        Taps : int list
        Reward: Reward option
    }
    
let ParseIntoTrials parsedstates =
    let mainfsms = Map.find 0 parsedstates |> SplitList (getStateNum >> (=) 0)
    let rewardfsms = 
        match Map.tryFind 2 parsedstates with
        | None -> []
        | Some x -> x |> SplitList (getStateNum >> (=) 0)
    let getTapRCEID parsedstate =
        match parsedstate.Transition with
        | EventTrans rce -> rce.RatEventsID.Value
        | _ -> failwith "Not Possible"
    let getReward rewardfsm =
        match rewardfsm with
        | [_;s;_] when getStateNum s = 2 -> None //No Reward
        | _::s::t when getStateNum s = 1 ->
            let tonerceid,tonesamplenum = s.Tasks |> List.head 
                                          |> fun x -> 
                                                x.RatEventsID.Value,
                                                x.Details.Element(xn "AODetails").Attribute(xn "SampleNum")|>uint64
            let reward = 
                match t with
                | [s;_] -> //Reward Delivered
                    let pw,freq,num = s.Tasks |> List.head
                                            |> fun x -> 
                                                getintAttribute "PulseWidth" x,
                                                getintAttribute "Frequency" x,
                                                getintAttribute "NumPulses" x
                    let pokerceid = 
                        match s.Transition with
                        | EventTrans x -> x.RatEventsID.Value
                        | _ -> failwith "Not Possible"                    
                    Some {PokeRCEID = pokerceid; PulseRCEID = s.Tasks |> List.head |> fun x -> x.RatEventsID.Value; 
                          PulseWidth = pw; Frequency = freq; NumPulses = num}
                | [_] -> None
                | _ -> failwith "Not Possible"
            Some {ToneRCEID = tonerceid; SampleNum = tonesamplenum; PulseTrain = reward}
        | _ -> failwith "Not Possible"

    let getTapRCEIDs parsedstates =
        parsedstates |> List.filter (fun s -> getStateNum s <> 3) |> List.map getTapRCEID
    let foldfun (trials,rewardfsms) parsedstates =
        match parsedstates with
        | _::taps -> 
            let taprceids = getTapRCEIDs taps
            let reward,newrewardfsms =
                if (List.length taps > 1) then //At least two taps
                    match rewardfsms with
                    | head::tail -> 
                        getReward head,tail
                    | _ -> failwith "Not Possible"
                else None,rewardfsms
            {Taps = taprceids; Reward = reward}::trials,newrewardfsms
        | _ -> //Abandoned trial
            trials,rewardfsms
    mainfsms |> Seq.fold foldfun ([],rewardfsms)
    |> fst |> List.rev

let getExpts ratName =
    query {
        for expt in db.Expts do
        where (expt.Rats.RatName = ratName)
        select expt.ExptID
    }
    |> Seq.toList

let getSMs exptid stageName = 
    let stageNum =
        query {
            for stage in db.Stages do
            where (stage.StageLists.ExptID ?= exptid && stage.SMInstances.ParametrizedStateMachines.Name = stageName)
            select stage.Number
            take 1
        }
        |> Seq.toList
    match stageNum with
    | stageNum::_ ->
        query {
            for sm in db.StateMachines do
            where (sm.ExptID = exptid && sm.Type = 0uy && sm.Details = Nullable stageNum)
            sortBy sm.DefinitionID
            select (sm.DefinitionID,sm.Definition)
        }
        |> Seq.map (fun (smid,defn) -> smid,defn.Descendants(xn "Timing")
                                            |>Seq.head
                                            |>fun x -> 
                                                let f n (x:XElement) = x.Attribute(xn n)|>int
                                                f "lower" x,
                                                f "upper" x,
                                                f "target" x)
        |> Seq.toList
    | _ -> []

let getSMStartTime exptid smid =
    query {
        for sm in db.StateMachines do
        where (sm.ExptID = exptid && sm.DefinitionID = smid)
        select (sm.Expts.StartTime,sm.ActiveTime)
        take 1
    }
    |> Seq.head
    |> fun (t,dt) -> (t.Value)+TimeSpan.FromMilliseconds(dt.Value |> float)

let expts = [(1,3);(2,78)]

let getStateDurations exptid smid =
    let p,o =
        query { for rce in db.SMRatEvents(Nullable exptid,Nullable smid) do 
                sortBy rce.RatEventsID.Value
                select rce }
        |> Seq.toList
        |> ParseIntoStates
    Map.find 0 p
    |> Seq.pairwise
    |> Seq.map (fun (ps1,ps2) ->
            (getintAttribute "NewStateID" ps1.Entry,ps2.Entry.CPUTime.Value-ps1.Entry.CPUTime.Value))


let ipis2 = 
    [1;2]
    |> Seq.map (fun (exptid) ->
        getSMs exptid "TwoTapTrialAndErrorV2"
        |> Seq.map (fun (smid,(lower,upper,target)) ->
            //printfn "%d\t%d\t%d\t%d\t%d\t%A" exptid smid lower upper target (getSMStartTime exptid smid)
            let rces = 
                query { for rce in db.SMRatEvents(Nullable exptid,Nullable smid) do 
                        sortBy rce.RatEventsID.Value
                        select rce }
                |> Seq.toList
            let p,_ =
                rces
                |> ParseIntoStates            
            let cputimes = rces |> List.map (fun rce -> rce.RatEventsID.Value, rce.CPUTime.Value) |> Map.ofList
            let gt rceid = cputimes |> Map.find rceid
            p|>ParseIntoTrials
            |> List.choose (fun trial ->
                match trial.Taps with
                | [tap1;tap2] -> Some (((gt tap2)-(gt tap1),lower,upper))
                | _ -> None
            )
        )
        |> Seq.concat
    )
    |> Seq.concat
    |> Seq.toList


let ParseSM exptid smid = 
    let rces = 
        query { for rce in db.SMRatEvents(Nullable exptid,Nullable smid) do 
                sortBy rce.RatEventsID.Value
                select rce }
        |> Seq.toList
    let p,o =
        rces
        |> ParseIntoStates
    p|>ParseIntoTrials,o

let showTapCounts () = 
    [1;2]
    |> Seq.iter (fun (exptid) ->
        getSMs exptid "TwoTapTrialAndErrorV2"
        |> Seq.iter (fun (smid,(lower,upper,target)) ->
            //printfn "%d\t%d\t%d\t%d\t%d\t%A" exptid smid lower upper target (getSMStartTime exptid smid)
            let p,o = ParseSM exptid smid
            p
            |> Seq.countBy (fun trial -> (trial.Taps|>List.length))
            |> fun x ->
                let getcounts numtaps =
                    match x |> Seq.tryFind (fun (i,n) -> i=numtaps) with
                    | None -> 0
                    | Some (_,n) -> n
                let highcounts =
                    x |> Seq.filter (fun (i,n) -> i >=4)
                    |> Seq.sumBy snd
                printfn "%d\t%d\t%d\t%d\t%d\t%d" lower upper (getcounts 1) (getcounts 2) (getcounts 3) highcounts
        )
    )

getExpts "Bairagi"
|> Seq.iter (fun (exptid) ->
    getSMs exptid "TwoTapTrialAndErrorV2"
    |> Seq.iter (fun (smid,_) ->
        printfn "%d %d" exptid smid
        getStateDurations exptid smid
        |> Seq.choose (function
                        | (statenum,duration) when statenum = 1 || statenum = 3 -> duration |> Some
                        | _ -> None)
        |> Seq.countBy (fun x -> x > 1300L)
        |> Seq.sort
        |> Seq.iter (printfn "%A")
        )
    )

let nictrtimes = 
    expts
    |> Seq.map (fun (exptid,offset) ->
        exptid,getDIEventTimes exptid offset |> List.toArray)
    |> Map.ofSeq

let nieventtimes = nictrtimes |> Map.map (fun _ v -> v|> Array.map (fun x -> x.EventTime))

let gettime exptid =
        let times = Map.find exptid nieventtimes
        getDIEventTimeFromRCEID times>>(fun x -> x.NICtrTime)>>float>>(fun x -> x/100.0)

let all2tap =
    expts
    |> Seq.map (fun (exptid,offset) ->
        let gt = gettime exptid
        getSMs exptid "TwoTapTrialAndErrorV2"
        |> Seq.map (fun (smid,_) ->
            let p,o = ParseSM exptid smid
            p
            |> Seq.choose (function
                            | {Taps = tap1::tap2::_} ->
                                (gt tap2)-(gt tap1) |> Some
                            | _ -> None)
        )
        |>Seq.concat
    )
    |>Seq.concat
    |>Seq.toArray

let getunrewarded exptid smid =
    let rces = 
        query { for rce in db.SMRatEvents(Nullable exptid,Nullable smid) do 
                sortBy rce.RatEventsID.Value
                select rce }
        |> Seq.toList
    let p,_ =
        rces
        |> ParseIntoStates            
    let cputimes = rces |> List.map (fun rce -> rce.RatEventsID.Value, rce.CPUTime.Value) |> Map.ofList
    let gt rceid = cputimes |> Map.find rceid
    p|>ParseIntoTrials
    |> Seq.pairwise    
    |> Seq.choose (function
                    | ({Taps = tap1::tap2::rest; Reward = None} as trial1,trial2) -> 
                        let posttapstrial1 = (tap2::rest)
                        let trial2tap =
                            match trial2 with
                            | {Taps = tap3::_} -> [tap3]
                            | _ -> []
                        posttapstrial1@trial2tap |> Seq.map gt |> Seq.pairwise |> Seq.map (fun (a,b) -> b-a)
                        |> Seq.toList
                        |> fun x -> if (List.length x > 0) then Some x else None
                    | _ -> None)

let postunrewarded2tap1 = 
    [204;209]
    |> Seq.map (fun (exptid) ->
        getSMs exptid "TwoTapTrialAndErrorV2"
        |> Seq.map (fun (smid,_) ->
            getunrewarded exptid smid
        )
        |> Seq.concat
    )
    |> Seq.concat
    |> Seq.toList

open PlotHelper
open RP.Controls
open System.Windows.Media

let plotdata line ys =
    let wnd = new D3DPlot2DWindow()
    wnd.Show()

    let xrange = Range(0.0,Array.length (Array.get ys 0)|>float)
    addXAxis wnd xrange Colors.Black "Trial Number" |> ignore

    let yrange = Range(0.0,3000.0)
    addYAxis wnd yrange Colors.Black "Interval (ms)" |> ignore

    let toplot = 
        ys
        |> Array.map (fun y ->
            if line then
                getLine Colors.Black 0.0 1.0 y
            else
                getPoints 30.0f (Color.FromArgb(255uy,0uy,0uy,0uy)) (y |> Array.mapi (fun i x -> float i,x))
        )
    addSubPlot wnd xrange yrange 
        (toplot|>Seq.cast) |> ignore

let iqr xs = 
    xs |> Array.sort |> fun x -> 
                           let n = x.Length 
                           x.[n/4],x.[n/2],x.[3*n/4]
let intervals =
    Array.init 3600 (fun i -> all2tap.[i..i+49] |> iqr)

plotdata true 
    [|intervals|>Array.map (fun (x,_,_) -> x);intervals|>Array.map (fun (_,x,_) -> x);intervals|>Array.map (fun (_,_,x) -> x);|]
