module TwoTapHelper

open BehaviorHelper
open System.Xml.Linq
open System

type RewardPulseTrain =
    {
        PokeRCEID: int
        PulseRCEID: int
        PulseWidth: float
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
        Taps : (int*int) list
        Reward: Reward option
    }

type IPIType =
    | NewTrialAfterReward
    | NewTrialAfterSingleTap
    | NewTrialAfterFailure
    | Rewarded
    | UnRewarded of int

type IPI =
    {
        RCEID: int
        Duration: int64
        CPUTime: int64
        Type: IPIType
    }

let getTrialrceids trial =
    seq {
        for (td,tu) in trial.Taps do
            yield td
            yield tu
        match trial.Reward with
        | Some x -> 
            yield x.ToneRCEID
            match x.PulseTrain with
            | Some x -> 
                yield x.PokeRCEID
                yield x.PulseRCEID
            | _ -> ()
        | _ -> ()
    }
    
let ParseIntoTrials rces =
    let parsedstates = rces |> ParseIntoStates
    let mainfsms = Map.find 0 parsedstates |> SplitList (getStateNum >> (=) 0)
    let rewardfsms = 
        match Map.tryFind 2 parsedstates with
        | None -> 
            match Map.tryFind 1 parsedstates with
            | Some x -> x
            | _ -> []
        | Some x -> x
        |> SplitList (getStateNum >> (=) 0)
    let getTapRCEID parsedstate rces =
        match parsedstate.Transition with
        | EventTrans rce -> 
            let chnum,state = getDIDetails rce.Details
            let rceid = rce.RatEventsID.Value
            let rec findtapup rces =
                match rces with
                | (currce:RatControlEvent)::t ->
                    match currce.RatEventsID.Value,currce.EventType.Value with
                    | a,x when a > rceid && x=4uy -> 
                        match getDIDetails (currce.Details) with
                        | curchnum,curstate when curchnum = chnum && curstate <> state ->
                            currce.RatEventsID.Value,t
                        | _ -> findtapup t
                    | _ -> findtapup t
                | _ -> -1,[]
            let tapuprceid,rest = findtapup rces
            (rceid,tapuprceid),rest
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
                                                getfloatAttribute "PulseWidth" x,
                                                getintAttribute "Frequency" x,
                                                getintAttribute "NumPulses" x
                    let pokerceid = 
                        match s.Transition with
                        | EventTrans x -> x.RatEventsID.Value
                        | _ -> failwith "Not Possible"                    
                    Some {PokeRCEID = pokerceid; PulseRCEID = s.Tasks |> List.head |> fun x -> x.RatEventsID.Value; 
                          PulseWidth = pw; Frequency = freq; NumPulses = num}
                | _ -> None
            Some {ToneRCEID = tonerceid; SampleNum = tonesamplenum; PulseTrain = reward}
        | _ -> failwith "Not Possible"

    let getTapRCEIDs parsedstates rces =
        parsedstates |> List.fold (fun ((curtaps,rces) as state) s -> 
            if getStateNum s = 3 then state else
            let tap,rces = getTapRCEID s rces
            tap::curtaps,rces
        ) ([],rces)
        |> fun (taps,rces) -> taps|>List.rev,rces
    let foldfun (trials,(rewardfsms,rces)) parsedstates =
        match parsedstates with
        | _::taps -> 
            let taprceids,rces = getTapRCEIDs taps rces
            let reward,newrewardfsms =
                if (List.length taps > 1) then //At least two taps
                    match rewardfsms with
                    | head::tail -> 
                        getReward head,tail
                    | _ -> failwith "Not Possible"
                else None,rewardfsms
            {Taps = taprceids; Reward = reward}::trials,(newrewardfsms,rces)
        | _ -> //Abandoned trial
            trials,(rewardfsms,rces)
    mainfsms |> Seq.fold foldfun ([],(rewardfsms,rces))
    |> fst |> List.rev

type db2tap(cstr,stagename) =
    inherit db(cstr)    
    member this.getTwoTapSMs(exptid) = 
        this.getStageSMs(exptid,stagename)
        |> List.map (fun (smid,defn) -> 
            smid,defn.Descendants(xn "Timing")
            |>Seq.head
            |>fun x -> 
                let f n (x:XElement) = x.Attribute(xn n)|>int
                f "lower" x,
                f "upper" x,
                f "target" x)

    member this.get2TapTrials(exptid,smid) =
        let rces = this.getRCEs(exptid,smid)
        rces |> ParseIntoTrials |> List.toArray

    member this.getipis(exptid,smid) = 
        printfn "%d" smid
        let rces = this.getRCEs(exptid,smid)
        if List.length rces = 0 then [||] else
        let cputimes = rces |> List.map (fun rce -> rce.RatEventsID.Value, rce.CPUTime.Value) |> Map.ofList
        let gcput rceid = cputimes |> Map.find rceid
        let createIPI (tap1d,_) (tap2d,_) ipitype =
            let duration = ((gcput tap2d)-(gcput tap1d))
            let cpu = gcput tap2d
            {Duration=duration;CPUTime=cpu;Type=ipitype;RCEID = tap2d}
        let curipis =
            match (rces|>ParseIntoTrials) with
            | head::tail ->
                tail |> List.fold (fun (ipis,lasttrial) curtrial ->
                    match curtrial.Taps with
                    | [] -> ipis,lasttrial
                    | curtap1::rest ->
                        let ipis = 
                            match (lasttrial.Taps|>List.rev) with
                            | lasttap::rest -> 
                                match (rest,lasttrial.Reward) with
                                | [],_ -> createIPI lasttap curtap1 NewTrialAfterSingleTap::ipis
                                | _, Some {PulseTrain=Some _} -> createIPI lasttap curtap1 NewTrialAfterReward::ipis
                                | _ -> createIPI lasttap curtap1 NewTrialAfterFailure::ipis
                            | _ -> ipis
                        let ipis = 
                            match rest with
                            | curtap2::_ -> 
                                let ipis = 
                                    createIPI curtap1 curtap2 
                                        (match curtrial.Reward with
                                            | Some {PulseTrain=Some _} -> Rewarded 
                                            |_ -> UnRewarded 0)::ipis
                                rest |> Seq.pairwise |> Seq.fold (fun (ipis,n) (tap1,tap2) -> createIPI tap1 tap2 (UnRewarded n)::ipis,n+1) (ipis,1)
                                |> fst
                            | _ -> ipis 
                        ipis,curtrial
                ) ([],head)
                |> fst |> List.rev |> List.toArray
            | _ -> [||]
        curipis

let loaddata cstr exptid smid = 
    let getTrialsCPUTimes trials rcetimes =
        trials |> Seq.map getTrialrceids |> Seq.concat
        |> Seq.fold (fun map rceid -> Map.add rceid (Map.find rceid rcetimes) map) Map.empty
    let curdb = db2tap(cstr)
    let rces = curdb.getRCEs(exptid,smid)
    if List.length rces = 0 then None else
    let rcetimes = rces |> List.map (fun rce -> rce.RatEventsID.Value,rce.CPUTime.Value) |> Map.ofList
    let trials = ParseIntoTrials rces |> List.filter (fun t -> List.length t.Taps > 0) |> List.toArray
    let n = Array.length trials
    if Array.length trials > 0 then 
        let trials,rcetimes = 
            let (tapd,tapu)::rest = trials.[n-1].Taps|>List.rev
            if tapu = -1 then
                use dbc = dbschema.GetDataContext(cstr|>fst)
                let tapu = dbc.GetNextDIStateChange(Nullable exptid, Nullable tapd)
                if tapu.HasValue then
                    let tapu = tapu.Value
                    let tapucpu = 
                        query {
                            for evt in dbc.RatControlEvents do
                            where (evt.ExptID = exptid && evt.RatEventsID = tapu)
                            take 1
                            select (evt.CPUTime)
                        } |> Seq.head
                    trials.[n-1] <- {trials.[n-1] with Taps=(tapd,tapu)::rest|>List.rev}
                    trials,(rcetimes |> Map.add tapu tapucpu)
                else trials.[0..n-2],rcetimes 
            else (trials,rcetimes)
        Some (trials,getTrialsCPUTimes trials rcetimes) 
    else None

