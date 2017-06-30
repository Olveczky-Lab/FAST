module TwoTapHelper

open BehaviorHelper

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
        Taps : (int*int) list
        Reward: Reward option
    }
    
let ParseIntoTrials parsedstates tapuprceids =
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
    let foldfun (trials,rewardfsms,curtapuprceids) parsedstates =
        match parsedstates with
        | _::taps -> 
            let taprceids,newtapuprceids = 
                getTapRCEIDs taps
                |> List.fold (fun (xs,ys) x ->
                    let rec getnexttapup ys =
                        match ys with
                        | head::tail -> if (head > x) then head,tail else getnexttapup tail
                        | _ -> failwith "Not Possible"
                    let tapup,newys = getnexttapup ys
                    (x,tapup)::xs,newys
                    ) 
                    ([],curtapuprceids)
                |> fun (x,y) -> x|>List.rev,y
            let reward,newrewardfsms =
                if (List.length taps > 1) then //At least two taps
                    match rewardfsms with
                    | head::tail -> 
                        getReward head,tail
                    | _ -> failwith "Not Possible"
                else None,rewardfsms
            {Taps = taprceids; Reward = reward}::trials,newrewardfsms,newtapuprceids
        | _ -> //Abandoned trial
            trials,rewardfsms,curtapuprceids
    mainfsms |> Seq.fold foldfun ([],rewardfsms,tapuprceids)
    |> (fun (trials,_,_) -> trials) |> List.rev

open System
let ParseSM exptid smid = 
    use db = db ()
    let rces =  
        query { 
            for rce in db.SMRatEvents(Nullable exptid,Nullable smid) do 
            sortBy rce.RatEventsID.Value
            select rce }
        |> Seq.toList
    let p,o =
        rces
        |> ParseIntoStates
    let tapdowns,tapups = 
        rces
        |>List.filter (fun rce -> rce.EventType.Value=4uy && (getintAttribute "ChannelNum" rce = 1))
        |> List.partition (fun rce ->
            rce.Details.Attribute(xn "NewState").Value|>bool.Parse)
    ParseIntoTrials p (tapups|>List.map(fun rce -> rce.RatEventsID.Value)),o,
    tapdowns|>List.map(fun rce -> rce.CPUTime.Value,rce.RatEventsID.Value)|>Map.ofList
