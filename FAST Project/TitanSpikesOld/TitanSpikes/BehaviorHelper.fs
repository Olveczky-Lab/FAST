module BehaviorHelper

open Microsoft.FSharp.Data.TypeProviders
open System.Xml.Linq

[<Literal>]
let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
type dbschema = SqlDataConnection<cstr>
let inline xn s = XName.Get(s)
type RatControlEvent = dbschema.ServiceTypes.SMRatEventsResult
let db () = new dbschema.ServiceTypes.OpConASHESHDHAWALE1(cstr)

let dbc = db ()
let evts =
    query {
        for evt in dbc.EventTimes do
        where (evt.EventType = 1uy && evt.ExptID = 2)
        select evt.NICtrTime
    }

type ParsedState =
    { Entry : RatControlEvent
      Transition : TransitionType
      Tasks : RatControlEvent list}
and  TransitionType =
     | EventTrans of RatControlEvent
     | StateTrans of ParsedState
     | Initial

let getintAttribute name (rce:RatControlEvent) =
    rce.Details.Attribute(xn name)|>int

let ParseIntoStates rces =
    let (|StateEntry|EntryTask|Event|Other|) (rce:RatControlEvent) = 
        match rce.EventType.Value with
        | 7uy -> StateEntry ((getintAttribute "SubFSMID" rce),
                             let a = rce.Details.Attribute(xn "CausalSubFSMID")
                             if (a = null) then None else Some (int a))
        | 2uy | 3uy | 8uy | 9uy -> EntryTask
        | 4uy | 5uy | 6uy | 10uy -> Event
        | _ -> Other
    
    let addstate i state m =
        match Map.tryFind i m with
        | Some x -> m |> Map.add i (state::x)
        | None -> m |> Map.add i [state]
    let updatestate i state m =
        match Map.find i m with
        | head::tail -> m |> Map.add i (state::tail)
        | _ -> failwith "Not Possible"
    let foldfun (curfsm, orphanevents, parsed) x =
        match x with
        | StateEntry (s,c) ->
            let t,o =  match c with
                       | None ->  
                            match orphanevents with
                            | head::tail -> EventTrans head,tail
                            | _ -> failwith "Not Possible"
                       | Some i -> StateTrans (parsed |> Map.find i |> List.head), 
                                              orphanevents            
            let newstate = {Entry = x; Transition = t; Tasks = []} 
            (s,o,parsed|>addstate s newstate)
        | Event -> 
            (curfsm, x::orphanevents, parsed)
        | EntryTask ->
            let curstate = parsed|>Map.find curfsm|>List.head
            let newstate = {curstate with Tasks = x::curstate.Tasks}
            (curfsm, orphanevents, parsed |> updatestate curfsm newstate)
        | _ -> //ignore
            (curfsm, orphanevents, parsed)
    match rces with 
    | head::tail ->
        let initstate = {Entry = head; Transition = Initial; Tasks = []}        
        let _,o,p = tail 
                    |> Seq.fold foldfun 
                        (0,[],Map.empty |> addstate 0 initstate)
        p|>Map.map (fun _ l -> List.rev l) ,o|>List.rev
    | _ -> failwith "Empty RCEs"

let getStateNum parsedstate = 
    getintAttribute "NewStateID" parsedstate.Entry

let SplitList delimeter list =
    let foldFunc i acc =
        if acc = [] then
            [ [ i ] ]
        elif delimeter (List.head (List.head acc)) then
            [ i ] :: acc
        else
            (i :: (List.head acc)) :: (List.tail acc)
    List.foldBack foldFunc list []

type EventTime = 
    {
        NICtrTime: int64
        RCEID: int
        CPUTime: int64
    }

type DIEventTime = 
    {
        EventTime: EventTime
        Channel: int
        NewState: bool
    }

let getNICtrtime exptid offset eventype =
    use db = db ()
    let rawvals = 
        query {
            for rce in db.EventTimes do
            where (rce.ExptID = exptid && rce.EventType = eventype && rce.EventTimesID >= offset)
            sortBy rce.EventTimesID
            select (rce.NICtrTime)
        }
    rawvals
    |> Seq.pairwise
    |> Seq.scan (fun (offset) (a,b) -> 
                    if (b-a < 0L) then (offset+4294967296L) else (offset)) 0L
    |> Seq.zip rawvals
    |> Seq.map (fun (a,b) -> a+b)
    |> Seq.toArray

let getEventTime exptid offset evtype =
    let nictrtimes = getNICtrtime exptid offset evtype
    use db = db ()
    let rces = 
        query {
            for rce in db.RatControlEvents do
            where (rce.ExptID = exptid && rce.EventType = evtype)
            sortBy rce.RatEventsID
            select (rce.RatEventsID,rce.CPUTime)
        }
    Seq.zip nictrtimes rces
    |> Seq.pairwise
    |> Seq.scan (fun offset ((nictr1,(_,cpu1)),(nictr2,(_,cpu2))) ->
        let numperiods = ((cpu2-cpu1)|>float)/42949672.96 |> floor |> int64
        if numperiods > 0L then (offset+4294967296L*numperiods) else offset
    ) 0L
    |> Seq.zip nictrtimes
    |> Seq.map (fun (a,b) -> a+b)
    |> Seq.zip rces
    |> Seq.map (fun ((a,b),t) -> {NICtrTime=t;RCEID=a;CPUTime=b})
    |> Seq.toList

let getDIEventTimes exptid offset =
    let nictrtimes = getNICtrtime exptid offset 4uy

    use db = db ()
    let rces = 
        query {
            for rce in db.RatControlEvents do
            where (rce.ExptID = exptid && rce.EventType = 4uy)
            sortBy rce.RatEventsID
            select (rce.RatEventsID,rce.Details,rce.CPUTime)
        }
        |> Seq.map (fun (x,y,z) -> (x,y.Attribute(xn "ChannelNum")|>int,z,y.Attribute(xn "NewState").Value|>bool.Parse))
    Seq.zip nictrtimes rces
    |> Seq.map (fun (t,(a,b,c,d)) -> {EventTime={NICtrTime=t;RCEID=a;CPUTime=c};Channel=b;NewState=d})
    |> Seq.toList

let getExptTimeExtent exptid =
    use db = db ()
    let exptstarttime = 
        query {
            for expt in db.Expts do
            where (expt.ExptID = exptid)
            select (expt.StartTime)
            take 1}
        |> Seq.head |> fun x -> x.Value

    let exptduration =
        query {
            for rce in db.RatControlEvents do
            where (rce.ExptID = exptid)
            maxBy (rce.CPUTime)
            }
    exptstarttime,exptduration

open Helper

let getDIEventTimeFromRCEID nictrtimes rceid =
    let n = Array.length nictrtimes
    match binSearch (fun i -> compare rceid nictrtimes.[i|>int].RCEID) 0L (n-1|>int64) with
    | Exact i | LargerThan i -> nictrtimes.[i|>int]
    | _ -> failwith "rceid not found"
    
