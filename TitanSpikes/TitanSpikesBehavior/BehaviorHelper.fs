module BehaviorHelper

open Microsoft.FSharp.Data.TypeProviders
open System.Xml.Linq
open Linq.NullableOperators
open System

let ephyssr = 30000.7235
let hrclockspeed = 100000

[<Literal>]
let cstr = "Data Source=140.247.178.203,5900;Initial Catalog=OpConRPODDARTwoTap;User ID=rajeshpoddar;Password=htsn0f00d!"
type dbschema = SqlDataConnection<cstr,Functions=true,StoredProcedures=true>
let inline xn s = XName.Get(s)
type RatControlEvent = dbschema.ServiceTypes.GetSMRatEventsResult
type ParsedState =
    { Entry : RatControlEvent
      Transition : TransitionType
      Tasks : RatControlEvent list}
and  TransitionType =
        | EventTrans of RatControlEvent
        | StateTrans of ParsedState
        | Initial

let getfloatAttribute name (rce:RatControlEvent) =
    rce.Details.Attribute(xn name)|>float

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
    let foldfun (curfsm,lastevent, parsed) x =
        match x with
        | StateEntry (s,c) ->
            let t =  match c with
                        | None ->  
                            match lastevent with
                            | Some x -> EventTrans x
                            | _ -> failwith (sprintf "Failed at RCEID %d" (x.RatEventsID.Value))
                        | Some i -> StateTrans (parsed |> Map.find i |> List.head)
            let newstate = {Entry = x; Transition = t; Tasks = []} 
            (s,lastevent,parsed|>addstate s newstate)
        | Event -> 
            (curfsm, Some x, parsed)
        | EntryTask ->
            let curstate = parsed|>Map.find curfsm|>List.head
            let newstate = {curstate with Tasks = x::curstate.Tasks}
            (curfsm, lastevent, parsed |> updatestate curfsm newstate)
        | _ -> //ignore
            (curfsm, lastevent, parsed)
    match rces with 
    | head::tail ->
        let initstate = {Entry = head; Transition = Initial; Tasks = []}        
        let _,o,p = tail 
                    |> Seq.fold foldfun 
                        (0,None,Map.empty |> addstate 0 initstate)
        p|> Map.map (fun _ l -> List.rev l)
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

let inline getDIDetails (x:XElement) =
    x.Attribute(xn "ChannelNum")|>int,
    x.Attribute(xn "NewState").Value|>bool.Parse

type EventTime = 
    {
        NICtrTime: int64
        RCEID: int
        CPUTime: int64
        EventType:byte
    }

type DIEventTime = 
    {
        EventTime: EventTime
        Channel: int
        NewState: bool
    }

let unfold32bit rawvals =
    rawvals
    |> Seq.pairwise
    |> Seq.scan (fun (offset) (a,b) -> 
                    if (b-a < 0L) then (offset+4294967296L) else (offset)) 0L
    |> Seq.zip rawvals
    |> Seq.map (fun (a,b) -> a+b)
    |> Seq.toArray

type db(cstr) = 
    member this.getExpts(ratName) =
        use dbc = dbschema.GetDataContext(cstr)
        query {
            for expt in dbc.Expts do
            where (expt.Rats.RatName = ratName)
            select (expt.ExptID,expt.StartTime)
        }
        |> Seq.toList

    member this.getStageSMs(exptid,stageName) =
        use dbc = dbschema.GetDataContext(cstr)
        let stageNums =
            query {
                for stage in dbc.Stages do
                where (stage.StageLists.ExptID ?= exptid && stage.SMInstances.ParametrizedStateMachines.Name = stageName)
                select stage.Number
            }
            |> Seq.toList
        stageNums |> List.map (fun stageNum ->
            query {
                for sm in dbc.StateMachines do
                where (sm.ExptID = exptid && sm.Type = 0uy && sm.Details ?= stageNum && sm.ActiveTime.HasValue)
                sortBy sm.DefinitionID
                select (sm.DefinitionID,sm.Definition)
            } |> Seq.toList
        )
        |> List.concat
        |> List.sortBy fst

    member this.getExptTimeExtent(exptid) =
        use dbc = dbschema.GetDataContext(cstr)
        let exptstarttime = 
            query {
                for expt in dbc.Expts do
                where (expt.ExptID = exptid)
                select (expt.StartTime)
                take 1}
            |> Seq.head |> fun x -> x.Value

        let exptduration =
            query {
                for rce in dbc.RatControlEvents do
                where (rce.ExptID = exptid)
                maxBy (rce.CPUTime)
                }
        exptstarttime,exptduration

    member this.getSMCPUTime(exptid,smid) =
        use dbc = dbschema.GetDataContext(cstr)
        let start,lasttime = this.getExptTimeExtent exptid
        let r = 
            query {
                for sm in dbc.StateMachines do
                where (sm.ExptID = exptid && (sm.DefinitionID = smid || sm.DefinitionID = smid+1))
                sortBy sm.DefinitionID
                select (sm.ActiveTime)
            } |> Seq.toList
        match r with
        | [t1;t2] when t1.HasValue && t2.HasValue -> Some (t1.Value,t2.Value)
        | [t1] when t1.HasValue -> Some (t1.Value,lasttime)
        | _ -> None

    member this.getSMTime(exptid,smid) =
        let start,lasttime = this.getExptTimeExtent exptid
        this.getSMCPUTime(exptid,smid) |> Option.map (fun (t1,t2) -> 
            start+TimeSpan.FromMilliseconds(t1|>float),TimeSpan.FromMilliseconds(t2-t1|>float)
        )

    member this.getRCEs(exptid,smid) = 
        use dbc = dbschema.GetDataContext(cstr)
        query { for rce in dbc.GetSMRatEvents(Nullable exptid,Nullable smid) do 
                sortBy rce.RatEventsID.Value
                select rce }
        |> Seq.toList

    member this.getNICtrtime(exptid,evtype,numevents) =
        use dbc = dbschema.GetDataContext(cstr)
        let rawvals = 
            query {
                for rce in dbc.EventTimes do
                where (rce.ExptID = exptid &&  rce.EventType = evtype)
                sortBy rce.EventTimesID
                select (rce.NICtrTime)
                take numevents
            }
        rawvals
        |> unfold32bit

    member this.getHRTimes(exptid,maxrceid) = 
        use dbc = dbschema.GetDataContext(cstr)
        dbc.DataContext.CommandTimeout <- System.Int32.MaxValue
        let rces = 
            query {
                for rce in dbc.RatControlEvents do
                where (rce.ExptID = exptid && rce.EventType <= 4uy && rce.RatEventsID <= maxrceid)
                sortBy rce.RatEventsID
                select (rce.RatEventsID,(rce.EventType,rce.Details),rce.CPUTime)
            }
            |> Seq.toArray
        printfn "%d" (rces |> Array.length)
        let nictrtimes = 
            let rawvals = 
                query {
                    for rce in dbc.EventTimes do
                    where (rce.ExptID = exptid &&  rce.EventType <= 4uy)
                    sortBy rce.EventTimesID
                    select (rce.NICtrTime)
                    take (rces |> Array.length)
                }
            rawvals
            |> unfold32bit    
        Seq.zip nictrtimes rces
        |> Seq.toArray
        |> Array.map (fun (t,(rceid,(evtype,details),cputime)) -> details,{NICtrTime=t;RCEID=rceid;CPUTime=cputime;EventType=evtype})
        |> fun xs ->
            (xs |> Array.map snd),
            (xs |> Array.filter (snd>>(fun x -> x.EventType = 4uy)) |> Array.map (fun (y,evt) -> 
                let chnum,state = getDIDetails y
                {EventTime=evt;Channel=chnum;NewState=state}
            ))

    member this.getAOTimes(exptid) =
        use dbc = dbschema.GetDataContext(cstr)
        query { 
            for rce in dbc.RatControlEvents do
            where (rce.ExptID = exptid && rce.EventType = 9uy) 
            sortBy rce.RatEventsID
            select (rce.RatEventsID,rce.Details,rce.CPUTime)         
        } 
        |> Seq.filter (fun (x,y,_) -> y.Attribute(xn "Name").Value = "AO")
        |> Seq.map (fun (x,y,z) -> x,y.Element(xn "AODetails").Attribute(xn "SampleNum")|>int64,z)
        |> Seq.toArray
