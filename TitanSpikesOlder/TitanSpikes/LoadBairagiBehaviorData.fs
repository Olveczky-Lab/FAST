#if COMPILED
module LoadBairagiBehaviorData
#else
#r @"FSharp.Data.TypeProviders.dll"
#r @"System.Data.Linq.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"

#time
fsi.ShowDeclarationValues <- false
#endif

open System.Xml.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Collections
open Linq.NullableOperators
open System
open System.Data.Linq.SqlClient
open RP.HDF5

[<Literal>]
let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
type dbschema = SqlDataConnection<cstr>
let dbinst () = new dbschema.ServiceTypes.OpConASHESHDHAWALE1(cstr)

let inline xn s = XName.Get(s)
let db = dbinst ()

//
let offset = 239861913L-1092L
let lastcputime = ((79.00*3600.0*1000.0)|>int64)
let offsetrceid = 994339
let lastrceid = 1977513
let period = (2.0**15.0-1.0)/30.0+0.001058 //in ms
let sr = 1000.0

let getdievents chnum = 
    let e = 
       (query {for rce in db.RatControlEvents do
                where (rce.ExptID = 2 && rce.EventType = 4uy && rce.RatEventsID > offsetrceid 
                        && (db.GetDIChannelNum(rce.Details) ?= chnum)
                        && rce.RatEventsID < lastrceid)
                sortBy (rce.RatEventsID)
                select (rce.CPUTime-offset)}
        |> Seq.toArray)
    [|for i in 0..2..e.Length-1 -> e.[i]|]

let cherryswitchevents = getdievents 1

let sms =
    query {for sm in db.StateMachines do
           where (sm.ExptID = 2 && sm.FirstRCEID ?>= offsetrceid && sm.FirstRCEID ?<= lastrceid)
           sortBy (sm.DefinitionID)
           select (sm.ActiveTime,sm.Type,sm.Details)}
    |> Seq.toArray

let (lightsofftimes,lightsontimes) =
    let offtimes = sms |> Array.choose (fun (t,typ,det) -> if (typ = 1uy && det.Value = 0) then Some (t.Value-offset) else None)
    let ontimes = sms |> Array.choose (fun (t,typ,det) -> if (typ = 1uy && det.Value = 1) then Some (t.Value-offset) else None)
    (offtimes,ontimes)

let trainingsms =
    Array.zip [|0..(sms.Length-1)|] sms
    |> Array.filter (fun (_,(_,typ,_)) -> typ = 0uy)
    |> Array.map (fun (i,(t,_,_)) -> (t.Value-offset, let (t2,_,_) = sms.[i+1] in t2.Value-offset))

[<Literal>]
let outputfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\Processed.h5"
type h5file = HDF5File<outputfname>
h5file.LeverPressTimes.WriteData(null,null,cherryswitchevents)
h5file.LightsOffTime.WriteData(null,null,lightsofftimes)
h5file.LightsOnTime.WriteData(null,null,lightsontimes)
h5file.TrainingSessionTimes.WriteData(null,null,Array2D.init (Array.length trainingsms) 2 (fun i j -> if (j = 0) then (trainingsms.[i]|>fst)
                                                                                                       else (trainingsms.[i]|>snd)))
//Check eSyncValidity
let displayoffset () =
    let num = 20000
    let all = 
        query {for rce in db.RatControlEvents do
               where (rce.ExptID = 2 && rce.EventType = 4uy && rce.RatEventsID > 1507413 && (db.GetDIChannelNum(rce.Details) ?= 2))
               sortBy (rce.RatEventsID)
               select (rce.CPUTime)
               take num }
    let first = all |> Seq.head
    let last = all |> Seq.skip (num-1) |> Seq.head        
    ((last - first)|>float)-(period*((num|>float)-1.0))
    |> printfn "%A ms"
