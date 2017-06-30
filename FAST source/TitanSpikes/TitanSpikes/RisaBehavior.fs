module RisaBehavior

open Linq.NullableOperators
open System
open BehaviorHelper
open TwoTapHelper
open Nessos.FsPickler
open System.IO
open Helper

let cstrs = 
    ["Data Source=140.247.178.220;Initial Catalog=OpConRISAKAWAI3;User ID=risakawai;Password=iawakasir!1","TwoTapTrialAndErrorV2"
     "Data Source=140.247.178.220;Initial Catalog=OpConRISAKAWAI4;User ID=risakawai;Password=iawakasir!1","Two Tap"
     "Data Source=140.247.178.220;Initial Catalog=OpConRISAKAWAI5;User ID=risakawai;Password=iawakasir!1","TwoTapTrialAndErrorV2"
     "Data Source=140.247.178.225;Initial Catalog=OpConRISAKAWAI6;User ID=risakawai;Password=iawakasir!1","TwoTapTrialAndErrorV2"
    ]
let ratsintact = 
    ["BurkinaFaso",117;"Burundi",102;"Cambodia",170;"Cameroon",113;"CostaRica",167;"IvoryCoast",184;"Estonia",135;
     "Ghana",75;"Guinea",175;"GuineaBissau",218;"Italy",153;"Iran",163;"Kansas",66;"Kentucky",83;"Maine",103;
      "Massachusetts",73;"Ireland",126;"Iraq",309]

let learning =
    let intertrial = [|20443;6000;25110;14745;9388;29361;7318;15092;12210;23667;18434;10031;7356;6000;6000;10962;13000;44356|]
    let intertap = [|7799;18639;17230;11227;8447;23177;10378;14433;13605;24075;13947;9828;7437;6990;11449;9415;16726;21595|]
    Array.zip (ratsintact |> List.map fst |> List.toArray) (Array.zip intertap intertrial) |> Map.ofArray

let ratslesioned =
    ["CzechRepublic",203;"Dominica",259;"DominicanRepublic",241;"EastTimor",254;"ElSalvador",174;
     "EquatorialGuinea",179;"Comoros",165;"Congo",125;"Croatia",208;"Cuba",130;"Cyprus",178]

let getAllData () = 
    List.append ratsintact ratslesioned 
    |> List.map (fun (rat,n) ->
        cstrs |> List.map (fun cstr ->
            let curdb = db2tap(cstr)
            curdb.getExpts(rat) |> List.choose (fun (a,x)  -> if x.HasValue then Some (a,x.Value) else None) 
            |> List.sortBy snd |> Seq.map (fun (exptid,_) -> 
                curdb.getTwoTapSMs(exptid)
                |> Seq.sortBy fst 
                |> Seq.choose (fun ((smid,_) as sm) -> 
                    printfn "%A" (rat,exptid,smid)
                    loaddata cstr exptid smid |> Option.map (fun x -> (((cstr,exptid),sm),x))
                )
            ) |> Seq.concat
        )
        |> Seq.concat
        |> Seq.take n
        |> Seq.toList
    )

let loadAllData () =
    use fs = new MemoryStream(getArray<byte> (Remote "140.247.178.16:5900") @"/root/data/rpoddar/RisaData/alldata" 0UL -1)
    let x = 
        FsPickler().Deserialize<((((string * string) * int) * (int * (int * int * int))) *(Trial [] * Map<int,int64>)) list list>(fs)
    let allrats = List.append ratsintact ratslesioned |> List.map fst
    let trials = List.zip allrats (x |> List.map ((List.map snd)>>List.toArray)) |> Map.ofList
    let sms = List.zip allrats (x |> List.map ((List.map fst)>>List.toArray)) |> Map.ofList
    trials,sms
