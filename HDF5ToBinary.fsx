#r "System.Xml.Linq.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
fsi.ShowDeclarationValues <- false
open System.IO
open System.Xml.Linq
open Helper

let path = @"Z:\badlands"
let fnum = "635276396558304920"

let esets = loadnTrodes (XElement.Load(sprintf @"%s\TitanSpikeSnippeterSettings.xml" path).Element(xn "EIBElectrodePlacement"))
let dirpath = sprintf @"%s\%s" path fnum
let createDirStruct =
    Directory.CreateDirectory(dirpath) |> ignore
    esets |> List.iter (fun (chname,_) -> Directory.CreateDirectory(sprintf @"%s\ChGroup_%s" dirpath chname)|>ignore)
    Directory.CreateDirectory(sprintf @"%s\LFP" dirpath) |> ignore

let remotepath = @"./data/rpoddar/badlands"
let zoneuuids = [|"1b1b7a27-7691-4dd5-8993-b5be6ab5a33e";"3bb1ab2c-7293-455b-a414-56f3193b52d4";"3cef86ef-eb14-40df-98d3-1b183e6ce8ad";"427ed081-8048-45ba-9f61-50814e7ff3d7";"72615434-fdba-459e-8965-0394b09e4308";"82f259db-4819-4bd1-b813-daf45b5abd84";"89a7bd8d-98b1-4327-ab1b-4e91f0eea918";"adf78d5d-7e79-4254-9146-862c386b4c04";"c4b9f298-0469-4226-8081-7b952402da38";"e8370f46-3d6f-4152-9042-98c8767355d3"|]
let copySpikeSnippet =
    let dump chname dset indx = 
        sprintf @"h5dump -d '/ChGroup_%s/%s' -b LE -o %s/%s/ChGroup_%s/%s_%d %s/%s_%d.h5 &" 
            chname dset remotepath fnum chname dset indx remotepath fnum indx
    for i in 0..9 do
        esets |> Seq.map (fun (chname,_) ->
                [(dump chname "Spikes" i);(dump chname "SpikeTimes" i)]
        ) |> Seq.concat
        |> String.concat " "
        |> printfn @"zlogin %s ""%s""" zoneuuids.[i]
let copyLFP =
    let sourcepath = @"./data/rpoddar/.zfs/snapshot/weekly-20140324T000000Z/badlands"
    let x indx =
        if indx < 5 then
            30UL+36621000UL*(indx|>uint64),36621000UL
        else
            30UL+36621000UL*5UL+35964000UL*(indx-5|>uint64),35964000UL
            
    let dump chnum indx =         
        let offset,count = x indx
        sprintf @"h5dump -d '/LFP/Ch_%d[%d;;%d]' -b LE -o %s/%s/LFP/Ch_%d_%d %s/%s_%d.h5 &" 
            chnum offset count remotepath fnum chnum indx sourcepath fnum indx
    for i in 0..9 do
        esets |> Seq.map (snd>>Seq.map (fun chnum -> dump chnum i)) |> Seq.concat |> String.concat " "
        |> printfn @"zlogin %s ""%s""" zoneuuids.[i]

let mergefiles = 
    let dump chnum =
        sprintf @"cat %s > %s/%s/LFP/Ch_%d &" 
            ([|for i in 0..9 -> sprintf @"%s/%s/LFP/Ch_%d_%d" remotepath fnum chnum i|] |> String.concat " ")
            remotepath fnum chnum
    esets |> Seq.iter (fun (_,chs) ->
        chs |> Seq.iter (fun chnum ->
            printfn "%s" (dump chnum))
    )