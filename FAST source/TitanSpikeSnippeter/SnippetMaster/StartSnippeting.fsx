﻿#r "System.Xml.Linq"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"

open System.Xml.Linq
open System.IO
open Helper
open System.Text.RegularExpressions
let xn = XName.op_Implicit

let datahostip = @"192.168.0.100:5900"

let serverpathip = Remote @"140.247.178.16:5900"
let serverpath = sprintf @"/root/data/rpoddar/%s/raw/%s"
let serverpathgz = sprintf @"/zones/data/rpoddar/%s/raw/%s"
let userpathout = @"\\140.247.178.8\rpoddar\"
let serverpathout = sprintf @"/root/data/rpoddar/%s/raw/%s"

let cmdpath = @"& 'C:\Titanic\Snipppeter"

let init rat fnum chstoexclude numsamples byteoffset =
    let sp = serverpath rat fnum

    //Tetrodes
//    let esets = List.init 16 (fun i -> 
//        sprintf "%d" i,Set.difference ([i*4..i*4+3]|>Set.ofList) chstoexclude|>Set.toList)

    //Single Electrodes
    let esets = 
        List.init 64 (fun i ->
            if chstoexclude |> Set.contains i then
                None
            else
                Some (sprintf "%d" i,[i])
        ) |> List.choose id

    let dirpath = 
        let x = sprintf @"%s\%s\%s" userpathout rat fnum
        match byteoffset with 
        | Some bo -> sprintf @"%s_%d" x bo
        | None -> x

    Directory.CreateDirectory(dirpath) |> ignore
    esets |> List.iter (fun (chname,_) -> Directory.CreateDirectory(sprintf @"%s\ChGroup_%s" dirpath chname)|>ignore)
    Directory.CreateDirectory(sprintf @"%s\LFP" dirpath) |> ignore

    let settingsfile = sprintf @"%s\SnippeterSettings.xml" dirpath

    let samplesPerBlock = 30000*15 //15 secondss
    let endPadding = 3000
    let w_pre,w_post = 31,32
    let dfact = 100
    let numsamples = 
        match numsamples with
        |Some x -> x 
        |None -> 
            (getSize serverpathip (sprintf "%s.amp" sp) |> Async.RunSynchronously)/128L
            //(getSize serverpathip (sprintf "%s.rhd" sp) |> Async.RunSynchronously)/176L
    let numblocks = (numsamples-(endPadding|>int64))/(samplesPerBlock|>int64)|>int

    let refchans chipnum =
        esets |> List.map snd |> List.concat |> List.filter (fun chnum -> chnum >= chipnum*32 && chnum < (chipnum+1)*32)
        |> List.map (fun x -> XElement(xn "Channel",x))
        |> fun x -> XElement(xn "MedianReferenceGroup",x)

    let settings = 
        XElement(xn "TitanSpikeSnippeterSettings",
            XElement(xn "BlockSize",samplesPerBlock),
            XElement(xn "EndPadding",endPadding),
            XElement(xn "WPre", w_pre),
            XElement(xn "WPost", w_post),
            XElement(xn "LFPDownSampleFactor",dfact),
            XElement(xn "FirstBlock",0),
            XElement(xn "LastBlock",numblocks-1),
            XElement(xn "EIBElectrodePlacement",
                esets|>List.choose(fun (chname,chnums) -> 
                    if List.length chnums > 0 then
                        XElement(xn "ElectrodeSet",
                            XAttribute(xn "Name",chname),
                            chnums|>List.map (fun chnum -> XElement(xn "Channel",chnum))
                        )|>Some
                    else None
                )
            ),
            XElement(xn "Thresholds",[0..63] |> List.map (fun chnum -> 
                XElement(xn "Channel",
                    XAttribute(xn "EventThreshold",250),
                    XAttribute(xn "ReturnThreshold",100),
                    XAttribute(xn "Number",chnum)
                )
            )),
            XElement(xn "MedianReferenceGroups",refchans 0,refchans 1)
        )

    match byteoffset with 
    | Some bo -> 
        settings.Add(XElement(xn "ByteOffset",bo))
    | None -> ()
    settings.Save(settingsfile)
    let spout = 
        let x = serverpathout rat fnum
        match byteoffset with 
        | Some bo -> sprintf @"%s_%d" x bo
        | None -> x
    sp,spout

let files = [|"bairagi",[|"635013888324512817",[||],None,None;|]|]

//let files = 
//    File.ReadAllLines(@"C:\Users\rpoddar\Downloads\snippeting-SW-7.txt")
//    |> Array.map (fun x -> x.Split())
//    |> Array.fold (fun state x -> 
//        match x with
//        | [|""|] -> state
//        | [|rat|] when (Regex.IsMatch(rat,"^\d+$")|>not) -> 
//            (rat,[])::state
//        | _ -> 
//            match state with
//            | (rat,xs)::ys ->
//                let file::rest = x |> Array.filter (fun a -> a <> "")  |> Array.toList
//                let rest = rest |> List.map int64
//                let chstoexclude = 
//                    rest |> List.choose (fun r -> 
//                        if r <= 64L then Some (r-1L|>int) else None
//                    ) |> List.toArray
//                let last = rest |> List.tryFind (fun r -> r > 64L)
//                (rat,((file,chstoexclude,last,None)::xs))::ys
//            | _ -> failwith "Not Possible"
//    ) [] |> List.rev |> List.toArray |> Array.map (fun (rat,xs) -> rat,xs|>List.rev|>List.toArray)

let initall () = 
    files |> Array.map (fun (rat,xs) -> xs |> Array.map (fun (file,chs,n,bo) -> 
        (rat,file),init rat file (chs|>Set.ofArray) n bo))
    |> Array.concat 
    |> fun x ->
        x |> Array.iter (fun (_,(cmd,_)) -> printfn "'%s'," cmd)
        printfn ""
        x |> Array.iter (fun (_,(_,cmd)) -> printfn "'%s'," cmd)

let dispdelcmds () = 
    files |> Array.iter (fun (rat,xs) -> xs |> Array.iter (fun (file,_,n,_) ->
        let oldfname = serverpathgz rat file
        printfn "rm %s.rhd" oldfname
    ))

let dispttlcmd () =
    let cmd = sprintf @"%s\StartTTL.ps1'" cmdpath
    files |> Array.iter (fun (rat,xs) ->
        xs |> Array.iter (fun (fnum,_,_,_) ->
            let serverf = serverpath rat fnum
            let localf = sprintf @"%s\%s\%s\%s" userpathout rat fnum
            Directory.CreateDirectory(localf "TTLChanges") |> ignore
            let numsamples = (getSize Local (localf "TTLIns") |> Async.RunSynchronously)/2L
            printfn "%s %s %s %d"  cmd datahostip serverf numsamples
        )   
    )
    
let dispampcmd userpath rat fnum =
    let cmd = sprintf @"%s\StartAmp.ps1'" cmdpath
    let localf = sprintf @"%s\%s\%s\%s" userpath rat fnum
    let esets = 
        loadnTrodes (XElement.Load(localf "SnippeterSettings.xml").Element(xn "EIBElectrodePlacement"))
    for chgroup,chans in esets do
        let serverf = sprintf "%s/ChGroup_%s" (serverpath rat fnum) chgroup
        let numsamples = (getSize Local (localf (sprintf @"ChGroup_%s\SpikeTimes" chgroup)) |> Async.RunSynchronously)/8L
        printfn "%s %s %s %d %d"  cmd datahostip serverf numsamples (Array.length chans)

