#if INTERACTIVE
#load "VideoAlignment.fs"
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open VideoAlignment
open Helper

let ephyspath = @"Z:\badlands"
let ephysfnum = "635276396558304920"
let tapch = "5"

//[<EntryPoint>]
let main argv = 
    match argv with
    | [|ephyspath;ephysfnum;videopath;videofnum;syncch;tapch;offset;|] ->
        let tmppath = @"C:\temp\Tap"
        let videofnum = videofnum |> int64
        let ephysfnum = ephysfnum |> int64
        let syncch = syncch |> int
        let tapch = tapch |> int
        let offset = offset |> uint32
        let out x = sprintf @"%s\%d\%d%s.png" videopath videofnum offset x
        saveAvgTapFrame ephyspath ephysfnum videopath videofnum syncch offset tapch tmppath (out "D",out "U")
    | _ -> printfn "Invalid Arguments"
    0 // return an integer exit code

let _ = 
    let videopath = @"Z:\badlands\Cam1"
    let videofnums = 
        [|"635279076170677523";"635279724190557725";"635279940155527163";"635280588170452721";"635280804160877294";"635281452142125462"|]
    let offsets = [|2;0;0;0;1;4|]
    let syncch = "6"
    for videofnum in videofnums do
        for offset in 0..10 do
            let argv = [|ephyspath;ephysfnum;videopath;videofnum;syncch;tapch;offset|>string;|]
            main argv |> ignore

let _ =
    let videopath = @"Z:\badlands\Cam2"
    let syncch = "7"
    let videofnums = 
        [|"635279076170867523";"635279724190817725";"635279940155807191";"635280588170662742";"635280804161107294";"635281452142315481"|]
    let offsets = [|7;8;9;9;9;5|]
    for videofnum in videofnums do
        for offset in 0..10 do
            let argv = [|ephyspath;ephysfnum;videopath;videofnum;syncch;tapch;offset|>string;|]
            main argv |> ignore

let saveframetimes () = 
    let videofnum = 635276396670964920L
    let ephysfnum = 635276396558304920L
    let syncch = 8
    let videopath = @"Z:\badlands\Cam3"
    let ephyspath = @"Z:\badlands"
    let initoffset = 0u
    let frameinterval = 1./30.
    let frametimes = 
        getAlignedFrameTimes videofnum ephysfnum syncch videopath ephyspath initoffset frameinterval
    frametimes
    |> Option.get
    |> Array.choose (fun (ft,x) -> x |> Option.map (fun _ -> ft))
    |> writeArray Local (sprintf "%s\%d\%d.ft" videopath videofnum ephysfnum) 0UL
    |> Async.RunSynchronously
