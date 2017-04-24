let printcmds drive rat fnum first last (numblocks:int[]) alloc = 
    let exepath = @"Y:\Ephys\Data Analysis\FSharp\TitanSpikeSegFuser\TitanSpikeSegFuser\bin\Release\TitanSpikeSegFuser.exe"
    let fusecmd chnum = sprintf @"& '%s' 'SegFuse' '%s:\%s\%s' %d %d %d '%s:\%s\%s_ChGroup_%d.sf'" exepath drive rat fnum chnum first last drive rat fnum chnum
    let timescmd chnum = sprintf @"& '%s' 'BlockTimes' '%s:\%s\%s' %d %d %d '%s:\%s\%s_ChGroup_%d.bt'" exepath drive rat fnum chnum first last drive rat fnum chnum
    let all = alloc |> List.concat
    if  all |> Seq.distinct |> Seq.length <> 16 || all |> Seq.length <> 16 then failwith "Invalid Allocation"
    for chnums in alloc do
        printfn "%d" (chnums|>List.map (fun j ->numblocks.[j]) |> List.sum)
        for chnum in chnums do
            printfn "%s" (fusecmd chnum)
            //printfn "%s" (timescmd chnum)

//let rat = "badlands";
//let fnum = "635276396558304920"
//let first = 0
//let last = 9
//let numblocks = [|72907;33373;31851;20269;96773;71210;80507;16829;72587;25168;40568;18584;39021;64935;36479;28326|]
//let alloc = [[0;1;2];[3;4;9];[6;8];[5;10;12];[7;11;13;14;15]]
//let drive = "Z"

let rat = "arches";
let fnum = "635267094066850917"
let first = 0
let last = 4
let numblocks = [|109818;67810;76496;106886;86836;72134;96221;129881;86546;83558;64527;84202;48311;70296;46021;16788|]
let alloc = [[0;1;2];[3;4;5];[12;7;8];[9;10;11];[6;13;14;15]]
let drive = "X"

printcmds drive rat fnum first last numblocks alloc