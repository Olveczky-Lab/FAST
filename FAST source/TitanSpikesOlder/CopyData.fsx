open System.IO

let saveallchannels () =
    let f = new FileStream(@"\\ephys1\E\Data\635021511117167662.amp",FileMode.Open,FileAccess.Read)
    let fw = new FileStream(@"C:\EphysData\Sullivan\Ashesh\ForBehtash10minAllChannels.amp",FileMode.Create,FileAccess.Write)
    f.Seek(2L*64L*30000L*3600L*3L,SeekOrigin.Begin) |> ignore
    let b = Array.zeroCreate<byte>(2*64*30000*60)
    for i in 1..10 do
        printfn "%d" i
        f.Read(b,0,b.Length) |> ignore
        fw.Write(b,0,b.Length)

let savechannels chnums =
    let f = new FileStream(@"\\ephys1\E\Data\635021511117167662.amp",FileMode.Open,FileAccess.Read)
    let fws = chnums |> List.map (fun chnum -> chnum,new FileStream(sprintf @"C:\EphysData\Sullivan\Ashesh\ForBehtash7hrsCh%d.amp" chnum,FileMode.Create,FileAccess.Write))
    let b = Array.zeroCreate<byte>(2*64*30000*60)
    for i in 1..7*60 do
        printfn "%d" i
        f.Read(b,0,b.Length) |> ignore
        fws |>
        List.iter (fun (chnum,fw) -> for offset in (chnum*2)..(64*2)..b.Length-1 do
                                        fw.Write(b,offset,2))
                        
    