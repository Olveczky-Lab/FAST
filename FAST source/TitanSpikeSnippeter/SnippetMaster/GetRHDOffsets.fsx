#r "System.Xml.Linq"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
fsi.ShowDeclarationValues <- false
open System.Xml.Linq
open System.IO
open Helper

let hostname = Remote "140.247.178.16:5900"
let fnums =
    [|"Hindol",
        [|"635610886969812486"|]
    |]
for rat,fnum in fnums |> Array.map (fun (rat,xs) -> xs |> Array.map (fun x -> rat,x)) |> Array.concat do
    let fname = sprintf @"/root/data/asheshdhawale/Data/%s/%s.rhd" rat fnum
    let flen = getSize hostname fname |> Async.RunSynchronously

    let magic = [|66uy; 25uy; 2uy; 39uy; 153uy; 25uy; 145uy; 198uy|]
    let nmagic = Array.length magic

    let rec blockLoop blockfun offset =
        let blocksize = 176*3000
        let nbytes = min (blocksize|>int64) (flen-offset) |> int
        if nbytes <= 0 then None else
        let bytes = getArray<byte> hostname fname (offset|>uint64) nbytes
        match blockfun bytes with
        | Some x -> Some (offset+(x|>int64))
        | None -> blockLoop blockfun (offset+(blocksize|>int64))

    let findnextmagic bytes =
        let nbytes = Array.length bytes
        let rec findnextmagicR ibytes imagic = 
            if ibytes < nbytes then
                if bytes.[ibytes] = magic.[imagic] then
                    if imagic = nmagic-1 then Some (ibytes-nmagic+1)
                    else findnextmagicR (ibytes+1) ((imagic+1)%nmagic)
                else findnextmagicR (ibytes+1) 0
            else None
        findnextmagicR 0 0

    let blockSize = 15L*30000L*176L
    let rec getNumBlocks offset n =
        if offset+(nmagic|>int64) < flen then
            match findnextmagic (getArray<byte> hostname fname (offset|>uint64) nmagic) with
            | Some 0 -> getNumBlocks (offset+blockSize) (n+1)
            | _ -> n
        else n

    let blockRanges = 
        Seq.unfold (fun offset ->
            blockLoop findnextmagic offset |> Option.map (fun offset ->
                let nblocks = getNumBlocks (offset+blockSize) 0
                (offset,nblocks),(offset + (nblocks+1|>int64)*blockSize)
            )
        ) 0L |> Seq.toArray

    printfn "%s %s" rat fnum
    for offset,nblocks in blockRanges do
        if nblocks > 0 then
            printfn "%d %d" offset (nblocks-2)
    printfn ""