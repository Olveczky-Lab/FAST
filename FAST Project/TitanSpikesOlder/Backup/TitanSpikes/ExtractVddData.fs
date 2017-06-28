module ExtractVddData

open System.IO
open System
open RP.HDF5.H5TypeProviderRuntime

let extractVdd fnum = 
    let path =  @"H:\Data\"
    let fname = path + fnum + ".vdd"
    let sr = 30000/60 // sampling rate
    let f = new FileInfo(fname)
    let chipnums = [0..1]

    //Assumes data array contains blocks of 5 samples of one chip followed by 5 samples of the next.
    //let convertToUInt16 blocksize b chipnum =
    //    let d = Array.zeroCreate<uint16> blocksize
    //    for i in 0..blocksize/5-1 do
    //        let start_offset = i*10*2 + chipnum*10
    //        for j in 0..5-1 do
    //            d.[i*5+j] <- BitConverter.ToUInt16(b,start_offset+j*2)
    //    d

    //Assumes one sample of one chip followed by one sample of the next ...
    let convertToUInt16 blocksize b chipnum =
        Array.init blocksize (fun i -> BitConverter.ToUInt16(b,chipnum*2+i*(chipnums.Length)*2))

    let ReadBlock blocksize (fs:FileStream) (perChComp: byte[] -> int -> _) = 
        let b = Array.zeroCreate<byte> (blocksize*(chipnums.Length)*2)
        let numRead = fs.Read(b,0,b.Length)
        chipnums |> List.map (fun chipnum -> async { return (perChComp b chipnum) }) |> Async.Parallel |> Async.RunSynchronously

    let outputfname = @"C:\EphysData\Sullivan\Ashesh\" + fnum + ".h5"
    let getVDD =
        async {        
            let blocksize = sr*30 //30 seconds
            let numblocks = f.Length/2L/(chipnums.Length|>int64)/(blocksize|>int64) |> int
            use fs = f.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            for i in 1..numblocks do
                let r = ReadBlock blocksize fs (convertToUInt16 blocksize)            
                writeData outputfname
                          "./VDD"
                          ([|(uint64 (i-1))*(uint64 blocksize);0UL|])
                          null
                          (Array2D.init blocksize chipnums.Length (fun i j -> r.[j].[i]))
                if (i%60 = 59) then
                    printfn "Completed block %d of %d" i numblocks
        }

    Async.RunSynchronously getVDD

extractVdd "635025802044762119"