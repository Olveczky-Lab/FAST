module ExtractAxlData

open System.IO
open System
open RP.HDF5.H5TypeProviderRuntime

let extractAxl fnum = 
    let path =  @"H:\Data\"
    let fname = path + fnum + ".axl"
    let sr = 30000/4 // sampling rate
    let f = new FileInfo(fname)
    let chnums = [0..2]

    //Assumes one sample of x followed by one sample of the y followed by one sample of z ... repeat
    let convertToUInt16 blocksize b chnum =
        Array.init blocksize (fun i -> BitConverter.ToUInt16(b,chnum*2+i*(chnums.Length)*2))

    let ReadBlock blocksize (fs:FileStream) (perChComp: byte[] -> int -> _) = 
        let b = Array.zeroCreate<byte> (blocksize*(chnums.Length)*2)
        let numRead = fs.Read(b,0,b.Length)
        chnums |> List.map (fun chipnum -> async { return (perChComp b chipnum) }) |> Async.Parallel |> Async.RunSynchronously

    let outputfname = @"C:\EphysData\Sullivan\Ashesh\" + fnum + ".h5"
    let getAxl =
        async {        
            let blocksize = sr*30 //30 seconds
            let numblocks = f.Length/2L/(chnums.Length|>int64)/(blocksize|>int64) |> int
            use fs = f.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            for i in 1..numblocks do
                let r = ReadBlock blocksize fs (convertToUInt16 blocksize)            
                writeData outputfname
                          "./AXL"
                          ([|(uint64 (i-1)) * (uint64 blocksize);0UL|])
                          null
                          (Array2D.init blocksize chnums.Length (fun i j -> r.[j].[i]))
                if (i%60 = 59) then
                    printfn "Completed block %d of %d" i numblocks
        }

    Async.RunSynchronously getAxl

extractAxl "635025802044762119"