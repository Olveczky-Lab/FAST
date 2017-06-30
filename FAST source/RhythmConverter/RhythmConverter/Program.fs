#if COMPILED 
#else
#load "AgentHelper.fs"
let argv = [|@"\\RPODDAR-EPHYS\EphysData\Sullivan\634991170795962570.amp";@"C:\EphysData\634991170795962570.amp"|]
#endif

open System
open System.IO
open System.Diagnostics
open AgentHelper

[<EntryPoint>]
let main argv = 
    let src_file = argv.[0]
    let dst_path = argv.[1]
    let fname = Path.GetFileName(src_file)
    let dst_file = dst_path + fname
    let nchans,nbytespersamp,nsampsperblock =
        match (Path.GetExtension(src_file)) with
        | ".amp" -> 64,2,300
        | ".vdd" -> 2,2,5
        | ".temp" -> 2,4,5
        | _ -> failwith "Unknown File Type"

    let blocksize = 200e6/(nbytespersamp|>float)/(nchans|>float)|>int

    let totalblocks = (FileInfo(src_file).Length|>float)/(nbytespersamp|>float)/(nchans|>float)/(blocksize|>float) |> ceil |> int
    let numblocks = (totalblocks|>float)/1.0 |> ceil |> int

    if (Array.length argv = 2) then
        let aP = agentProcessor ()
        let aS = agentSerialBasic ()
        let duration = (FileInfo(src_file).Length|>float)/2.0/64.0/30000.0/3600.0   
        printfn "Converting file %s (%.2f hours)" fname duration    
        File.Create(dst_file).Close()
        [0..numblocks..totalblocks-1]
        |> List.map (fun startblock ->
                        async { 
                            aS.Post (fun _ -> printfn "Starting %d" startblock)
                            use p = 
                                new Process(StartInfo = ProcessStartInfo(FileName = System.Reflection.Assembly.GetExecutingAssembly().Location,
                                                Arguments = sprintf "%s %s %d" src_file dst_path startblock, UseShellExecute = false))
                            p.Start() |> ignore
                            p.WaitForExit()
                            aP.Post CompletedProcessing
                            aS.Post (fun _ -> printfn "Completing %d" startblock)
                        })
        |> List.iter (fun c -> aP.Post(ProcessData c))
        AsyncLoopUntil 2000 (fun () -> (aP.PostAndReply (fun reply -> GetNumInProgress reply)) = 0)
        |> Async.RunSynchronously
    else
        let startblock = argv.[2]|>int
        use fs_src = new FileStream(src_file,FileMode.Open,FileAccess.Read)    
        use fs_dst = new FileStream(dst_file,FileMode.Open,FileAccess.Write,FileShare.ReadWrite)
        let numbytestoread = blocksize*nbytespersamp*nchans
        let startpos = (startblock|>int64)*(numbytestoread|>int64)
        let curpos = fs_src.Seek(startpos,SeekOrigin.Begin)
        if (curpos = startpos) then
            fs_dst.Seek(startpos,SeekOrigin.Begin) |> ignore
            let b_src = Array.zeroCreate<byte> numbytestoread
            let b_dst = Array.zeroCreate<byte> numbytestoread
            let rec processBlock i =
                let numbytesread = fs_src.Read(b_src,0,numbytestoread)
                for chnum in 0..nchans-1 do
                    for i in 0..(numbytesread/nbytespersamp/nchans)/nsampsperblock-1 do
                        let start_offset = i*nbytespersamp*nsampsperblock*nchans + chnum*nbytespersamp*nsampsperblock
                        for j in 0..nsampsperblock-1 do 
                            let src_pos = start_offset+j*nbytespersamp
                            let dst_pos = nchans*((i*nbytespersamp*nsampsperblock+j*nbytespersamp))+chnum*nbytespersamp
                            for k in 0..nbytespersamp-1 do
                                b_dst.[dst_pos+k] <- b_src.[src_pos+k]
                fs_dst.Write(b_dst,0,numbytesread)
                if ((numbytesread = numbytestoread) && (i+1) < numblocks) then processBlock (i+1)
            processBlock 0
    0
