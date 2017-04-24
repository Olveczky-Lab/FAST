#if COMPILED
module ExtractChannelData
#else
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.IO.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.FSharp.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\IntelIPP.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"

#load "Helper.fs"
#load "AmpDataHelper.fs"
#load "AgentHelper.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open System.IO
open System
open AmpDataHelper
open RP.HDF5.H5TypeProviderRuntime
open Helper
open AgentHelper

let calcThresholds blocksize b chnum =
    let d = scaleData 32768.0 blocksize b chnum |> filtfilt spikes_bandpass
    let mad = MAD d
    let sd = mad*1.4826
    (sd*5.0,sd*2.0)

let numtotalchannels = 64    

let extractData fnum channelgroups = 
    let path = @"\\Ephys1\g\Data\"
    let fname = path + fnum + ".amp"
    let outputfname = @"C:\EphysData\Sullivan\Ashesh\Striatum\" + fnum + ".h5"
    let sr = 30000 // sampling rate
    let f = new FileInfo(fname)
    let dfact = 100 //downsamplefactor for LFP

    let getThresholds () =
        let blocksize = sr*10 //10seconds
        let b = Array.zeroCreate<byte> (blocksize*2*64)
        use fs = f.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        fs.Read(b,0,b.Length) |> ignore
        List.init numtotalchannels (fun chnum -> async { return calcThresholds blocksize b chnum })
        |> Async.Parallel |> Async.RunSynchronously

    let blocksize = sr*30 //30 seconds
    let endPadding = sr/10 // 100 ms
    let firstblock = 0UL // 0UL
    let numblocks = 2UL //(uint64 f.Length)/2UL/(uint64 numtotalchannels)/(uint64 blocksize)
    let ts = getThresholds ()
    let aP = agentProcessor ()
    let aS = agentSerial (firstblock,List.init (List.length channelgroups) (fun i -> 0UL),[])
            
    let processDataBlock curBlock b =
        async {
            let data = 
                channelgroups 
                |> List.map 
                    (fun (chname,chnums) -> 
                        let extractSpikes thrs =
                            let ds =
                                List.zip thrs chnums
                                |> List.map 
                                    (fun ((event_thr,return_thr),chnum) ->
                                        let d_s =
                                            scaleData (32768.0) (blocksize+endPadding*2) b chnum
                                        let downsampled_d = d_s |> downsample100
                                        let d = d_s |> filtfilt spikes_bandpass
                                        (event_thr,return_thr,d),downsampled_d.[endPadding/dfact..(endPadding+blocksize)/dfact-1])
                            let spikes = ds |> List.map fst |> List.toArray |> getSpikeSnippets endPadding |> List.toArray
                            let spiketimes = spikes |> Array.map (fun (_,t) -> (int64 t) + (int64 curBlock)*(int64 (blocksize)))
                            let numspikes = Array.length spikes
                            let s = Array3D.init numspikes (w_pre+w_post+1) (List.length chnums) 
                                            (fun spikenum samplenum chindx -> 
                                                let (s,_) = spikes.[spikenum]
                                                int16 (s.[chindx,samplenum]))
                            ds |> List.map snd ,s,spiketimes
                        async {return extractSpikes (chnums |> List.map (fun chnum -> ts.[chnum]))})
                |> Async.Parallel
                |> Async.RunSynchronously

            let saveDataBlock curspikecounts =            
                let lfps2D =
                    let lfps = Array.zeroCreate<float[]> numtotalchannels
                    Array.zip (channelgroups |> List.toArray) data
                    |> Array.iter (fun ((_,chnums),(lfp,_,_)) -> chnums |> List.iteri (fun i chnum -> lfps.[chnum] <- lfp.[i]))                                                                                
                    Array2D.init (blocksize/dfact) numtotalchannels (fun i j -> int16 lfps.[j].[i])                                                                         
                writeData outputfname "./LFP" [|((endPadding/dfact)|>uint64) + curBlock*(uint64 (blocksize/dfact));0UL|] null lfps2D
                let newspikecounts = 
                    List.zip3 channelgroups (data |> Array.toList) curspikecounts
                    |> List.map (fun ((chname,chnums),(_,s,sts),cnt) -> 
                                    let chstr = "./ChGroup_" + chname
                                    writeData outputfname
                                                (chstr+"/ChNums")
                                                null
                                                null
                                                (chnums |> List.toArray)
                                    writeData outputfname 
                                                (chstr+"/SpikeTimes") 
                                                [|cnt|]
                                                null
                                                sts
                                    writeData outputfname 
                                                (chstr+"/Spikes") 
                                                [|cnt;0UL;0UL|] 
                                                null
                                                s
                                    let numspikes = Array.length sts
                                    cnt+(uint64 numspikes))
                printfn "Complete Block %d of %d" curBlock numblocks
                aP.Post CompletedProcessing
                newspikecounts

            let rec saveall blocknumtosave curspikecounts xs =
                match xs with
                | (xblocknum,savefunc)::ys when (xblocknum = blocknumtosave) ->
                        saveall (blocknumtosave+1UL) (savefunc curspikecounts) ys
                | _ -> (blocknumtosave,curspikecounts,xs)
            
            if (curBlock = 0UL) then
                do! Async.Sleep(3000)
            aS.PostAndReply(fun reply ->
                                (fun (blocknumtosave,curspikecounts,xs) -> 
                                    let newxs = (curBlock,saveDataBlock)::xs |> List.sortBy fst
                                    let r = saveall blocknumtosave curspikecounts newxs
                                    r,()),reply)
            |> ignore
        }
                        
    use fs = f.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)

    let rec extractSpikes curBlock =
        async {
            if (curBlock < numblocks) then
                do! AsyncLoopUntil 1000 (fun () -> aP.PostAndReply (fun reply -> GetNumInProgress reply) < 5)
                printfn "Starting Block %d of %d" curBlock numblocks
                fs.Seek((curBlock|>int64)*(blocksize|>int64)*2L*64L,SeekOrigin.Begin) |> ignore
                let b = Array.zeroCreate<byte> ((blocksize+endPadding*2)*2*64)
                fs.Read(b,0,b.Length) |> ignore
                aP.Post(ProcessData (processDataBlock curBlock b))
                do! extractSpikes (curBlock+1UL)
            else
                do! AsyncLoopUntil 2000 (fun () -> (aP.PostAndReply (fun reply -> GetNumInProgress reply)) = 0)
        }

    extractSpikes firstblock |> Async.RunSynchronously

let fnums = Directory.GetFiles(@"C:\EphysData\Sullivan\Ashesh\Striatum","*.h5") |> Array.map (fun fname -> FileInfo(fname).Name.Split('.').[0])
for fnum in fnums do
    printfn "Extracting Spikes from file %s" fnum
    extractData fnum (loadnTrodes @"C:\EphysData\Sullivan\Ashesh\Striatum\EIBElectrodePlacement.xml")
