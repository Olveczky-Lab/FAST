#if COMPILED
module ComputeLFPPower
#else

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.IO.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.FSharp.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\IntelIPP.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"

#load "Helper.fs"
#load "AmpDataHelper.fs"
#load "AgentHelper.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open RP.HDF5.H5TypeProviderRuntime
open AmpDataHelper
open AgentHelper
open RP.HDF5

//matlab [b,a] = cheby1(4,.05,[0.5 4]*2/300)
let filtercoeffs_slow =
    let a = [|1.000000000000000; -7.838477634252449; 26.890504144338735; -52.733515615271358; 64.656799395723652; -50.755405889072755; 24.911045614820726; -6.989174495128440; 0.858224478842430; |]
    let b = [|0.000001942563876; 0.000000000000000; -0.000007770255503; 0.000000000000000; 0.000011655383255; 0.000000000000000; -0.000007770255503; 0.000000000000000; 0.000001942563876; |]
    (a,b)

let filtercoeffs_fast = 
    let a = [|1.000000000000000; -7.797949290804811; 26.631786397013833; -52.028929647394413; 63.596552995551590; -49.804393968968384; 24.403300744738097; -6.840056648105682; 0.839689418672426; |]
    let b = [|0.000003278938655; 0.000000000000000; -0.000013115754619; 0.000000000000000; 0.000019673631929; 0.000000000000000; -0.000013115754619; 0.000000000000000; 0.000003278938655; |]
    (a,b)

let fname = @"C:\EphysData\Sullivan\Ashesh\635025802044762119.h5"
let dsname = "./LFP"
let srlfp = 300.0
let numsamplesperblock = srlfp*30.0*60.0|>uint64
let totalnumsamples = srlfp*79.0*3600.0|>uint64
let dfactlfp = (srlfp) |> int
let chnums = [|7|]
let filtercoeffs = filtercoeffs_fast

let loadRawData offset = 
    let dims = getDataSetDimensions fname dsname
    readData fname dsname [|offset;0UL|] null [|numsamplesperblock;dims.[1]|] :?> int16[,]

let processRawData (data_raw:int16[,]) =
    let numsamples = (Array2D.length1 data_raw)
    let getdata ch = 
        let r = Array.init numsamples (fun i -> (float data_raw.[i,ch])*3.74e-5)
        r |> filtfilt filtercoeffs |> Array.map (fun x -> x*x)
    let numchs = chnums |> Array.length
    let datas = Array.init numchs (fun i -> async {return getdata chnums.[i]}) |> Async.Parallel |> Async.RunSynchronously
    Array.init numsamples (fun i -> (Array.init numchs (fun j -> datas.[j].[i]) |> Array.sum)/(float numchs))
    |> downsampleAvg dfactlfp

let power = Array.zeroCreate<float> (int (totalnumsamples/(uint64 dfactlfp)))
 
let powercompute =
 async {
     let aP = agentProcessor ()
     let aS = agentSerial ()
     let computations = 
         seq   {for offset in 0UL..numsamplesperblock..totalnumsamples-1UL do
                    let data_raw = loadRawData offset
                    let blocknum = (offset/numsamplesperblock)
                    printfn "Staring Block %d" blocknum
                    let datapower = 
                        async { do
                                    let dr = processRawData data_raw
                                    aS.PostAndReply(fun reply -> 
                                                        (fun () -> 
                                                            for dsnum in 0..(numsamplesperblock|>int)/dfactlfp-1 do
                                                                power.[dsnum+((offset/(uint64 dfactlfp))|>int)] <- dr.[dsnum]
                                                            printfn "Completed Block %d" blocknum
                                                            aP.Post CompletedProcessing
                                                            ((),())),reply)}
                    yield datapower}
     for comp in computations do
        do! AsyncLoopUntil 1000 (fun () -> aP.PostAndReply (fun reply -> GetNumInProgress reply) < 50)
        aP.Post(ProcessData comp)
     do! AsyncLoopUntil 2000 (fun () -> (aP.PostAndReply (fun reply -> GetNumInProgress reply)) = 0)
 }

Async.RunSynchronously powercompute

[<Literal>]
let outputfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\Processed.h5"
type h5file = HDF5File<outputfname>
h5file.LFPPower.WriteData(null,null,power)