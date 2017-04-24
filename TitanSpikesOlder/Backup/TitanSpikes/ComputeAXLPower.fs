#if COMPILED
module PlotAXL
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

// matlab [b,a] = cheby2(4,.05,1*2/7500,'high');
let axl_highpass =
    let a = [|1.000000000000000; -3.998079815733305; 5.994241354644872; -3.994243261040201; 0.998081722129054; |]
    let b = [|0.993305966671120; -3.973223866684479; 5.959835800026718; -3.973223866684479; 0.993305966671120; |]
    (a,b)

let fname = @"C:\EphysData\Sullivan\Ashesh\635025802044762119.h5"
let sraxl = 7500.0
let numsamplesperblock = sraxl*1.0*60.0|>uint64
let totalnumsamples = sraxl*79.0*3600.0|>uint64
let dfactaxl = (sraxl) |> int

let axlpower = Array.zeroCreate<float> (int (totalnumsamples/(uint64 dfactaxl)))
 
let axlpowercompute =
 async {
     let aP = agentProcessor ()
     let aS = agentSerial ()
     let computations = 
         seq   {for offset in 0UL..numsamplesperblock..totalnumsamples-1UL do
                    let axl_raw = readData fname "./AXL" [|offset;0UL|] null [|numsamplesperblock;3UL|] :?> uint16[,]
                    printfn "Staring hour %.2f" ((float offset)/sraxl/3600.0)
                    let axlpower = 
                        async { do
                                    let numsamples = (Array2D.length1 axl_raw)
                                    let numchs = Array2D.length2 axl_raw
                                    let getaxl ch = 
                                        let r = Array.init numsamples (fun i -> (float axl_raw.[i,ch])*3.74e-5)
                                        r |> filtfilt axl_highpass |> Array.map (fun x -> x*x)
                                    let axls = Array.init numchs (fun i -> async {return getaxl i}) |> Async.Parallel |> Async.RunSynchronously
                                    let dr = 
                                        Array.init numsamples (fun i -> (Array.init numchs (fun j -> axls.[j].[i]) |> Array.sum)/(float numchs))
                                        |> downsampleAvg (dfactaxl)
                                    aS.PostAndReply(fun reply -> 
                                                        (fun () -> 
                                                            for dsnum in 0..(numsamplesperblock|>int)/dfactaxl-1 do
                                                                axlpower.[dsnum+((offset/(uint64 dfactaxl))|>int)] <- dr.[dsnum]
                                                            printfn "Completed hour %.2f" ((float offset)/sraxl/3600.0)
                                                            aP.Post CompletedProcessing
                                                            ((),())),reply)}
                    yield axlpower}
     for comp in computations do
        do! AsyncLoopUntil 1000 (fun () -> aP.PostAndReply (fun reply -> GetNumInProgress reply) < 50)
        aP.Post(ProcessData comp)
     do! AsyncLoopUntil 2000 (fun () -> (aP.PostAndReply (fun reply -> GetNumInProgress reply)) = 0)
 }

Async.RunSynchronously axlpowercompute

[<Literal>]
let outputfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\Processed.h5"
type h5file = HDF5File<outputfname>
h5file.AXLPower.WriteData(null,null,axlpower)


