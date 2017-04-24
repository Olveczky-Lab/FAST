module ComputeAXLPower

open MathHelper
open RP.HDF5.H5TypeProviderRuntime

// matlab [b,a] = cheby2(4,.05,1*2/7500,'high');
let axl_highpass =
    let a = [|1.000000000000000; -3.998079815733305; 5.994241354644872; -3.994243261040201; 0.998081722129054; |]
    let b = [|0.993305966671120; -3.973223866684479; 5.959835800026718; -3.973223866684479; 0.993305966671120; |]
    (a,b)

let axl_bandpass =
    let filta = [|1.000000000000000; -3.799990733199073; 5.421056152488188; -3.442138848781477; 0.821073429537006; |]
    let filtb = [|0.014259737441850; -0.036210647908830; 0.043901820934406; -0.036210647908830; 0.014259737441850; |]
    (filta,filtb)

let getaxlpower fname offset count =
    let axl_raw = readData fname "./AXL" [|offset;0UL|] null [|count;3UL|] :?> uint16[,]
    let numsamples = (Array2D.length1 axl_raw)
    let numchs = Array2D.length2 axl_raw
    let getaxl ch = 
        let r = Array.init numsamples (fun i -> (float axl_raw.[i,ch])*3.74e-5)
        r |> filtfilt axl_highpass |> Array.map (fun x -> x*x)
    let axls = Array.init numchs (fun i -> async {return getaxl i}) |> Async.Parallel |> Async.RunSynchronously
    Array.init numsamples (fun i -> (Array.init numchs (fun j -> axls.[j].[i]) |> Array.sum)/(float numchs))
    |> downsample100
 
