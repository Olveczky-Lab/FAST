module SimHelper

type UpdateFunc = int*int*int->int*int*int->int*int
let inline n x = System.Nullable x
let inline trygetval (x:System.Nullable<_>) = if (x.HasValue) then Some (x.Value) else None

let maxChange = 50; //50 ms maximum change
let toughRange = 150; // once range is 150ms, only change by 10ms
let intervalIncrement = 10; //Amount to shift if slowly improving (10ms)
let maxInterval = 1100; //Max intertap interval
let minTrials = 50
let minPercent = 30
let maxPercent = 40

let inline relaxUpper upper =
    min (upper+intervalIncrement) maxInterval

let inline makeEasier (lower,target,upper) (_,med,_) =
    match med with
    | _ when med < target -> // too fast - relax lower bound
        max (lower-intervalIncrement) 0,upper
    | _ -> //too slow - relax upper bound
        lower,relaxUpper upper
    
let inline makeHarder (lower,target,upper) (q1,med,q3) =
    match med with
    | _ when med < target || lower < target -> // relax upper bound and toughen lower bound
        let newupper = relaxUpper upper
        let newlower = (max lower q1)+intervalIncrement |> min target |> min (lower+maxChange)
        (if newupper-newlower <= toughRange then lower+intervalIncrement |> min target else newlower),newupper
    | _ -> // toughen upper bound
        let newupper = (min upper q3)-intervalIncrement |> max target |> max (upper-maxChange)
        target, (if newupper-target <= toughRange then upper-intervalIncrement |> max target else newupper)

let (|Good|Hard|Easy|) (nTrials,nRewarded) : Choice<unit,UpdateFunc,UpdateFunc> = 
    if nTrials < minTrials then Good else
    let frac = (nRewarded|>float)/(nTrials|>float)*100.
    if (frac < (minPercent|>float)) then Hard makeEasier else
    if (frac > (maxPercent|>float)) then Easy makeHarder else Good

let inline updateBoundaries (lower,target,upper) ipis =
    let nTrials = Array.length ipis
    let nRewarded = ipis |> Array.filter (fun x -> x >=lower && x <= upper) |> Array.length
    match (nTrials,nRewarded) with
    | Hard f | Easy f ->
        let (q1,med,q3) as q = 
            ipis
            |> Array.sort
            |> fun x -> 
                let nx = Array.length x
                x.[nx/4]|>int,x.[nx/2]|>int,x.[nx*3/4]|>int
        let newlower,newupper = f (lower,target,upper) q
        (newlower,target,newupper)
    | _ -> (lower,target,upper)
