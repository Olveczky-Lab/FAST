module AmpDataHelper

open MathNet.Numerics.Interpolation.Algorithms
open MathNet.Numerics.LinearAlgebra.Double
open IntelIPP                

let w_pre = 31 //Number of samples in the spike before min
let w_post = 32 //Number of samples after min
let num_samples_return = 8 // Number of samples within return_thr to mark end of event

//8th order bandpass elliptical filter with sampling rate = 30kHz, fmin = 400Hz, fmax = 7500Hz
//matlab [b,a]=ellip(4,0.1,40,[400 7500]*2/30000);
let spikes_bandpass = 
    let a = [|1.000000000000000; -3.846459248924393; 6.400008256018882; -6.897994681914004; 6.012175156527787; -4.006309579443387; 1.743002745445204; -0.498357335624322; 0.094028800349336|]
    let b = [|0.128663805693912; -0.126464955890528; -0.241840578157192; 0.050219351663251; 0.378845694505466; 0.050219351663249; -0.241840578157193; -0.126464955890527; 0.128663805693912|]
    (a,b)

//8th order cheybychev filter for 5 fold decimation
//matlab [b,a] = cheby1(8,.05,.8/5)
let decimate5_lowpass = 
    let a = [|1.000000000000000; -6.577646008494098; 19.390531070800172; -33.402319261839551; 36.727988359849093; -26.372286620099054; 12.068094264841948; -3.216287226855165; 0.382117314803825|]
    let b = [|0.000000745279514; 0.000005962236109; 0.000020867826382; 0.000041735652763; 0.000052169565954; 0.000041735652763; 0.000020867826382; 0.000005962236109; 0.000000745279514|]
    (a,b)

//8th order cheybychev filter for 4 fold decimation
//matlab [b,a] = cheby1(8,.05,.8/4)
let decimate4_lowpass = 
    let a = [|1.000000000000000; -6.097112958371416; 16.934600476036053; -27.866038271992011; 29.630453733495955; -20.809092318786636; 9.414279089931320; -2.506737807064877; 0.300687219366244|]
    let b = [|0.000004035929289; 0.000032287434315; 0.000113006020101; 0.000226012040203; 0.000282515050254; 0.000226012040203; 0.000113006020101; 0.000032287434315; 0.000004035929289|]
    (a,b)

let getinitcoeffs ((a:float[]),(b:float[])) = 
    let init n x = List.init n (fun i -> x)
    let nfilt = (Array.length b)
    let nfact = 3*(nfilt-1) //length of edge transients
    let rows = [0..nfilt-2]@[1..nfilt-2]@[0..nfilt-3] |> List.toArray
    let cols = (init (nfilt-1) 0)@[1..nfilt-2]@[1..nfilt-2] |> List.toArray
    let vals = [1.0+a.[1]]@(a.[2..nfilt-1]|>Array.toList)@(init (nfilt-2) 1.0)@(init (nfilt-2) -1.0) |> List.toArray
    let rhs = DenseVector.init (nfilt-1) (fun i -> b.[i+1]-b.[0]*a.[i+1])
    let m = DenseMatrix(nfilt-1)
    for i in 0..(vals.Length-1) do
        m.[rows.[i],cols.[i]] <- vals.[i]
    m.QR().Solve(rhs) |> Vector.toArray

let filter (a,b) (d:float [])=
    let df = Array.zeroCreate<float> (Array.length d)
    let f = new IIRFilter(new IIRFilterCoefficients(a,b))
    f.Filter(d,df)
    df

let smooth windowSize (d:float []) =
    if (windowSize = 1) then d else
    Array.init ((Array.length d)-windowSize+1)
               (fun i -> (d.[i..i+windowSize-1]|>Array.sum)/(float windowSize))
    

let filtfilt (a,b) (d:float []) =
    let dlyLine = getinitcoeffs (a,b)
    let nfilt = (Array.length b)
    let nfact = 3*(nfilt-1) //length of edge transients
    let nd = d.Length
    
    let dt1 = Array.init nfact (fun i -> -d.[nfact-i] + 2.0*d.[0])
    let f = new IIRFilter(new IIRFilterCoefficients(a,b,dlyLine|>Array.map(fun x -> x*dt1.[0])))
    let dtf = Array.zeroCreate<float> nfact
    f.Filter(dt1,dtf)
    let df = Array.zeroCreate<float> nd
    f.Filter(d,df)
    let dt2 = Array.init nfact (fun i -> -d.[nd-2-i]+2.0*d.[nd-1])
    f.Filter(dt2,dtf)

    let frev = new IIRFilter(new IIRFilterCoefficients(a,b,dlyLine|>Array.map(fun x -> x*dtf.[nfact-1])))
    frev.Filter(dtf|>Array.rev,dtf)
    frev.Filter(df|>Array.rev,df)

    df |> Array.rev

let getSpikeSnippets endPadding ds =
    let numchs = Array.length ds
    let numsamples = 
        let (_,_,d) = ds.[0]
        Array.length d

    let rec is_within_thr chnum t0 = 
        if (chnum = numchs) then true else
        let (_,return_thr,d:float[]) = ds.[chnum]
        if (abs (d.[t0]) < return_thr) then  is_within_thr (chnum+1) t0 else false

    let rec getmax chnum t0 (curmax,curt,maxch) =
        if (chnum = numchs) then (curmax,curt,maxch) else
        let (_,_,d:float[]) = ds.[chnum]
        let x = abs (d.[t0])
        if (x > curmax) then  getmax (chnum+1) t0 (x,t0,chnum) else getmax (chnum+1) t0 (curmax,curt,maxch)

    let rec getReturnXings t0 max tmax maxch return_cnt =
        if (t0 >= numsamples) then None else
            if (return_cnt = num_samples_return || t0-tmax >= w_post-1) then Some (tmax,t0,maxch) else
                if (is_within_thr 0 t0) then 
                    getReturnXings (t0+1) max tmax maxch (return_cnt+1)
                else
                    let newmax, newt, newmaxch = getmax 0 t0 (max,tmax,maxch)
                    getReturnXings (t0+1) newmax newt newmaxch 0
    
    let rec is_outside_thr chnum t0 =
        if (chnum = numchs) then false else
        let (event_thr,_,d:float[]) = ds.[chnum]
        if (abs (d.[t0]) > event_thr) then true else is_outside_thr (chnum+1) (t0)
    
    let rec getSpikeTimes sts t0 =
        if (t0 >= numsamples) then sts else
            match (is_outside_thr 0 t0) with
            | true -> 
                let tmax =
                    let (_,_,d) = ds.[0]
                    abs d.[t0]                    
                let rx = getReturnXings (t0) tmax t0 0 0
                match rx with
                | Some (tmax,treturn,maxch) -> getSpikeTimes ((tmax,maxch)::sts) treturn
                | None -> sts
            | false -> getSpikeTimes sts (t0+1)            
    let all_sts = getSpikeTimes [] 0

    let left_endPadding = max (w_pre+1) endPadding
    let right_endPadding = max (w_post+2) endPadding
    let rec removeRightEnd sts = match sts  with
                                    | (h,chnum)::t -> if (numsamples-h) <= right_endPadding then removeRightEnd t else sts
                                    | _ -> []
    let rec removeLeftEnd sts = match sts with
                                | (h,chnum)::t -> if h < left_endPadding then removeLeftEnd t else sts
                                | _ -> []
    let sts = all_sts |> removeRightEnd |> List.rev |> removeLeftEnd
     
    //Cubic interpolate spikes to align them better
    sts |>
    List.map (fun (st,chnum) -> 
                let sps = ds 
                          |> Array.map (fun (_,_,d) -> 
                                            new CubicSplineInterpolation([|float (-w_pre-1)..float (w_post+1)|], d.[st-w_pre-1..st+w_post+1]))
                let sp = sps.[chnum]
                let x = vector [-1.0..0.25..1.0] |> (Vector.map (sp.Interpolate))
                let i = (float ((x |> Vector.map abs).MaximumIndex()))/4.0 - 1.0
                let newxs = [|i- float w_pre..i+ float w_post|]
                (Array2D.init (numchs) (w_pre+w_post+1) (fun ch_i samplenum -> sps.[ch_i].Interpolate(newxs.[samplenum])),
                 (float (st))+i))


let MAD d =
    let dn = d |> Array.map (fun x -> abs x)
    IPPS.sort(dn)
    dn.[dn.Length/2]

let decimate coeff dfact xs =
    let f = xs |> filtfilt coeff 
    Array.init (xs.Length/dfact) (fun i -> f.[i*dfact])

let downsample100 (xs:float []) =
    xs |> decimate decimate5_lowpass 5 |> decimate decimate5_lowpass 5 |> decimate decimate4_lowpass 4

let downsampleAvg dfact x =
    Array.init ((Array.length x)/dfact)
            (fun i -> (Array.sum x.[i*dfact..(i+1)*dfact-1])/(dfact|>float)) 
