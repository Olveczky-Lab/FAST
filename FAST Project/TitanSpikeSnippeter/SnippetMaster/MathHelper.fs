module MathHelper

open MathNet.Numerics.Interpolation.Algorithms
open MathNet.Numerics.LinearAlgebra.Double

let num_samples_return = 8 // Number of samples within return_thr to mark end of event

//4th order bandpass elliptical filter with sampling rate = 30kHz, fmin = 400Hz, fmax = 7500Hz
//matlab [b,a]=ellip(4,0.1,40,[400 7500]*2/30000);
let spikes_bandpass = 
    let a = [|1.000000000000000; -1.283257009295659; -0.038563797833481; 0.052423558517670; 0.273729147342332; |]
    let b = [|0.465847672427516; -0.006154167253708; -0.919343691360310; -0.006154167253708; 0.465847672427517; |]
    (a,b)

//4th order cheybychev filter for 5 fold decimation
//matlab [b,a] = cheby1(4,.05,.8/5)
let decimate5_lowpass = 
    let a = [|1.000000000000000; -2.762818097535585; 3.125716321218834; -1.670510887101431; 0.352862969179089; |]
    let b = [|0.002811910772015; 0.011247643088058; 0.016871464632087; 0.011247643088058; 0.002811910772015; |]
    (a,b)

//4th order cheybychev filter for 4 fold decimation
//matlab [b,a] = cheby1(4,.05,.8/4)
let decimate4_lowpass = 
    let a = [|1.000000000000000; -2.417348570701302; 2.553249621833365; -1.310743280241978; 0.273215961378886; |]
    let b = [|0.006113067145048; 0.024452268580194; 0.036678402870291; 0.024452268580194; 0.006113067145048; |]
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
    m.LU().Solve(rhs) |> Vector.toArray

let IIRFilter ((a:float[]),(b:float[])) (dlyLine:float[]) (x:float[]) (y:float[]) =    
    let nx = Array.length x
    let nf = Array.length a
    for i in 0..nx-1 do
        y.[i] <- b.[0]*x.[i] + dlyLine.[0]
        for j in 0..nf-3 do
            dlyLine.[j] <- b.[j+1]*x.[i] + dlyLine.[j+1] - a.[j+1]*y.[i]
        dlyLine.[nf-2] <- b.[nf-1]*x.[i] - a.[nf-1]*y.[i]

let filtfilt (a,b) (d:float []) =
    let dlyLine = getinitcoeffs (a,b)
    let nfilt = (Array.length b)
    let nfact = 3*(nfilt-1) //length of edge transients
    let nd = d.Length
    
    let dt1 = Array.init nfact (fun i -> -d.[nfact-i] + 2.0*d.[0])
    let dtf = Array.zeroCreate<float> nfact
    let forward_dlyLine = dlyLine|>Array.map(fun x -> x*dt1.[0])
    IIRFilter (a,b) forward_dlyLine  dt1 dtf
    let df = Array.zeroCreate<float> nd
    IIRFilter (a,b) forward_dlyLine d df
    let dt2 = Array.init nfact (fun i -> -d.[nd-2-i]+2.0*d.[nd-1])
    IIRFilter (a,b) forward_dlyLine dt2 dtf

    let reverse_dlyLine = dlyLine|>Array.map(fun x -> x*dtf.[nfact-1])
    IIRFilter (a,b) reverse_dlyLine (dtf|>Array.rev) dtf
    IIRFilter (a,b) reverse_dlyLine (df|>Array.rev) df

    df |> Array.rev

let getSpikeSnippets w_pre w_post endPadding ds =
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
     
    sts |>
    List.map (fun (st,chnum) -> 
                (Array2D.init (numchs) (w_pre+w_post+1) (fun ch_i samplenum -> 
                                                            let (_,_,d) = ds.[ch_i]
                                                            d.[st-w_pre+samplenum]),
                 (float st)))


let decimate coeff dfact xs =
    let f = xs |> filtfilt coeff 
    Array.init (xs.Length/dfact) (fun i -> f.[i*dfact])

let downsample100 (xs:float []) =
    xs |> decimate decimate5_lowpass 5 |> decimate decimate5_lowpass 5 |> decimate decimate4_lowpass 4

let bytespersample = 176
let bytespersampleOld = 128

let convertToUInt16Old b chnum =
    let newchnum = (chnum%32)*2+(chnum/32)
    Array.init (Array.length b/bytespersampleOld) (fun i -> System.BitConverter.ToUInt16(b,i*bytespersampleOld+newchnum*2))

let convertToUInt16 b chnum =
    //Array.init blocksize (fun i -> System.BitConverter.ToUInt16(b,chnum*2+i*64*2))
    let ampOffset = 8+4+12
    let newchnum = (chnum%32)*2+(chnum/32)
    Array.init (Array.length b/bytespersample) (fun i -> System.BitConverter.ToUInt16(b,i*bytespersample+ampOffset+newchnum*2))

let scaleData offset b chnum =
    convertToUInt16 b chnum
    |> Array.map (fun x -> (float x) - offset)

let scaleDataOld offset b chnum = 
    convertToUInt16Old b chnum
    |> Array.map (fun x -> (float x) - offset)

