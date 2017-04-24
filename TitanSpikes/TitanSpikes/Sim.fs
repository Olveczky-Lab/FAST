#if COMPILED
module Sim
#else
#load "SimHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open System
open SimHelper
open PlotHelper

let inline norm_pdf pdf = 
    let s = pdf |> Array.sum
    pdf |> Array.map (fun a -> a/s)

let inline update kernel center pdf =
    let n = (Array.length kernel - 1)/2
    for i in (center-n |> max 0) .. (center+n |> min (Array.length pdf - 1)) do    
        let x = kernel.[i-center+n]
        pdf.[i] <- pdf.[i] + (if x > 0. then (1.-pdf.[i])*x else (pdf.[i]*x)) |> max 0.
    pdf |> norm_pdf

let inline transfermass f (i0,i1) (pdf:float[]) =
    let x = pdf.[i0..i1] |> Array.sum
    //printfn "%.2f" x
    let amt = if f > 0. then (1.0-x)*f else x*f
    let a = (x+amt)/x
    let b = (1.-x-amt)/(1.-x)
    //printfn "%.2f %.2f" a b
    for i in 0..Array.length pdf - 1 do
        let r = if i >= i0 && i <= i1 then a else b
        pdf.[i] <- r*pdf.[i] |> max 1e-5
    pdf |> norm_pdf

let inline cumsum x = Array.scan (+) 0.0 x |> fun x -> x.[1..]

let rand =
    let r = new Random()
    fun () -> r.NextDouble()

let sim_ipis =
//    let reward_kernel = Array.init 50 (fun i -> 1./10000.)
//    let noreward_kernel = Array.init 5 (fun i -> -1./30.)
    let target = 700
    let rec loop_sm pdf (lower,upper) n = seq {
        printfn "%d %d" lower upper
        let rec loop_trial results pdf n = 
            if n = 0 then (pdf,(results|>List.rev|>List.toArray)) else
            let cdf = cumsum pdf
            let i = 
                match Array.BinarySearch(cdf,rand ()) with
                | i when i < 0 -> ~~~i
                | i -> i 
            let getrange r = (i-r|>max 0,i+r|>min (Array.length pdf - 1))
            let f = 0.0005
            let newpdf = 
                if i > lower && i < upper then 
                    //update reward_kernel i pdf 
                    //transfermass f (getrange 150) pdf
                    pdf
                else 
                    //update noreward_kernel i pdf
                    transfermass (-f) (getrange 150) pdf
                    //pdf
            loop_trial (i::results) newpdf (n-1)
        let newpdf,results = loop_trial [] pdf 250
        let (newlower,_,newupper) = updateBoundaries (lower,target,upper) results
        yield! results
        if n = 0 then () else yield! loop_sm newpdf (newlower,newupper) (n-1)
    }
    let pdf = 
        let x = Array.init 1200 (fun _ -> 1./1200.)
//        for i in 500..549 do
//            x.[i] <- x.[i] + 5./1200.
        x |> norm_pdf
    loop_sm pdf (200,1100) 100 |> Seq.toArray |> Array.map float

let plot = 
    let ((y0,y1),yn) as yp = (0.,1200.),1000
    let (_,xn) as xp = (0.0,Array.length sim_ipis-1|>float),1000
    let x = imhist (4.f,4.f) xp yp (sim_ipis |> Array.mapi (fun i x -> i|>float,x))
    let x' = x |> convert2Dto1D
    let img = getheatmap (x' |> Array.min, x' |> Array.max) x
    dispimg img
