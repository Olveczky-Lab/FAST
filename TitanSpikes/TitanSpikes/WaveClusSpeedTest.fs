#if COMPILED
module WaveClusSpeedTest
#else
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open Cluster
open Helper

let featurefunc spikesraw spikestocluster =
    let nchans = Array3D.length3 spikesraw
    let waveletfeatures = getFeatures spikesraw spikestocluster [|getwavelets (Array.init nchans id) 64|]
                            |> cullfeaturesks 40
    let ampfeatures = getFeatures spikesraw spikestocluster [|getspikeamp (Array.init nchans (fun i -> (31,i)))>>(Array.map (fun x -> x*2.0))|]
    Array.zip waveletfeatures ampfeatures |> Array.map (fun (x,y) -> Array.append x y)            

let cnt = 10000UL
let spikesraw,_ = getSpikesRaw @"Z:\badlands\635276396558304920_0.h5" "0" cnt 0UL 
let features = featurefunc spikesraw [|0..(cnt|>int)-1|]
let nspikes = Array.length features
let clus1 = wave_clus 0.01 0.1 0.01 100 11 1 @"C:\temp\temp" features false
let distances = 
    Array.init nspikes (fun i -> 
        Array.init nspikes (fun j -> 
            Array.map2 (fun x y -> (x-y)*(x-y)) features.[i] features.[j] |> Array.sum |> sqrt) |> Array.sort)
let clus2 = wave_clus 0.01 0.1 0.01 300 11 1 @"C:\temp\temp" distances true

