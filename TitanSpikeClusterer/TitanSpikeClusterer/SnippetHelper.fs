module SnippetHelper

open RP.HDF5.H5TypeProviderRuntime

type binSearchResult = Exact of int64 | LargerThan of int64 | TooSmall
type ComparisonResult = TargetIsSmaller|TargetIsLarger|Match
let inline compare target x = 
    match x with 
    | _ when x < target -> TargetIsLarger
    | _ when x > target -> TargetIsSmaller
    | _ -> Match 

let binSearch targetfunc min max =
    let rec binSearch' min max =  
        if (max < min) then
            if (min = 0L) then TooSmall else LargerThan max
        else
            let middle = (min + max) / 2L
            match targetfunc middle with
            | TargetIsSmaller ->
                binSearch' min (middle-1L)
            | TargetIsLarger ->
                binSearch' (middle+1L) max
            | Match ->
                Exact middle
    binSearch' min max

let getSpikesIndex t0 t1 h5fname chgroup = 
    let spikestr = sprintf "./ChGroup_%s/Spikes" chgroup
    let spiketimestr = sprintf "./ChGroup_%s/SpikeTimes" chgroup
    let dims = getDataSetDimensions h5fname spikestr
    let inline st i = (readData h5fname spiketimestr [|i|>uint64|] null [|1UL|] :?> int64[]).[0]
    let startindx = match (binSearch (st>>(compare t0)) 0L ((dims.[0]|>int64)-1L)) with
                    | Exact i -> Some i
                    | LargerThan i -> if (i = (dims.[0]|>int64)-1L) then None else Some (i+1L)
                    | TooSmall -> Some 0L
    if (startindx.IsNone) then None else
        let count = match (binSearch (st>>(compare t1)) 0L ((dims.[0]|>int64)-1L)) with
                    | Exact i -> Some (i - startindx.Value + 1L)
                    | LargerThan i -> Some (i - startindx.Value)
                    | TooSmall -> None
        if (count.IsNone || (count.Value < 1L)) then None else
            Some (startindx.Value|>uint64, count.Value|>uint64)

let getSpikes fname chgroup maxtoload totaltoread startoffset =
    let path = sprintf @"./ChGroup_%s/" chgroup
    let dims = getDataSetDimensions fname (path + "Spikes")
    let numchannels = int dims.[2]
    let spikelen = int dims.[1]
    let stride = 1 + int ((totaltoread-1UL)/maxtoload) 
    let numspikes = int ((totaltoread-1UL)/(uint64 stride)+1UL)
    let spiketimes = Array.zeroCreate<int64> numspikes
    let spikesraw = Array3D.zeroCreate<int16> numspikes spikelen numchannels
    let numperread = uint64 ((int maxtoload/stride)*stride)

    for offset in startoffset..numperread..totaltoread+startoffset-1UL do
        let numtoread = (min (min numperread (totaltoread+startoffset-offset)) (dims.[0]-offset))
        let cur_spiketimes = readData fname (path + "SpikeTimes") [|offset|] null [|numtoread|] :?> int64[]
        for i in 0..(int (numtoread-1UL)/stride) do
            spiketimes.[int (offset-startoffset)/stride+i] <- cur_spiketimes.[i*stride]
        let cur_spikesraw = readData fname (path + "Spikes") [|offset;0UL;0UL|] null [|numtoread;uint64 spikelen;uint64 numchannels|] :?> int16[,,]
        for i in 0..(int (numtoread-1UL)/stride) do
            for j in 0..spikelen-1 do
                for k in 0..numchannels-1 do
                    spikesraw.[int (offset-startoffset)/stride+i,j,k] <- cur_spikesraw.[i*stride,j,k]

    let spikes =
            Array.init numspikes (fun i -> spiketimes.[i],
                                            Array2D.init spikelen numchannels (fun j chnum -> spikesraw.[i,j,chnum]))
    spikes,spikesraw

let scalespike (st:int64,s:int16[]) = (float st)/30000.0, s |> Array.map (fun s -> (float s)*0.195e-6)
