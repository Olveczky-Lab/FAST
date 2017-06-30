module SnippetHelper

open RP.HDF5.H5TypeProviderRuntime

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
