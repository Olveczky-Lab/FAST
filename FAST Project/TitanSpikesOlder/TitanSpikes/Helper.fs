module Helper

open System
open RP.HDF5.H5TypeProviderRuntime
open IntelIPP
open System.Xml.Linq

let rnd = new Random()
let rndgauss = new RandGauss(DateTime.Now.Ticks|>uint32)

let rand n = Array.init n ((fun _ -> rnd.NextDouble()))
let randn n = 
    let a = Array.zeroCreate<float> n
    rndgauss.Generate(a)
    a

let clamp min max x = if (x < min) then min else if (x > max) then max else x

let xn n = XName.op_Implicit(n)
let loadnTrodes (fname:string) =
    XElement.Load(fname).Elements(xn "ElectrodeSet")
    |> Seq.map (fun x -> (x.Attribute(xn "Name").Value,x.Elements(xn "Channel")
                                                       |> Seq.map (int) |> Seq.toList))
    |> Seq.toList
    
let getOrderOfMag x =
    if (x = 0.0) then (x,"") else
    let order = clamp -5 3 (int (floor ((log10 (abs x))/3.0)))
    let prefix =  
        match order with
        | x when x = 3 -> "T"
        | x when x = 2 -> "G"
        | x when x = 1 -> "k"
        | x when x = 0 -> ""
        | x when x = -1 -> "m"
        | x when x = -2 -> Char.ConvertFromUtf32(956)
        | x when x = -3 -> "n"
        | x when x = -4 -> "p"
        | x when x = -5 -> "f"
        | _ -> failwith "Not Possible"
    1.0/(10.0**(float (3*order))), prefix


let getOrderOfMagTime t = //t is in seconds
    let at = abs t
    if (abs t < 24.0*60.0*60.0) then
        if (abs t < 60.0*60.0) then 
            if (abs t < 60.0) then
                if (abs t  < 1.0) then
                    let multiplier,s = getOrderOfMag t
                    multiplier,s+"s"
                else 1.0,"s"
            else 1.0/60.0,"m"
        else 1.0/60.0/60.0,"h"
    else 1.0/24.0/60.0/60.0,"d"


let (|Zero|Pos|Neg|) x = if (x = 0) then Zero else (if (x < 0) then Neg else Pos)

let intersect comparef l1 l2 =
    let rec rintersect xs ys xs_com xs_diff ys_com ys_diff = 
        match xs with
        | [] -> (xs_com,xs_diff,ys_com,ys@ys_diff)
        | hx::tx -> match ys with
                      | [] -> (xs_com,xs@xs_diff,ys_com,ys_diff)
                      | hy::ty -> match comparef hx hy with
                                    | Zero -> rintersect tx ty (hx::xs_com) xs_diff (hy::ys_com) ys_diff
                                    | Neg -> rintersect tx ys xs_com (hx::xs_diff) ys_com ys_diff
                                    | Pos -> rintersect xs ty xs_com xs_diff ys_com (hy::ys_diff)
    let (a,b,c,d) = rintersect l1 l2 [] [] [] []
    (a |> List.rev, b|> List.rev, c|> List.rev, d |> List.rev)

type binSearchResult = Exact of int64 | Internal of int64 | LeftEdge
let loadSpikesInWindow t0 t1 h5fname chgroup = 
    let spikestr = sprintf "./ChGroup_%s/Spikes" chgroup
    let spiketimestr = sprintf "./ChGroup_%s/SpikeTimes" chgroup
    let dims = getDataSetDimensions h5fname spikestr
    let binSearch target =
        let rec binSearch' target min max =  
            if (max < min) then
                if (min = 0L) then LeftEdge else Internal max
            else
                let middle = (min + max) / 2L
                let x = (readData h5fname spiketimestr [|middle|>uint64|] null [|1UL|] :?> int64[]).[0]
                if target < x then
                    binSearch' target min (middle-1L)
                else if target > x then
                    binSearch' target (middle+1L) max
                else 
                    Exact min
        binSearch' target 0L ((dims.[0]|>int64)-1L)
    let startindx = match (binSearch t0) with
                    | Exact i -> Some i
                    | Internal i -> if (i = (dims.[0]|>int64)-1L) then None else Some (i+1L)
                    | LeftEdge -> Some 0L
    if (startindx.IsNone) then [||] else
        let count = match (binSearch t1) with
                    | Exact i -> Some (i - startindx.Value + 1L)
                    | Internal i -> Some (i - startindx.Value)
                    | LeftEdge -> None
        if (count.IsNone || (count.Value < 1L)) then [||] else
            let st_raw = readData h5fname spiketimestr [|startindx.Value|>uint64|] null [|count.Value|>uint64|] :?> int64[]
            let s_raw = readData h5fname spikestr [|startindx.Value|>uint64;0UL;0UL|] null [|count.Value|>uint64;dims.[1];dims.[2]|] :?> int16[,,]
            Array.init (count.Value|>int) (fun i -> 
                                            ((float (st_raw.[i]-t0))/30000.0),
                                            (Array2D.init (Array3D.length2 s_raw) (Array3D.length3 s_raw) 
                                            (fun j chnum -> (float (s_raw.[i,j,chnum]))*0.195e-6)))

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
        let numtoread = (min numperread (dims.[0]-offset))
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
    let chnums = readData fname (path + "ChNums") null null null :?> int[]
    chnums,spikes,spikesraw

let scalespike (st:int64,s:int16[]) = (float st)/30000.0, s |> Array.map (fun s -> (float s)*0.195e-6)

//This assumes data array contains one sample of channel 0 followed by one sample of channel 1 ...
let convertToUInt16 blocksize b chnum =
    Array.init blocksize (fun i -> BitConverter.ToUInt16(b,chnum*2+i*64*2))

let scaleData offset blocksize b chnum =
    convertToUInt16 blocksize b chnum
    |> Array.map (fun x -> (float x) - offset)
