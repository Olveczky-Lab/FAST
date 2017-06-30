#if COMPILED
module PlotSpikeAmplitudes
#else

#load "Helper.fs"
#load "HDF5Helper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open System.IO
open RP.HDF5.H5TypeProviderRuntime
open RP.Controls
open System.Windows.Media
open System.Windows
open Helper
open PlotHelper
open System

let path = @"C:\EphysData\Sullivan\Ashesh\Bairagi"
let fnames = 
    let tmp = 
        let fnums = File.ReadAllLines(sprintf "%s\DataFileNums.txt" path)
        fnums
        |> Array.sort
        |> Array.map (fun fnum -> 
                        let datafname = sprintf @"%s\%s.h5" path fnum
                        datafname,fnum |> int64)
    let (_,first) = tmp.[0]
    tmp |> Array.map (fun (fname,t) -> (fname,((t-first)|>float)/1e7))
             

let getSpikeAmps fname chgroup dfact numperread spike_max_indx =
    let path = sprintf @"./ChGroup_%s/" chgroup
    let dims = getDataSetDimensions fname (path + "Spikes")
    printfn "%s : NumSpikes = %d" fname dims.[0]
    let numchannels = int dims.[2]
    let spikelen = int dims.[1]
    if (dims.[0] > 0UL) then
        let data = 
            [|0UL..numperread..dims.[0]-1UL|]
            |> Array.map (fun offset ->
                            let numtoread = (min numperread (dims.[0]-offset))
                            let cur_spiketimes = readData fname (path + "SpikeTimes") [|offset|] null [|numtoread|] :?> int64[]
                            let cur_spikesraw = readData fname (path + "Spikes") [|offset;0UL;0UL|] null [|numtoread;uint64 spikelen;uint64 numchannels|] :?> int16[,,]
                            Array.init numchannels (fun chnum ->
                                                        Array.init ((int numtoread)/dfact) 
                                                                   (fun i -> (float cur_spiketimes.[i*dfact])/30000.0,
                                                                             (float cur_spikesraw.[i,spike_max_indx,chnum])*0.195e-6)))
        Some (Array.init numchannels (fun chnum -> Array.init (Array.length data) (fun i -> data.[i].[chnum]) |> Array.concat))
    else None
let plotchannel (spikeamps:(float*float)[]) col =
    let wnd = new D3DPlot2DWindow()
    wnd.Width <- 2048.0
    wnd.Height <- 1152.0
    wnd.Left <- 2048.0
    wnd.Top <- 0.0
    wnd.Show()
    let (tmax,_) = spikeamps.[spikeamps.Length-1]
    let xrange = Range(0.0,tmax)
    addTimeXAxis wnd xrange |> ignore
    let yrangespikeamp = Range(0.0,1.0e-3)
    addScaledYAxis wnd yrangespikeamp col (sprintf "Spike Amplitude (%sV)") |> ignore
    let ampl = getPoints 1.0f col spikeamps
    addSubPlot wnd xrange yrangespikeamp [ampl] |> ignore
    wnd.KeyDown.Add (fun args ->let delta_s, delta_a = 
                                        match args.Key with
                                        | Input.Key.D -> 1.0f,0
                                        | Input.Key.A -> -1.0f,0
                                        | Input.Key.S -> 0.0f,-5
                                        | Input.Key.W -> 0.0f,5
                                        | _ -> 0.0f,0
                                ampl.Width <- ampl.Width + delta_s
                                ampl.Height <- ampl.Height + delta_s
                                ampl.Color <- Color.FromArgb(byte (clamp 0 255 ((int ampl.Color.A) + delta_a)),ampl.Color.R,ampl.Color.G,ampl.Color.B)
                                wnd.Plot.ForceRefresh())
    wnd

let r = 
    let chgroup,chnum = "26",0
    let data = 
        fnames.[8..8]
        |> Array.choose (fun (fname,offset) ->                            
                            match (getSpikeAmps fname chgroup 20 1000000UL 31) with
                            | Some samps ->
                                Some (samps |> Array.map (Array.map (fun (st,amp) -> st+offset,amp)))
                            | None -> None)
    let numchannels = data.[0].Length
    let spikeamps = Array.init (Array.length data) (fun i -> data.[i].[chnum]) |> Array.concat
    let wnd = plotchannel spikeamps Colors.Black
    wnd.Title <- sprintf "ChGroup_%s_%d" chgroup chnum

                                

#if COMPILED
[<STAThread()>]
(new Application()).Run() |> ignore
#endif