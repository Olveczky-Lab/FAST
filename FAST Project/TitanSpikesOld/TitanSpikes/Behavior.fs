#if COMPILED
module Behavior
#else

#load "MathHelper.fs"
#load "Helper.fs"
#load "HDF5Helper.fs"
#load "ClusterHelper.fs"
#load "PlotHelper.fs"
#load "BehaviorHelper.fs"
#load "EphysSync.fs"
#load "RemoveNoise.fs"
#load "ComputeAXLPower.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open EphysSync
open System.IO

let exptid = 2
let offset = 78
let h5path = @"C:\EphysData\Sullivan\Ashesh\Bairagi"
let fnums = File.ReadAllLines(sprintf "%s\DataFileNums.txt" h5path) |> Array.map int64
let clocks,nictrtimes = 
    let c,n = getClocks exptid offset fnums
    c, n|>List.toArray
let selchan chnum =
    nictrtimes |> Array.filter (fun x -> x.Channel = chnum) |> Array.map (fun x -> x.EventTime.NICtrTime)
let leverpresstimes = selchan 1

open Helper
open PlotHelper
open RP.Controls
open System.Windows.Media

open System.Xml.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Collections
open Linq.NullableOperators
open System.Data.Linq.SqlClient
open System

[<Literal>]
let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
type dbschema = SqlDataConnection<cstr>
let inline xn s = XName.Get(s)
let db = new dbschema.ServiceTypes.OpConASHESHDHAWALE1(cstr)

let nieventtimes = nictrtimes |> Array.map (fun x -> x.EventTime)
let getSampleNumInterval smid =
    let firstrceid = db.FirstRCE(Nullable exptid, Nullable smid).Value
    let lastrceid = db.LastRCE(Nullable exptid, Nullable smid).Value
    let f = getSampleNumFromRCEID clocks nieventtimes
    (f firstrceid,f lastrceid)


#if INTERACTIVE
#load "Cluster.fs"
#endif
open Cluster
open RP.HDF5.H5TypeProviderRuntime
open RemoveNoise
open ComputeAXLPower
open HDF5Helper

let padding = 0L*30000L //x seconds
for smid in [|52|] do    
    let f0,s0,s1 = 
        match (getSampleNumInterval smid) with
        | (Some (f0,s0),Some(f1,s1)) when f0=f1 -> f0,s0-30000L*1800L,s1+30000L*1800L
        | _ -> failwith "Oops"
    let fname = sprintf "%s\%d.h5" h5path f0
    //let axlpower = getaxlpower fname ((s0|>uint64)/4UL) ((s1-s0-1L|>uint64)/4UL)
    let channelgroups = [|1;5;7;8;10;13;14;15;20;21;22;23;26;28|] |> Array.map (sprintf "%d") 
    let spikeindices =
        channelgroups |> Array.map (fun chgroup ->
                            chgroup, Option.get <| getSpikesIndex (s0-padding) (s1+padding) fname chgroup)
    let non_noise = 
        let spiketimes =
            spikeindices |> Array.map (fun (chgroup,(spikeoffset,spikecount)) -> 
                                printfn "Reading %d spikes" spikecount 
                                readData fname ("./ChGroup_" + chgroup + "/SpikeTimes") [|spikeoffset|] null [|spikecount|] :?> int64[]
                                |> Array.toList)
        getCorrelationCountAll spiketimes
        |> Array.map (Array.map (fun x -> x <= 4))
    let leverpressintervals =
        leverpresstimes 
        |> Array.choose (getSampleNum clocks)
        |> Array.filter (fun (f,s) -> f = f0 && s >=s0 && s <= s1)
        |> Array.map snd
        |> fun x -> Array.init ((Array.length x)/2) (fun i -> x.[2*i],x.[2*i+1])

    let tmin = (s0|>float)/30000.0
    let tmax = (s1|>float)/30000.0

    let xrange = Range(tmin,tmax)

    let wnd = new D3DPlot2DWindow()
    wnd.Title <- sprintf "%d" smid
    wnd.Plot.SelectionBoxEnabled <- true
    wnd.Show()
    addTimeXAxis wnd xrange |> ignore

    let yrangelever = Range(-40.0,20.0)
    addYAxis wnd yrangelever Colors.DeepPink "Lever Presses" |> ignore
    leverpressintervals
    |> Array.map (fun (t1,t2) -> 
                    let scalet t = (float t)/30000.0
                    let (t1,t2) = scalet t1, scalet t2
                    let l = getLinexy Colors.DeepPink [t1;t1;t2;t2;t1] [0.0;1.0;1.0;0.0;0.0]
                    l)
    |> Seq.cast
    |> fun x -> addSubPlot wnd xrange yrangelever x
    |> ignore

    printfn "%d %d %d" f0 s0 s1
    for (chgroupidx,chnum) in [(7,0)] do 
        let chgroup,(spikeoffset,spikecount) = spikeindices.[chgroupidx]
        let chnums,spikes,spikesraw = getSpikes fname chgroup spikecount spikecount spikeoffset

        let spikelen = 64
        let spike_max_indx = 31
//
//            let yrangeaxl = Range(-0.1,0.1)
//            addYAxis wnd yrangeaxl Colors.Black "AXL Power" |> ignore
//            addSubPlot wnd xrange yrangeaxl [getLine Colors.Black tmin (1.0/75.0) axlpower] |> ignore

        let scaled_spikes = spikes |> Array.map (fun (st,s) -> scalespike (st,Array.init spikelen (fun i -> s.[i,chnum])))
        printfn "%d" (Array.length scaled_spikes)

        let spikestoplot = Array.zip scaled_spikes non_noise.[chgroupidx]
                            |> Array.filter (fun ((st,s),n) -> n && s.[spike_max_indx] <= -100e-6)
                            |> Array.map fst
        
        //let spikestoplot = scaled_spikes

        plotSpikeAmps 3.0f xrange (Range(-500.0e-6,-75.0e-6)) spike_max_indx wnd Colors.Blue (sprintf "Ch %d (%sV)" chnums.[chnum]) [Colors.Blue,spikestoplot]
                (fun r -> 
                    let inside = getSpikesInWindow spike_max_indx r.Left r.Right r.Top r.Bottom scaled_spikes
                                 |> Array.choose (fun i -> if non_noise.[chgroupidx].[i] then Some i else None)
                    ()
//                    let nspk = Array.length inside
//                    if (nspk > 0 && nspk < 2000) then
//                        let mintemp =  0.01
//                        let tempstep = 0.01
//                        let maxtemp =  0.2
//                        let clus = wave_clus 10 mintemp (maxtemp+tempstep/2.0) tempstep 300 11 0 @"C:\EphysData\Sullivan\Ashesh\temp\temp" spikesraw inside [|1|] spike_max_indx
//                        let ntemp = clus |> Array.length
//                        plotClusters (Array.zip inside clus) 
//                                      mintemp maxtemp tempstep scaled_spikes
                )


#if COMPILED
open System.Windows
[<STAThread()>]
(new Application()).Run() |> ignore
#endif
