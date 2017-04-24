#if COMPILED
module PlotSpikeAmps
#else
#load "Subject.fs"
#load "ClusterViewerHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open Helper
open PlotHelper
open Cluster
open System.Xml.Linq
open ClusterTreeHelper
open ClusterViewerHelper
open ClusterHelper
open RunCluster
open System.IO

let hostname = Remote "140.247.178.94:5900"
let path = @"/root/data/asheshdhawale/Data/Gandhar/635315177878133140"
let fpath =  sprintf @"%s/%s" path
let esets =
    use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
    loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
let chgroup,spikelen = "0",64
let fname = sprintf "%s/ChGroup_%s/%s" path chgroup
let nchans = esets|>Array.find (fun (x,_) -> x = chgroup)|>snd|>Array.length
let safname,stfname = 
    fname @"SpikeAmps", fname @"SpikeTimes"
let ntotalspikes = (getSize hostname stfname |> Async.RunSynchronously)/8L|>int
printfn "%d" ntotalspikes
let start,numtoread = 0UL,1000000
let numtoshow = min numtoread 10000000

let spikeamps = 
    let sts = getSpikeTimes hostname stfname start numtoread
    let samps = getArray2D<int16> nchans hostname safname start numtoread
    Array.init numtoshow (fun k ->
        let i = (k|>int64)*(numtoread|>int64)/(numtoshow|>int64)|>int
        (sts.[i]|>float)/30000., Array.init nchans (fun j -> 0.195*(samps.[i,j]|>float))
    )

let t0 = spikeamps.[0] |> fst
let t1 = spikeamps.[Array.length spikeamps-1] |> fst
let xrange = Range(t0,t1)
let yrange = Range (-400.,400.)
let (wndamps,_),selected,setcol = plotTicks xrange yrange Colors.Black spikeamps
wndamps.Left <- 2048.; wndamps.Width <- 2048.; wndamps.Height <- 1112.; wndamps.Top <- 0.
wndamps.Title <- ""
addTimeXAxis wndamps xrange |> ignore
addScaledYAxis wndamps yrange Colors.Black (sprintf "%sV") |> ignore


#if COMPILED
[<STAThread()>]
(new Application()).Run() |> ignore
#endif
