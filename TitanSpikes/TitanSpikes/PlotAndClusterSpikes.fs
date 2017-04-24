#if COMPILED
module PlotAndClusterSpikes
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

let hostname = Remote "140.247.178.16:5900"
let path = @"/root/data/rpoddar/bairagi/635013888324512817"
let fpath =  sprintf @"%s/%s" path
let esets =
    use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
    loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
let chgroup = "4"
let spikelen = 64
let fname = sprintf "%s/ChGroup_%s/%s" path chgroup
let nchans = esets|>Array.find (fun (x,_) -> x = chgroup)|>snd|>Array.length
//let ntotalspikes = (getSize hostname (fname "Clusters1.indx") |> Async.RunSynchronously)/8L|>int
let ntotalspikes = (getSize hostname (fname "SpikeTimes") |> Async.RunSynchronously)/8L|>int
//let a,b = getSubSetindx hostname (fname "SpikeTimes") (6469372800UL,6469372800UL+3600UL*30000UL)
let numperblock,firstblock,numblocks = 1000,0,4

let scaled_spikes,spikesraw = 
    let sfname,stfname,clusfname = 
        fname @"Spikes", fname @"SpikeTimes", fname @"Clusters1.indx"
    //let spikestoload = getClustersIndx hostname clusfname numperblock firstblock numblocks
    let spikestoload = [|firstblock*numperblock..(firstblock+numblocks)*numperblock-1|]|>Array.map uint64
    let spikes = getSpikeSubsets hostname sfname stfname (spikelen*nchans) [|spikestoload|] |> Array.concat
    let scaled_spikes = 
        spikes |> Array.map (fun (st,spk) -> 
            (st|>float)/30000.,spk |> Array.map (fun x -> 0.195*(x|>float)) |> scalespike nchans)
    let spikesraw = spikes |> Array.map snd
    scaled_spikes,spikesraw

showSortedSpikes spikelen nchans [|scaled_spikes,(Colors.Black,[||])|]
|> ignore


let tmax,_ = scaled_spikes.[scaled_spikes.Length-1]
let tmin,_ = scaled_spikes.[0]
let trange = Range(tmin,tmax)
let yrange = Range (-400.,400.)

let clusterSpikes spikestocluster =
    let mintemp =  0.01
    let tempstep = mintemp
    let maxtemp =  0.15
    let nspikes = Array.length spikestocluster
    let features = 
        spikestocluster |> Array.map (fun i -> spikesraw.[i]|>scalespike nchans)
    for i in 0..8 do
        let clus = wave_clus mintemp maxtemp tempstep 300 11 0 
                    @"C:\temp\tempclus" features false
        ()
    let clus = wave_clus mintemp maxtemp tempstep 300 11 0 
                @"C:\temp\tempclus" features false
    let ntemp = clus.[0]|>Array.length
    let xrange = Range(-0.5,0.5)
    let clusterblocks =
        let stab x = (x.Stability|>float)/(x.Items|>Array.length|>float)
        let clusterblock = 
            let xspikes = Array.zip spikestocluster clus
            let shapes = spikestocluster |> Array.map (fun i -> scaled_spikes.[i]|>snd)
            getStableTree shapes xspikes
            |> mapTree (fun x _ -> Visible,x)
            |> fun x -> 
                {StableClusters=x;XPos=(-0.25,0.5)}
        [|clusterblock|]
    let clustertrees = clusterblocks|>Array.map (fun x -> x.StableClusters |> mapTree (fun (_,x) _ -> x))
    let spikeamps = (scaled_spikes|>Array.map (fun (st,spk) -> 
            st,Array.init nchans (fun ch -> Array.get spk (31*nchans+ch))))
    let wnd,xranges,yranges,cluchanged,_ = plotStableClusters (spikelen,nchans,ntemp) xrange yrange clusterblocks    
    let wndspikes = 
        plotClusterSpikeAmps xrange [|scaled_spikes|>Array.minBy fst|>fst,scaled_spikes|>Array.maxBy fst|>fst|] 
            (clusterblocks|>Array.map (fun x -> x.StableClusters|> mapTree (fun (_,clu) _ -> clu.Items),x.XPos)) 
            yrange spikeamps cluchanged     
    plotClusterSpikeShapes spikelen nchans ntemp yrange clustertrees (scaled_spikes|>Array.map snd) cluchanged
    ()

clusterSpikes [|0..999|]

let (wnd,_),selected,setcol = 
    let spikeamps = scaled_spikes |> Array.map (fun (st,spk) -> 
        (st,Array.init nchans (fun ch -> spk.[31*nchans+ch])))
    plotTicks trange yrange Colors.Blue spikeamps
let _,plotfun = plotshapes spikelen nchans yrange
selected |> Observable.subscribe (fun inside ->
    inside |> Array.map (fun i -> scaled_spikes.[i]|>snd) |> fun x -> [x,Colors.Black] |> plotfun
    //clusterSpikes inside
) |> ignore
addTimeXAxis wnd trange |> ignore
addScaledYAxis wnd yrange Colors.Black (sprintf "%sV") |> ignore

#if COMPILED
[<STAThread()>]
(new Application()).Run() |> ignore
#endif
