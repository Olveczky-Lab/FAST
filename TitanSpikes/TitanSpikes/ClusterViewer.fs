#if COMPILED
module ClusterViewer
#else

#load "PlotHelper.fs"

#time "on"
fsi.ShowDeclarationValues <- false
#endif

open RP.Controls
open System.Windows
open PlotHelper

let chgroup,spikelen,nchans = "0",64,4
let hostname = "140.247.178.16"
let clusfname,spikesfname = 
    let fname = sprintf @"/root/data/rpoddar/badlands/635276396558304920/ChGroup_%s/%s" chgroup
    fname "Clusters", fname "Spikes"


let yrange = Range(-500.0e-6,500.0e-6)

let numperclustering = 1000

let chnum = 0

let wnd = ClusterViewer xrange yrange scaled_spikes clus chgroup chnum

#if COMPILED
[<System.STAThread()>]
(new Application()).Run() |> ignore
#endif
