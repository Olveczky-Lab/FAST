#if COMPILED
module PlotGoodStuff
#else
#load "PlotHelper.fs"
#load "SVGWriter.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open System.IO
open Nessos.FsPickler
open PlotHelper
open System.Windows.Media
open RP.Controls
open SVGWriter
open BehaviorHelper
open MathNet.Numerics.Statistics
open System.Xml.Linq
open Helper
open ClusterHelper
open AutoSortHelper

let clusterquality =
    let hostname = Remote "140.247.178.94:5900"
//    let path = @"/root/data/asheshdhawale/Data/Gandhar/635364676575794173"
//    let trange = (7465883152UL, 7591508868UL)
//    let i1,i2 = 0,2
    let path = @"/root/data/asheshdhawale/Data/Gandhar/635334227062367922"
    let trange = (2262339305UL, 2387997842UL)
    let i1,i2 = 1,3
    let stfname,sfname =  
        let f = sprintf "%s/%s" path
        f "ChGroup_1/SpikeTimes", f "ChGroup_1/Spikes"
    let firstindx,lastindx = getSubSetindx hostname stfname trange
    let spikeamps = 
        getSpikeAmpSubsets hostname sfname stfname 256 4 [|[|firstindx..lastindx|]|] |> fun x -> Array.get x 0
        |> Array.Parallel.map (fun (_,spk) ->
            spk|>Array.map (float>>(fun x -> x*0.195))
        )
    let xrange = Range(-1000.,-100.)
    let yrange = Range (-1000.,-100.)
    let x = 
        spikeamps |> Array.map (fun spk -> spk.[i1],spk.[i2]) |> Array.filter (fun (x,y) -> 
        x > xrange.Min && x < xrange.Max && y > yrange.Min && y < yrange.Max)
    let svgplot =
            getSVGPointList 600. 600. xrange yrange 1. 
                ([|0..10..Array.length x-1|]|>Array.map (fun i -> x.[i]))
            |> getSVG 600. 600.
    //svgplot.Save(@"C:\temp\ScatterLate.svg")
    svgplot.Save(@"C:\temp\ScatterEarly.svg")
    let plotscatter =
        let (wndamps,_),selected,setcol = plotTicks xrange yrange Colors.Gray (x|>Array.map (fun (a,b) -> a,[|b|]))
        wndamps.Title <- ""
        addScaledXAxis wndamps xrange Colors.Black (sprintf "%smV") |> ignore
        addScaledYAxis wndamps yrange Colors.Black (sprintf "%smV") |> ignore
    ()

let extra =
    let xrange = Range(-3.,5.)
    let goodstuff = 
        use fs =  new FileStream(@"C:\temp\Gandhar79Prelearning",FileMode.Open)
        FsPickler().Deserialize<float[][][]>(fs)

    let wnd,yrange,sps = 
        plotspikerasters xrange 0.1 
            (goodstuff |> Array.mapi (fun i x -> 
                Array.init 10 (fun j -> x.[j*(Array.length x)/10]),
                brightColors.[i%(Array.length brightColors)]))
    let zeroline = addSubPlot2 wnd xrange yrange xrange yrange [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList]
    wnd.Width <- 600.
    wnd.Height <- 600.
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    let svgplot =
        XElement(xnel "svg",
            seq {yield! sps; yield zeroline} |> Seq.map (getSVGSubPlot 1000. 600.)
        )
    svgplot.Save(@"C:\temp\prelearningmore.svg")


let prelearning =
    let xrange = Range(-3.,5.)
    let goodstuff = 
        use fs =  new FileStream(@"C:\temp\Badlands27Good",FileMode.Open)
        FsPickler().Deserialize<float[][][]>(fs)

    let wnd,yrange,sps = 
        plotspikerasters xrange 0.1 
            (goodstuff |> Array.mapi (fun i x -> 
                Array.init 10 (fun j -> x.[j*(Array.length x)/10]),
                brightColors.[i%(Array.length brightColors)]))
    let zeroline = addSubPlot2 wnd xrange yrange xrange yrange [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList]
    wnd.Width <- 600.
    wnd.Height <- 600.
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    let svgplot =
        XElement(xnel "svg",
            seq {yield! sps; yield zeroline} |> Seq.map (getSVGSubPlot 1000. 600.)
        )
    svgplot.Save(@"C:\temp\prelearning.svg")


let postlearning =
    let xrange = Range(-3.,5.)
    let goodstuff = 
        let gaudmalhar =
            use fs =  new FileStream(@"C:\temp\GaudMalhar91Good",FileMode.Open)
            FsPickler().Deserialize<float[][][]>(fs)
        let gandhar =
            use fs =  new FileStream(@"C:\temp\Gandhar79Good",FileMode.Open)
            FsPickler().Deserialize<float[][][]>(fs)
        Array.append gaudmalhar gandhar

    let wnd,yrange,sps = 
        plotspikerasters xrange 0.1 
            (goodstuff |> Array.mapi (fun i x -> 
                Array.init 10 (fun j -> x.[j*(Array.length x)/10]),
                brightColors.[i%(Array.length brightColors)]))
    let zeroline = addSubPlot2 wnd xrange yrange xrange yrange [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList]
    wnd.Width <- 600.
    wnd.Height <- 600.
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    let svgplot =
        XElement(xnel "svg",
            seq {yield! sps; yield zeroline} |> Seq.map (getSVGSubPlot 1000. 600.)
        )
    svgplot.Save(@"C:\temp\postlearning.svg")

let heatmap =
    let gandharlong =
        use fs =  new FileStream(@"C:\temp\goodtrialspikesinorder",FileMode.Open)
        FsPickler().Deserialize<(int*float[][])[]>(fs)

    let xrange = Range(-3.,5.)
    let dt = 0.01
    let nbuckets = xrange.Interval()/dt|>int
    let alltrials = gandharlong |> Array.map snd |> Array.concat
    let x = Array2D.zeroCreate<float> nbuckets (Array.length alltrials)
    alltrials
    |> Array.Parallel.iteri (fun i spikes ->
        let h = Histogram(spikes,nbuckets,xrange.Min,xrange.Max)
        for j in 0..nbuckets-1 do
            x.[j,i] <- h.[j].Count
    )
    writeArray Local @"C:\temp\heatmap" 0UL x |> Async.RunSynchronously
    
let longterm =
    let gandharlong =
        use fs =  new FileStream(@"C:\temp\goodtrialspikes",FileMode.Open)
        FsPickler().Deserialize<(int*float[][])[]>(fs)
    
    let frovertime = 
        let exptid = 79
        let wnd = new D3DPlot2DWindow()
        wnd.Show()
        let dt = 0.1
        let trange = Range(0.0,50.0)
        let yrange = Range(20.0,70.0)
        let firststart,_ = getSMTime exptid 2
        let sp =
            gandharlong|>Array.filter (snd>>Array.length>>fun x -> x >=20)
            |> Array.filter (fun (smid,_) -> smid <= 458)
            |> Array.map (fun (smid,x) -> 
                printfn "%d" smid
                (getSMTime exptid smid |> fun (start,dur) -> (start-firststart).TotalDays),
                x|>Array.concat|>Array.filter(fun t -> t > 0. && t < 1.5)|>Array.length|>float|>fun a -> (1./1.5)*a/(Array.length x|>float)
            )
            |> getLinexy2 Colors.Black
            |> fun x -> addSubPlot2 wnd trange yrange trange yrange [x]
        addXAxis wnd trange Colors.Black "Time (days)" |> ignore
        addYAxis wnd yrange Colors.Black "Firing Rate (1/s)" |> ignore
        let svgplot =
            XElement(xnel "svg",
                seq {yield sp} |> Seq.map (getSVGSubPlot 1000. 600.)
            )
        svgplot.Save(@"C:\temp\frate.svg")

    let plotpsths =
        let xrange = Range(-3.,5.)
        let yrange = Range(0.,160.)
        let dt = 0.05
        let wnd = new D3DPlot2DWindow()
        wnd.Show()
        let sps = 
            [|56;166;259;358;448|]
            |> Array.mapi (fun i smid ->
                let spikes = gandharlong |> Array.find (fst>>((=)smid)) |> snd
                let h = Histogram(spikes|>Array.concat,xrange.Interval()/dt|>int,xrange.Min,xrange.Max)
                Array.init (h.BucketCount) (fun i -> ((h.[i].LowerBound+h.[i].UpperBound))/2.,h.[i].Count/(Array.length spikes|>float)/dt)
                |> getLinexy2 brightColors.[i]
                |> fun x -> addSubPlot2 wnd xrange yrange xrange yrange [x]
            )
        let zeroline = addSubPlot2 wnd xrange yrange xrange yrange [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList]
        addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
        addYAxis wnd yrange Colors.Black "Firing Rate (1/s)" |> ignore
        let svgplot =
            XElement(xnel "svg",
                seq {yield! sps; yield zeroline} |> Seq.map (getSVGSubPlot 1000. 600.)
            )
        svgplot.Save(@"C:\temp\pethovertime.svg")
    ()

