#if COMPILED
module PlotLongBairagi
#else
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.IO.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.FSharp.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\IntelIPP.dll"

#r @"WindowsBase.dll"
#r @"PresentationCore.dll"
#r @"PresentationFramework.dll"
#r @"System.Xaml.dll"
#r @"UIAutomationTypes.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot2D.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\RPCommon.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"

#load "Helper.fs"
#load "PlotHelper.fs"
#load "AmpDataHelper.fs"
#load "SVGWriter.fs"

#time
fsi.ShowDeclarationValues <- false
#endif

open RP.Controls
open System.Windows.Media
open System.Windows
open RP.HDF5
open Helper
open PlotHelper
open MathNet.Numerics.Statistics
open AmpDataHelper
open SVGWriter
open System.Xml.Linq

[<Literal>]
let outputfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\Processed.h5"
type h5file = HDF5File<outputfname>

let realspikes =
    let spikes = h5file.Spikes.ReadData()
    let spiketimes = h5file.SpikeTimes.ReadData()
    Array.init (Array.length spiketimes) 
               (fun spikenum -> 
                        spiketimes.[spikenum],Array2D.init (Array3D.length2 spikes) (Array3D.length3 spikes)
                                                           (fun samplenum chnum -> spikes.[spikenum,samplenum,chnum]))
let scaled_spikes = realspikes 
                    |> Array.map 
                        (fun (st,s) -> 
                            scalespike (st, Array.init (Array2D.length2 s) 
                                            (fun i -> Array.init (Array2D.length1 s) (fun j -> s.[j,i]))
                                            |> Array.concat))

let axlpower = h5file.AXLPower.ReadData()
let lfppower = h5file.LFPPower.ReadData()
let leverpressevents = h5file.LeverPressTimes.ReadData()

let spike_max_indx = 31

let sr = 1.0
let smoothingWindow = 60.0
let smoothdata = smooth (smoothingWindow|>int)

let wnd = new D3DPlot2DWindow()
//wnd.Width <- 2048.0
//wnd.Height <- 400.0
wnd.Show()
let (tmin,tmax) = 0.0,79.0*3600.0
//let xrange = Range(1.3599*24.0*3600.0,1.3599*24.0*3600.0+30.0*60.0)
let xrange = Range(tmin,tmax)
addTimeXAxis wnd xrange |> ignore

//LFP
let yrangelfp = Range(-10.0e-3,2.0e-3)
addScaledYAxis wnd yrangelfp Colors.Green (sprintf "LFP Power (%sV^2)") |> ignore
addSubPlot wnd xrange yrangelfp [getLine Colors.Green (smoothingWindow/2.0) (1.0/sr) (lfppower|> smoothdata)] |> ignore

//Firing Rate
let yrangefrate = Range(-240.0,140.0)
addScaledYAxis wnd yrangefrate Colors.Black (sprintf "Firing Rate (%sHz)") |> ignore
let spikecounts =
    let h = Histogram(scaled_spikes|>Array.map fst,tmax|>int,tmin,tmax)
    Array.init (h.BucketCount) (fun i -> h.[i].Count)
addSubPlot wnd xrange yrangefrate 
           [getLine Colors.Black (smoothingWindow/2.0) (1.0/sr) (spikecounts|> smoothdata)] 
|> ignore

//Spike Amplitudes
let yrangespikeamp = Range(-0.8e-3,1.4e-3)
plotSpikeAmps 1.0f xrange yrangespikeamp spike_max_indx wnd (Color.FromArgb(255uy,0uy,0uy,255uy)) (sprintf "Spike Amplitude (%sV)") 
              [Colors.Blue,scaled_spikes]
              (fun r -> ())

//Spike Amplitude Distribution
let ampWindow = 600.0
let ampdist = 
    let dist = Array.init<float list> (((scaled_spikes.[scaled_spikes.Length-1]|>fst)/ampWindow|>int)+1) (fun i -> [])
    scaled_spikes
    |> Array.iter (fun (st,s) -> 
                        let binindx = (st/ampWindow)|>int
                        dist.[binindx] <- -s.[spike_max_indx]::dist.[binindx])
    dist
let ampdistsummary = ampdist |> Array.map (fun amps -> 
                                                let samps = List.sort amps |> List.toArray
                                                let namps = Array.length samps - 1
                                                samps.[namps/2],(samps.[(float namps)*0.975|>int],samps.[(float namps)*0.025|>int]),
                                                                (samps.[(float namps)*0.75|>int],samps.[(float namps)*0.25|>int]))
let yrangeampmeans = Range(0.0,0.5e-3)
addScaledYAxis wnd yrangeampmeans Colors.Blue (sprintf "Spike Amplitude (%sV)") |> ignore
addSubPlot wnd xrange yrangeampmeans
           [getLine Colors.Black (ampWindow/2.0) (ampWindow/sr) (ampdistsummary|>Array.map (fun (m,_,_) -> m));
            getLine Colors.Blue (ampWindow/2.0) (ampWindow/sr) (ampdistsummary|>Array.map (fun (_,(u,_),_) -> u));
            getLine Colors.Blue (ampWindow/2.0) (ampWindow/sr) (ampdistsummary|>Array.map (fun (_,(_,d),_) -> d));
            getLine Colors.LightBlue (ampWindow/2.0) (ampWindow/sr) (ampdistsummary|>Array.map (fun (_,_,(u,_)) -> u));
            getLine Colors.LightBlue (ampWindow/2.0) (ampWindow/sr) (ampdistsummary|>Array.map (fun (_,_,(_,d)) -> d));] 
|> ignore

//Accelerometer Power
let yrangeaxl = Range(-80e-3,260e-3)
addScaledYAxis wnd yrangeaxl Colors.Red (sprintf "Axl Power (%sV^2)") |> ignore
addSubPlot wnd xrange yrangeaxl [getLine Colors.Red (sr/2.0) (1.0/sr) (axlpower |> smoothdata)] |> ignore

//Lever Press Rate
let yrangepressrate = Range(-3.0,37.0)
addScaledYAxis wnd yrangepressrate Colors.Brown (sprintf "Lever Press Rate (%sHz)") |> ignore
let presscounts =
    let h = Histogram(leverpressevents|>Array.map (fun x -> (float x)/1000.0),tmax|>int,tmin,tmax)
    Array.init (h.BucketCount) (fun i -> h.[i].Count)
addSubPlot wnd xrange yrangepressrate 
           [getLine Colors.Brown (smoothingWindow/2.0) (1.0/sr) (presscounts |> smoothdata)] 
|> ignore

//SessionStructure
let yrangess = Range(-0.25,1.25)
let getticklines times col =
    times |> Array.map (fun t -> getLine col ((float t)/1000.0) 0.0 [|0.0;1.0|]) |> Array.toList
addYAxis wnd yrangess Colors.Black "Event Times" |> ignore
addSubPlot wnd xrange yrangess
           ((getticklines (h5file.LightsOnTime.ReadData()) Colors.Blue)@
            (getticklines (h5file.LightsOffTime.ReadData()) Colors.Green)@
            (getticklines (h5file.TrainingSessionTimes.ReadData()|>Seq.cast|>Seq.toArray) Colors.Red)|> Seq.cast)
|> ignore

let savesvg = 
    let getsvg col pos scalebar dmax xfact yfact (d:float[]) =
        let style = (sprintf "fill:none;stroke:%s;stroke-width:1" col)
        let datasvg = 
            d
            |> Seq.mapi (fun i x -> ((float i)/xfact,yfact*pos-(min x dmax)/dmax*yfact/1.1)) 
            |> getSVGPolyLine style
        let scalebarpos = (float (Array.length d))/xfact+5.0
        let scalebarsvg = getSVGPolyLine style [scalebarpos,yfact*pos;scalebarpos,yfact*pos-scalebar/dmax*yfact/1.1]
        datasvg,scalebarsvg

    let gettimescalebar duration xfact yfact =
        getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [0.0,-yfact-4.0;duration/xfact,-yfact-4.0]
    
    let savespikes =
        let numtoplot = 20
        let getspikes col offset = 
            scaled_spikes.[offset..offset+numtoplot-1]
            |> Array.map (fun (_,s) -> 
                            getSVGPolyLine (sprintf "fill:none;stroke:%s;stroke-width:1" col)
                                (s.[0..63] |> Array.mapi (fun i x -> (float i)*5.0,-x*1e6)))
        let x = XElement(xn "svg")
        getspikes "darkturquoise" 1442600 |> Seq.iter (fun spk -> x.Add(spk))
        getspikes "darkviolet" 1242500 |> Seq.iter (fun spk -> x.Add(spk))
        x.Add(getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [0.0,0.0;15.0*5.0,0.0])
        x.Add(getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [0.0,0.0;0.0,0.2e-3*1e6])
        x.Save(@"D:\Rajesh\Dropbox\Raj's paper\EphysDataFigure\spikes1.svg")
    
    let saveampdist3d =
        let xfact = 0.8
        let yfact = 150.0
        let xmax = (float (Array.length ampdistsummary))/xfact
        let dmax = 0.5e-3
        let ynoise = -(9.11e-6*1.4826*5.0)/dmax*yfact/1.1
        let get3dsvg col pos scalebar d =
            getsvg col pos scalebar dmax xfact yfact d
        let svgmed,sb = get3dsvg "black" 0.0 0.1e-3 (ampdistsummary|>Array.map (fun (m,_,_) -> m))
        let svg25,_ = get3dsvg "blue" 0.0 0.2e-3 (ampdistsummary|>Array.map (fun (_,(u,_),_) -> u))
        let svg75,_ = get3dsvg "blue" 0.0 0.2e-3 (ampdistsummary|>Array.map (fun (_,(_,d),_) -> d))
        let svg025,_ = get3dsvg "lightblue" 0.0 0.2e-3 (ampdistsummary|>Array.map (fun (_,_,(u,_)) -> u))
        let svg975,_ = get3dsvg "lightblue" 0.0 0.2e-3 (ampdistsummary|>Array.map (fun (_,_,(_,d)) -> d))
        let x = XElement(xn "svg",svgmed,sb,svg25,svg75,svg025,svg975)
        x.Add(getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [0.0,ynoise;xmax,ynoise]) //noise floor
        let scalex xval = xval/600.0/xfact
        x.Add(getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [scaled_spikes.[1242500]|>fst|>scalex,0.0;scaled_spikes.[1242519]|>fst|>scalex,10.0]) //sample_spikes1
        x.Add(getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [scaled_spikes.[1442600]|>fst|>scalex,0.0;scaled_spikes.[1442619]|>fst|>scalex,10.0]) //sample_spikes1
        x.Add(getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [1.3599*24.0*3600.0|>scalex,0.0;1.3599*24.0*3600.0+30.0*60.0|>scalex,0.0]) //sample_spikes1
        x.Save(@"D:\Rajesh\Dropbox\Raj's paper\EphysDataFigure\ampdist3d.svg")

    let save3dsvg =
        let ds = downsampleAvg 60
        let xfact = 8.0
        let yfact = 25.0
        let get3dsvg col pos scalebar d =
            getsvg col pos scalebar (Array.max d) xfact yfact d
        let svgfrate,fratesb = get3dsvg "black" 0.0 50.0 (spikecounts|>ds)
        let svgpress,presssb = get3dsvg "brown" 1.0 0.5 (presscounts|>ds)
        let timescalebar = gettimescalebar (12.0*60.0) xfact yfact
        let getsessionstructureline col tmin tmax =
            let stimepos = 15.0
            let scalestime t = (t|>float)/1000.0/60.0/xfact
            getSVGPolyLine (sprintf "fill:none;stroke:%s;stroke-width:10" col) 
                           [tmin|>scalestime,yfact+stimepos;tmax|>scalestime,yfact+stimepos]
        let x = XElement(xn "svg",svgfrate,fratesb,svgpress,presssb,timescalebar)
        let training = h5file.TrainingSessionTimes.ReadData()
        for (startt,endt) in (Array.init (Array2D.length1 training) (fun i -> training.[i,0],training.[i,1])) do
            x.Add(getsessionstructureline "blue" startt endt)
        let ontime = h5file.LightsOnTime.ReadData()
        let offtime = h5file.LightsOffTime.ReadData().[1..]
        for  (startt,endt) in Array.zip ontime offtime do
            x.Add(getsessionstructureline "black" startt endt)
        x.Save(@"D:\Rajesh\Dropbox\Raj's paper\EphysDataFigure\3d.svg")

    let save30msvg = 
        let offset = xrange.Min|>int
        let xfact = 4.0
        let yfact = 50.0
        let get30msvg col pos maxpos scalebar (d:float[]) =
            let curd = d.[offset..offset+30*60-1]
            let dmax = (curd |> Array.sort).[maxpos]
            getsvg col pos scalebar dmax xfact yfact curd
        let svglfp,lfpsb = get30msvg "green" 0.0 1780 0.4e-3 lfppower
        let svgfrate,fratesb = get30msvg "black" 1.0 1799 50.0 spikecounts
        let svgpress,presssb = get30msvg "brown" 2.0 1799 2.0 presscounts
        let svgaxl,_ = get30msvg "red" 3.0 1780 0.0 axlpower
        let timescalebar = gettimescalebar (5.0*60.0) xfact yfact
        let spikesinrange = 
            scaled_spikes |> Array.filter (fun (st,_) -> st >= xrange.Min && st <= xrange.Max)
                          |> Array.map (fun (st,_) -> st - xrange.Min)

        XElement(xn "svg",svglfp,lfpsb,svgfrate,fratesb,svgaxl,svgpress,presssb,timescalebar)
            .Save(@"D:\Rajesh\Dropbox\Raj's paper\EphysDataFigure\30m.svg")
    ()

