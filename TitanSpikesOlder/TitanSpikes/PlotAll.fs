#if COMPILED
module PlotAll
#else
#r @"WindowsBase.dll"
#r @"PresentationCore.dll"
#r @"PresentationFramework.dll"
#r @"System.Xaml.dll"
#r @"UIAutomationTypes.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.IO.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\Math.Net Numerics\MathNet.Numerics.FSharp.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\IntelIPP.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot2D.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\D3DPlot.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Debug\RPCommon.dll"

#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\HDF5IO\bin\Debug\HDF5IO.dll"

#load "Helper.fs"
#load "AmpDataHelper.fs"
#load "PlotHelper.fs"
#load "SVGWriter.fs"
#time
fsi.ShowDeclarationValues <- false
#endif

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open RP.HDF5.H5TypeProviderRuntime
open Helper
open PlotHelper
open System.IO
open AmpDataHelper
open System.Windows.Media.Imaging
open System.Windows.Interop
open SVGWriter
open System.Xml.Linq
open RP.HDF5

let fnum = "635013888324512817"
let h5fname = @"C:\EphysData\Sullivan\Ashesh\"+fnum+".h5"
let rawampfname = @"Z:\bairagi"+fnum+".amp"

[<Measure>] type s //seconds
[<Measure>] type hz = 1/s
let chgroup = "26"
let chnum = 0
let starttime = 1.3599*24.0*3600.0<s> + 1.0*60.0<s>
let duration = 30.0<s>
let sramp = 30000.0<hz>
let srvdd = 500.0<hz>
let srlfp = 300.0<hz>
let sraxl = 7500.0<hz>
let spike_max_indx = 31

let wnd = new D3DPlot2DWindow()
wnd.Show()
let xrange = Range(0.0,duration/1.0<s>)
addTimeXAxis wnd xrange |> ignore

let lfp_raw = readData h5fname "./LFP" [|srlfp*starttime|>uint64;chnum|>uint64|] null [|srlfp*duration|>uint64;1UL|] :?> int16[,]
let lfp = Array.init (Array2D.length1 lfp_raw) (fun i -> (float lfp_raw.[i,0])*0.195e-6)

let yrangelfp = Range(-1e-3,1e-3)
addScaledYAxis wnd yrangelfp Colors.Green (sprintf "LFP (%sV)") |> ignore
addSubPlot wnd xrange yrangelfp [getLine Colors.Green 0.0 (1.0<hz>/srlfp) lfp] |> ignore

let vdd_raw = readData h5fname "./VDD" [|srvdd*(starttime)|>uint64;0UL|] null [|srvdd*duration|>uint64;2UL|] :?> uint16[,]
let vdd = Array.init 2 (fun ch -> Array.init (Array2D.length1 vdd_raw) (fun i -> (float vdd_raw.[i,ch])*7.48e-5))

let yrangeVdd = Range(3.2,3.3)
addScaledYAxis wnd yrangeVdd Colors.Gray (sprintf "Vdd0 (%sV)") |> ignore
addSubPlot wnd xrange yrangeVdd [getLine Colors.Gray 0.0 (1.0<hz>/srvdd) (vdd.[0])] |> ignore

let axl_raw = readData h5fname "./AXL" [|sraxl*(starttime)|>uint64;0UL|] null [|sraxl*duration|>uint64;3UL|] :?> uint16[,]
let axl = Array.init 3 (fun ch -> Array.init (Array2D.length1 axl_raw) (fun i -> (float axl_raw.[i,ch])*3.74e-5))

let plotaxl chnum yrangeaxl =
    addScaledYAxis wnd yrangeaxl Colors.Red (sprintf "Axl%d (%sV)" chnum) |> ignore
    addSubPlot wnd xrange yrangeaxl [getLine Colors.Red 0.0 (1.0<hz>/sraxl) axl.[chnum]] |> ignore

plotaxl 0 (Range(-3.5,3.5))
plotaxl 1 (Range(-0.7,0.8))
plotaxl 2 (Range(-1.3,2.1))

let spikes = 
    loadSpikesInWindow (sramp*(starttime)|>int64) (sramp*(starttime+duration)|>int64) h5fname chgroup
let amp = 
    use fs = new FileStream(rawampfname,FileMode.Open,FileAccess.Read)
    fs.Seek((sramp*(starttime)|>int64)*64L*2L,SeekOrigin.Begin) |> ignore
    let numsamples = sramp*duration |> int
    let b = Array.zeroCreate<byte> (numsamples*64*2)
    fs.Read(b,0,b.Length) |> ignore
    (scaleData 32768.0 numsamples b chnum |> Array.map (fun x -> x*0.195e-6) |> filtfilt spikes_bandpass)

let yrangeampraw = Range(-0.5e-3,1.5e-3)
addScaledYAxis wnd yrangeampraw Colors.Black (sprintf "Raw (%sV)") |> ignore
let ampl = getPoints 3.0f Colors.Blue (spikes |> Array.map (fun (st,s:float[,]) -> st,s.[spike_max_indx,0]))
let spikesl = spikes |> Array.map (fun (st,s) -> getLine 
                                                    Colors.HotPink 
                                                    (st-(float spike_max_indx)/(sramp/1.0<hz>)) 
                                                    (1.0<hz>/sramp) 
                                                    (Array.init (Array2D.length1 s) (fun i -> s.[i,0]))) |> Array.toList

let amplines = [getLine Colors.Black 0.0 (1.0<hz>/sramp) amp] |> Seq.cast //::ampl::spikesl |> Seq.cast |> Seq.toArray
addSubPlot wnd xrange yrangeampraw amplines |> ignore
wnd.KeyDown.Add (fun args ->    let delta_s, delta_a = 
                                    match args.Key with
                                    | Input.Key.D -> 1.0f,0
                                    | Input.Key.A -> -1.0f,0
                                    | Input.Key.S -> 0.0f,-25
                                    | Input.Key.W -> 0.0f,25
                                    | _ -> 0.0f,0
                                ampl.Width <- ampl.Width + delta_s
                                ampl.Height <- ampl.Height + delta_s
                                ampl.Color <- Color.FromArgb(byte (clamp 0 255 ((int ampl.Color.A) + delta_a)),ampl.Color.R,ampl.Color.G,ampl.Color.B)
                                wnd.Plot.ForceRefresh())

[<Literal>]
let outputfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\Processed.h5"
type h5file = HDF5File<outputfname>
let pressevents = h5file.LeverPressTimes.ReadData()
                  |> Array.map (fun x -> (float x)/1000.0-starttime/1.0<s>)
                  |> Array.filter (fun x -> x >= 0.0 && x <= duration/1.0<s>)

let savesvg = 
    let getsvg col pos scalebar dmax dmin xfact yfact (d:float[]) =
        let style = (sprintf "fill:none;stroke:%s;stroke-width:1" col)
        let datasvg = 
            d
            |> Seq.mapi (fun i x -> ((float i)/xfact,yfact*pos-(x-dmin)/(dmax-dmin)*yfact/1.1)) 
            |> getSVGPolyLine style
        let scalebarpos = (float (Array.length d))/xfact+5.0
        let scalebarsvg = getSVGPolyLine style [scalebarpos,yfact*pos;scalebarpos,yfact*pos-scalebar/dmax*yfact/1.1]
        datasvg,scalebarsvg

    let gettimescalebar duration xfact yfact =
        getSVGPolyLine "fill:none;stroke:black;stroke-width:1" [0.0,-yfact-4.0;duration/xfact,-yfact-4.0]

    let save30ssvg =
        let yfact = 50.0
        let get30ssvg xfact col pos scalebar d =
            getsvg col pos scalebar (Array.max d) (Array.min d) xfact yfact d
        let svglfp,lfpsb = get30ssvg 20.0 "green" 0.0 1.0e-3 lfp
        let svgraw,rawsb = get30ssvg 20.0 "black" 1.0 0.2e-3 (FilterPoints.DownsampleByMinMax(amp,100)|>Seq.toArray)
        let axl1,_ = get30ssvg 20.0 "red" 2.0 0.0 (FilterPoints.DownsampleByMinMax(axl.[0],25)|>Seq.toArray)
        let axl2,_ = get30ssvg 20.0 "red" 3.0 0.0 (FilterPoints.DownsampleByMinMax(axl.[1],25)|>Seq.toArray)
        let axl3,_ = get30ssvg 20.0 "red" 4.0 0.0 (FilterPoints.DownsampleByMinMax(axl.[2],25)|>Seq.toArray)
        let timescalebar = gettimescalebar 5.0 (20.0/300.0) yfact
        let x = XElement(xn "svg",svglfp,lfpsb,svgraw,rawsb,axl1,axl2,axl3,timescalebar)
        for evt in pressevents do
            x.Add(getSVGPolyLine "fill:none;stroke:brown;stroke-width:2" [evt*(300.0/20.0),4.25*yfact;evt*(300.0/20.0),5.0*yfact-1.0/1.1*yfact])
        x.Save(@"D:\Rajesh\Dropbox\Raj's paper\EphysDataFigure\30s.svg")
    ()
