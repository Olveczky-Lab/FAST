module PlotHelper

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open Helper
open MathNet.Numerics.Statistics
open RP.HDF5.H5TypeProviderRuntime
open System.Windows.Media.Imaging
open System.Windows.Interop
open System.IO

let initDisplayState (range:Range<double>) color onRangeChanged =
    let ds = AxisDisplayState(AxisRange = range, AxisBrush = new SolidColorBrush(color))
    range.Changed.Add(fun _ -> onRangeChanged ds)
    range.Set(range)
    ds
    
let addXAxis (wnd:D3DPlot2DWindow) xrange = 
    let ds = initDisplayState xrange Colors.Black (fun _ -> ())
    wnd.Plot.Settings.XAxesDisplayState.Add(ds)
    ds

let addTimeXAxis (wnd:D3DPlot2DWindow) xrange = 
    let ds = initDisplayState xrange Colors.Black (fun ds -> 
                                                    let multiplier,s = getOrderOfMagTime (xrange.Interval()/3.0)
                                                    ds.AxisMultiplier <- multiplier
                                                    ds.AxisOffset <- xrange.Min
                                                    let offset_multiplier,offset_s = getOrderOfMagTime (((abs xrange.Min)+(abs xrange.Max))/2.0)
                                                    ds.AxisTitle <- sprintf "Time (%0.4f%s + _%s)" (xrange.Min*offset_multiplier) offset_s s)
    wnd.Plot.Settings.XAxesDisplayState.Add(ds)
    ds

let saveimage (wnd:D3DPlot2DWindow) fname = 
    let img = (wnd.PlotImage.Source :?> D3DImage)
    let pnt = wnd.PlotImage.TranslatePoint(Point(0.0,0.0),wnd)
    let w = img.PixelWidth
    let h = img.PixelHeight
    let rtb = RenderTargetBitmap(w,h,96.0,96.0,PixelFormats.Default)
    rtb.Render(wnd)
    use fs = new FileStream(fname,FileMode.Create,FileAccess.Write)
    let enc = TiffBitmapEncoder()
    enc.Frames.Add(BitmapFrame.Create(rtb))
    enc.Save(fs)

let addYAxis (wnd:D3DPlot2DWindow) yrange color title =
    let ds = initDisplayState yrange color (fun _ -> ())
    ds.AxisTitle <- title
    wnd.Plot.Settings.YAxesDisplayState.Add(ds)
    ds

let getScaledAxisDisplayState yrange color title = 
    initDisplayState yrange color (fun ds ->
                                    let multiplier,s = getOrderOfMag (((abs yrange.Min)+(abs yrange.Max))/2.0)
                                    ds.AxisMultiplier <- multiplier
                                    ds.AxisTitle <- (title s))

let addScaledYAxis (wnd:D3DPlot2DWindow) yrange color (title:string -> string) = 
    let ds = getScaledAxisDisplayState yrange color title
    wnd.Plot.Settings.YAxesDisplayState.Add(ds)
    ds

let addScaledXAxis (wnd:D3DPlot2DWindow) yrange color (title:string -> string) = 
    let ds = getScaledAxisDisplayState yrange color title
    wnd.Plot.Settings.XAxesDisplayState.Add(ds)
    ds

let addSubPlot (wnd:D3DPlot2DWindow) xrange yrange lines =
    let sp = SubPlot(DisplayState = PlotSurfaceDisplayState(XRange = xrange, YRange = yrange, ClipToBounds = true), 
                                Lines = lines)
    wnd.Plot.AddSubPlot(sp)
    sp

let getLine color t0 dt sp =
    let l = 
        let s = new PointSeries2D()
        s.data <- sp |> Seq.mapi (fun i x -> (t0+dt*(float i),x))
        s
    l.Color <- color
    l.LineStyle <- RenderType.Aliased
    l.Width <- 2.0f
    l

let getPoints size color points =
    let l = new PointSeries2D(points)
    l.Color <- color
    l.LineStyle <- RenderType.Points
    l.Width <- size
    l.Height <- size
    l

let hist n xs =
    let h = new Histogram(xs,n)
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    let xrange = Range(h.LowerBound,h.UpperBound)
    addXAxis wnd xrange |> ignore
    let counts = Array.init (h.BucketCount) (fun i -> (h.[i].LowerBound + h.[i].UpperBound)/2.0,h.[i].Count)
    let yrange = Range(0.0,(counts |> Array.maxBy snd |> snd) * 1.1)
    addYAxis wnd yrange Colors.Blue "Counts" |> ignore
    let p = getPoints 10.0f Colors.Blue counts
    let ls = counts |> Array.map (fun (x,y) -> getLine Colors.Blue x 0.0 [0.0;y]) |> Array.toList
    addSubPlot wnd xrange yrange ((p::ls) |> Seq.cast)

let plotlines wndtitle xrange linestoplot (lines:(float*float[])[]) = 
    let ls,min,max = 
        linestoplot
        |> Array.map (fun (i,color) -> (color,lines.[i]))
        |> Seq.fold (fun (ls,min,max) (color,(st,s)) ->                        
                        let curmax = Array.max s
                        let curmin = Array.min s
                        ((getLine color 0.0 1.0 s::ls),
                            (if (curmin < min) then curmin else min), 
                            (if (curmax > max) then curmax else max))) ([],0.0,0.0)
    if ((List.length ls) > 0) then
        let wnd = new D3DPlot2DWindow()
        wnd.Show()
        wnd.Title <- wndtitle
        let yrange = Range(min,max)
        addXAxis wnd xrange |> ignore
        addScaledYAxis wnd yrange Colors.Black (sprintf "%sV") |> ignore
        addSubPlot wnd xrange yrange (ls |> Seq.cast) |> ignore

let getSpikesInWindow spike_max_indx mintime maxtime minamp maxamp spikes =
    let spiketimes = spikes |> Array.map (fun (st,_) -> st)
    let inside = 
        let minindx = 
            let indx = Array.BinarySearch(spiketimes,mintime)
            if (indx < 0) then ~~~indx else indx
        if (minindx >= Array.length spikes) then [||]
        else
            let maxindx = 
                let indx = Array.BinarySearch(spiketimes,maxtime)
                if (indx < 0) then ((~~~indx)-1) else indx
            if (maxindx < 0) then [||] 
            else
                spikes.[minindx..maxindx]
                |> Array.mapi (fun i x -> (i+minindx,x)) 
                |> Array.choose (fun (i,(_,s:float[])) -> if (s.[spike_max_indx] >= minamp && s.[spike_max_indx] <= maxamp) then Some i else None)
    inside
    
let plotSpikeAmps pointsize xrange yrangeAmp spike_max_indx wnd col wndtitle spikes selchanged =
    addScaledYAxis wnd yrangeAmp col wndtitle |> ignore
    let ampls =
        spikes |> List.map (fun (spike_col,spike_data) -> 
                                 getPoints pointsize spike_col (spike_data |> Array.map (fun (st,s:float[]) -> st,-s.[spike_max_indx])))
    ampls |> Seq.iter (fun ampl -> addSubPlot wnd xrange yrangeAmp [ampl] |> ignore)
    wnd.KeyDown.Add (fun args -> let delta_s, delta_a = 
                                        match args.Key with
                                        | Input.Key.D -> 1.0f,0
                                        | Input.Key.A -> -1.0f,0
                                        | Input.Key.S -> 0.0f,-5
                                        | Input.Key.W -> 0.0f,5
                                        | _ -> 0.0f,0
                                 for ampl in ampls do
                                     ampl.Width <- ampl.Width + delta_s
                                     ampl.Height <- ampl.Height + delta_s
                                     ampl.Color <- Color.FromArgb(byte (clamp 0 255 ((int ampl.Color.A) + delta_a)),ampl.Color.R,ampl.Color.G,ampl.Color.B)
                                 wnd.Plot.ForceRefresh()
                    )
    wnd.Plot.add_SelectionChanged(fun rpixels ->
                                    let r = wnd.Plot.PhysicalToLogical(rpixels,xrange,yrangeAmp)
                                    selchanged r)

