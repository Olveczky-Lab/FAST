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
    
let addXAxis (wnd:D3DPlot2DWindow) xrange color title = 
    let ds = initDisplayState xrange color (fun _ -> ())
    ds.AxisTitle <- title
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
    let sp = SubPlot(DisplayState = PlotSurfaceDisplayState(XRange = xrange, YRange = yrange, ClipToBounds = false), 
                                Lines = lines)
    wnd.Plot.AddSubPlot(sp)
    sp

let getLine color t0 dt sp =
    let l = 
        let s = new PointSeries2D()
        s.data <- sp |> Seq.mapi (fun i x -> (t0+dt*(float i),x)) |> Seq.toArray
        s
    l.Color <- color
    l.LineStyle <- RenderType.Aliased
    l.Width <- 2.0f
    l

let getLinexy color x y =
    let l = 
        let s = new PointSeries2D()
        s.data <- Seq.zip x y |> Seq.toArray
        s
    l.Color <- color
    l.LineStyle <- RenderType.Aliased
    l.Width <- 2.0f
    l

let getPoints size color points =
    let l = new PointSeries2D(points|>Seq.toArray)
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
    addXAxis wnd xrange Colors.Black "" |> ignore
    let counts = Array.init (h.BucketCount) (fun i -> (h.[i].LowerBound + h.[i].UpperBound)/2.0,h.[i].Count)
    let yrange = Range(0.0,(counts |> Array.maxBy snd |> snd) * 1.1)
    addYAxis wnd yrange Colors.Blue "Counts" |> ignore
    let p = getPoints 10.0f Colors.Blue counts
    let ls = counts |> Array.map (fun (x,y) -> getLine Colors.Blue x 0.0 [0.0;y]) |> Array.toList
    addSubPlot wnd xrange yrange ((p::ls) |> Seq.cast)

let plotbarhist scale (h:Histogram) =
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    let xrange = Range(h.LowerBound,h.UpperBound)
    addXAxis wnd xrange Colors.Black "" |> ignore
    let counts = Array.init (h.BucketCount) (fun i -> (h.[i].LowerBound,h.[i].UpperBound),h.[i].Count)
    let yrange = Range(0.0,(counts |> Array.maxBy snd |> snd) * scale * 1.1)
    addYAxis wnd yrange Colors.Blue "Counts" |> ignore
    let getbar (x1,x2) cnt =
        getLinexy Colors.Black [x1;x1;x2;x2;x1] [0.0;cnt;cnt;0.0;0.0] 
    addSubPlot wnd xrange yrange (counts|>Array.map (fun (pos,cnt) -> getbar pos (cnt*scale)) |> Seq.cast) |> ignore
    wnd,yrange
    

let brightColors = 
    [|"#FF008000";"#FF0000FF";"#FF800080";"#FFFF00FF";"#FF008080";"#FFFFFF00";"#FF808080";"#FF00FFFF";"#FF000080";"#FF800000";"#FFFF3939";"#FF7F7F00";"#FFC0C0C0";"#FFFF6347";"#FFFFE4B5"|]
    |> Array.map (fun str -> ColorConverter.ConvertFromString(str) :?> System.Windows.Media.Color)

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
        addXAxis wnd xrange Colors.Black "" |> ignore
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
                                 getPoints pointsize spike_col (spike_data |> Array.map (fun (st,s:float[]) -> st,s.[spike_max_indx])))
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

open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Input

let addCrosshair (plot:D3DPlot2D) (xrange:Range<double>) (yrange:Range<double>) xstops f =
    let cnv = plot.cnvMain
    let line otation = 
        let l = Line(Stroke = Brushes.Blue, StrokeThickness = 1.0)
        match otation with
        | Orientation.Horizontal -> 
            l.X1 <- 0.0
            l.X2 <- cnv.ActualWidth
            l.Y1 <- 0.0
            l.Y2 <- 0.0
        | Orientation.Vertical -> 
            l.X1 <- 0.0
            l.X2 <- 0.0
            l.Y1 <- 0.0
            l.Y2 <- cnv.ActualHeight
        | _ -> failwith "Not possible"
        RenderOptions.SetEdgeMode(l,EdgeMode.Aliased)
        l
    let hline = line Orientation.Horizontal
    let vline = line Orientation.Vertical
    cnv.Children.Add(hline) |> ignore
    cnv.Children.Add(vline) |> ignore
    let getnearest xphy =
        let x = Ticks.PhysicalToLogical(xrange.Min,xrange.Max,cnv.ActualWidth,xphy,Orientation.Horizontal)
        (xstops |> List.minBy (((-) x) >> abs))
    cnv.MouseMove
    |> Observable.subscribe (fun args ->
                                let x,y = 
                                    let pos = args.GetPosition(cnv)
                                    getnearest pos.X
                                    |> fun a -> Ticks.LogicalToPhysical(xrange.Min,xrange.Max,cnv.ActualWidth,a,Orientation.Horizontal), pos.Y
                                hline.Y1 <- y
                                hline.Y2 <- y
                                vline.X1 <- x
                                vline.X2 <- x) 
    |> ignore
    cnv.MouseDown
    |> Observable.filter (fun args -> args.LeftButton = MouseButtonState.Pressed)
    |> Observable.subscribe (fun args -> 
                                let pos = args.GetPosition(cnv)
                                f (getnearest pos.X) (Ticks.PhysicalToLogical(yrange.Min,yrange.Max,cnv.ActualHeight,pos.Y,Orientation.Vertical)))
    |> ignore                            
    cnv.SizeChanged
    |> Observable.subscribe (fun _ -> 
                                hline.X2 <- cnv.ActualWidth
                                vline.Y2 <- cnv.ActualHeight)
    |> ignore

open ClusterHelper

let plotClusters clus mintemp maxtemp tempstep scaled_spikes = 
    let nspk = clus |> Array.length
    let spikelen = scaled_spikes |> Seq.head |> snd |> Array.length
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    wnd.Title <- sprintf "Number of Spikes = %d" nspk

    let clus_counts = countClusters clus
    let ntemp = Array.length clus_counts
    let stableclusters = getStableClusters ntemp clus 
    let xrange = Range(mintemp-tempstep,maxtemp+tempstep)
    let yrange = Range(-(log10 (float nspk))*0.1,(log10 (float nspk))*1.1)
    let plotspikes temp minclussize =
        let tempnum = (temp-mintemp)/tempstep |> round |> int
        let clus_colors = 
            let colored_clus = clus_counts.[tempnum] 
                                |> Seq.filter (fun (_,count) -> count >= minclussize)
                                |> Seq.toArray
                                |> Array.sortBy snd
                                |> Array.rev
            let numcolored = Array.length colored_clus
            colored_clus 
            |> Array.mapi (fun i (clunum,_) -> 
                            (clunum, if (i < brightColors.Length) then brightColors.[i] else Colors.Gray))
            |> Map.ofArray
        let linestoplot = 
            clus
            |> Array.map (fun (spikenum,c) ->
                                spikenum, match (Map.tryFind c.[tempnum] clus_colors) with
                                          | Some x -> x
                                          | None -> Colors.Gray)
        plotlines (sprintf "Temperature %f" temp) (Range(-0.5,((float spikelen)-0.5)))
                    linestoplot
                    scaled_spikes
    addCrosshair (wnd.Plot) xrange yrange [mintemp..tempstep..maxtemp] 
                    (fun x y -> plotspikes x ((10.0**y)|>ceil|>int))
    addXAxis wnd xrange Colors.Black "Temperature" |> ignore
    addYAxis wnd yrange Colors.Black ("Number of Spikes") |> ignore
    let scaletempnum tempnum =
        mintemp+tempstep*(float tempnum)
    let getcluspoint tempnum clunum =
        if tempnum = -1 then nspk else clus_counts.[tempnum]|>Seq.find (fun (c,_) -> c=clunum)|>snd
        |>float|>log10
    addSubPlot wnd xrange yrange [getPoints 5.0f Colors.Black 
                                    (clus_counts 
                                        |> Seq.mapi (fun i x -> x |> Seq.map (fun (_,c) -> scaletempnum i,log10 (float c)))
                                        |> Seq.concat)] |> ignore
    let stablelines = 
        stableclusters
        |> Seq.map (fun (firsttemp,clunums) ->
                        clunums |> Seq.mapi (fun i (clunum,count) -> getcluspoint (firsttemp+i) clunum) 
                        |> getLine Colors.Black (scaletempnum firsttemp) tempstep)
        |> Seq.cast
    
    addSubPlot wnd xrange yrange stablelines |> ignore

let addSelRect (plot:D3DPlot2D) (xrange:Range<double>) (yrange:Range<double>) xintervals (ymin,ymax) f g =
    let cnv = plot.cnvMain
    let rect = Rectangle(Visibility = Visibility.Collapsed, Stroke = Brushes.Black, StrokeThickness = 1.0)
    cnv.Children.Add(rect) |> ignore
    let selxrange (args:MouseEventArgs) =
        let x = Ticks.PhysicalToLogical(xrange.Min,xrange.Max,cnv.ActualWidth,args.GetPosition(cnv).X,Orientation.Horizontal)
        xintervals |> Array.tryFindIndex (fun (tmin,tmax) -> x>=tmin && x<=tmax)
    cnv.MouseMove
    |> Observable.subscribe (fun args ->
                                match (selxrange args) with
                                | Some i -> 
                                    let tmin,tmax = xintervals.[i]
                                    rect.Visibility <- Visibility.Visible
                                    let rectpos = plot.LogicalToPhysical(Rect(Point(tmin,ymin),Point(tmax,ymax)),xrange,yrange)
                                    Canvas.SetLeft(rect,rectpos.Left)
                                    Canvas.SetTop(rect,rectpos.Top)
                                    rect.Width <- rectpos.Width
                                    rect.Height <- rectpos.Height
                                | None -> 
                                    rect.Visibility <- Visibility.Collapsed)
    |> ignore
    cnv.MouseDown
    |> Observable.subscribe (fun args ->
                                match (selxrange args) with
                                | Some i -> 
                                    match (args.ChangedButton) with
                                    | MouseButton.Left -> f i
                                    | MouseButton.Right -> g i
                                    | _ -> ()
                                | None -> 
                                    ())
    |> ignore
