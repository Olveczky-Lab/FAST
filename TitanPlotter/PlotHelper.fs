module PlotHelper

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open MathNet.Numerics.Statistics
open System.Windows.Media.Imaging
open System.Windows.Interop
open System.IO
open System.Windows.Shapes
open System.Windows.Controls
open System.Windows.Input
open IntelIPP

type HorizontalLineRangeControl(range:Range<float>,brush,cnv:Canvas) as this =
    inherit RangeControl()
    do 
        this.Range <- range
        let l = Line(Stroke = brush, StrokeThickness = 2.0, X1 =0., Y1 = 0.0, X2 = cnv.ActualWidth, Y2 = 0.0)
        this.Height <- cnv.ActualHeight
        this.Content <- l
        RenderOptions.SetEdgeMode(l,EdgeMode.Aliased)
        cnv.Children.Add(this) |> ignore
        let update () =
            let newpos = Ticks.LogicalToPhysical(this.Range.Min,this.Range.Max,cnv.ActualHeight,0.,Orientation.Vertical)
            l.Y1 <- newpos
            l.Y2 <- newpos
        range.Changed.Add (fun _ -> update ())
        cnv.SizeChanged
        |> Observable.subscribe (fun _ -> 
            l.X2 <- cnv.ActualWidth
            this.Height <- cnv.ActualHeight
            update ())
        |> ignore
        l.MouseEnter.Add(fun _ -> this.Cursor <- Cursors.ScrollNS)
        l.MouseLeave.Add(fun _ -> this.Cursor <- Cursors.Arrow)
        PanAndZoom.SetYAxisControl(this,this)
        PanAndZoom.SetYPanEnabled(this,true)
        PanAndZoom.SetYZoomEnabled(this,true)

let clamp min max x = if (x < min) then min else if (x > max) then max else x

let getOrderOfMag x =
    if (x = 0.0) then (x,"") else
    let order = clamp -5 4 (int (floor ((log10 (abs x))/3.0)))
    let prefix =  
        match order with
        | x when x = 4 -> "T"
        | x when x = 3 -> "G"
        | x when x = 2 -> "M"
        | x when x = 1 -> "k"
        | x when x = 0 -> ""
        | x when x = -1 -> "m"
        | x when x = -2 -> Char.ConvertFromUtf32(956)
        | x when x = -3 -> "n"
        | x when x = -4 -> "p"
        | x when x = -5 -> "f"
        | _ -> failwith "Not Possible"
    1.0/(10.0**(float (3*order))), prefix

let getOrderOfMagTime t = //t is in seconds
    let at = abs t
    if (abs t < 24.0*60.0*60.0) then
        if (abs t < 60.0*60.0) then 
            if (abs t < 60.0) then
                if (abs t  < 1.0) then
                    let multiplier,s = getOrderOfMag t
                    multiplier,s+"s"
                else 1.0,"s"
            else 1.0/60.0,"m"
        else 1.0/60.0/60.0,"h"
    else 1.0/24.0/60.0/60.0,"d"


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

let addSubPlot2 (wnd:D3DPlot2DWindow) xrange1 yrange1 xrange2 yrange2 lines =
    let sp = SubPlot2(DisplayState = PlotSurfaceDisplayState2(XRange = xrange1, YRange = yrange1, XRange2 = xrange2, YRange2 = yrange2, ClipToBounds = false), 
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
    l

let getSpikeLine color t0 dt nchans sp =
    let spikelen = Array.length sp/nchans
    let l = 
        let s = new PointSeries2D()
        s.data <- 
            seq{
                for i in 0..nchans-1 do
                    for j in 0..spikelen-1 do
                        yield t0+dt*(i*spikelen+j|>float),sp.[j*nchans+i]
            } |> Seq.toArray
        s
    l.Color <- color
    l.LineStyle <- RenderType.Aliased
    l

let getLinexy color data =
    let l = 
        let s = new PointSeries2D()
        s.data <- data
        s
    l.Color <- color
    l.LineStyle <- RenderType.Aliased
    l

let getPoints size color points =
    let l = new PointSeries2D(points|>Seq.toArray)
    l.Color <- color
    l.LineStyle <- RenderType.Points
    l.Width <- size
    l.Height <- size
    l

let getMarkerPoints,getLineList =    
    let l points = 
        new MarkerSeries2D(points |> Array.map (fun ((x:float,y:float),(z:float,w:float),col:Color) ->
            let inline f b = (b|>float32)/255.0f
            let inline g x = x |> float32
            new Marker(x=g x,y=g y,z=g z,w=g w,a=f col.A,r=f col.R,g=f col.G,b=f col.B)
    ))
    (fun points -> 
        let curl = l points
        curl.LineStyle <- RenderType.XHair
        curl),
    (fun points -> 
        let curl = l points
        curl.LineStyle <- RenderType.LineList
        curl)

let getLinexy2 color data =
    data |> Seq.pairwise |> Seq.map (fun (a,b) -> a,b,color) |> Seq.toArray |> getLineList

let getSpikeLineList color t0 dt nchans sp =
    let spikelen = Array.length sp/nchans
    seq{
        for i in 0..nchans-1 do
            let f j = t0+dt*(i*spikelen+j|>float),sp.[j*nchans+i]
            for j in 0..spikelen-2 do
                yield f j,f (j+1),color
    }

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
        getLinexy Colors.Black (Array.zip [|x1;x1;x2;x2;x1|] [|0.0;cnt;cnt;0.0;0.0|]) 
    addSubPlot wnd xrange yrange (counts|>Array.map (fun (pos,cnt) -> getbar pos (cnt*scale)) |> Seq.cast) |> ignore
    wnd,yrange
    

let brightColors = 
    [|"#FF008000";"#FF800000";"#FF0000FF";"#FF800080";"#FFFF6347";"#FFFF00FF";"#FF008080";"#FF00FFFF";"#FF000080";"#FFFF3939";"#FF7F7F00";|]
    |> Array.map (fun str -> ColorConverter.ConvertFromString(str) :?> System.Windows.Media.Color)

let colorIntensity (colors:Color[]) (v:float32) =
    let ncol = Array.length colors - 1 |> float32
    let i = v*ncol |> int
    let f = ncol*v-(i|>float32)
    let col1,col2 = colors.[i],colors.[i+1]
    let interp (a,b) = a + (b-a)*f
    Color.FromScRgb(interp (col1.ScA,col2.ScA),interp (col1.ScR,col2.ScR),interp (col1.ScG,col2.ScG),interp (col1.ScB,col2.ScB))

let heatmapcolors = 
    BitmapPalette(Array.init 256 (fun i -> 
        colorIntensity [|Colors.Black;Colors.Blue;Colors.Cyan;Colors.Green;Colors.Yellow;Colors.Red;Colors.White|] ((i|>float32)/256.f)))

let plotspikelines wndtitle xrange linestoplot nchans (lines:float[][]) = 
    let spikelen = lines.[0]
    let ls,min,max = 
        linestoplot
        |> Array.map (fun (i,color) -> (color,lines.[i]))
        |> Seq.fold (fun (ls,min,max) (color,s) ->                        
                        let curmax = Array.max s
                        let curmin = Array.min s
                        ((getSpikeLine color 0.0 1.0 nchans s::ls),
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

let plotPointBlurs pointsize xrange yrange wnd col points =
    let ampl = getPoints pointsize col points
    addSubPlot wnd xrange yrange [ampl] |> ignore
    wnd.KeyDown.Add (fun args -> 
        let delta_s, delta_a = 
            match args.Key with
            | Input.Key.D -> 1.0f,0
            | Input.Key.A -> -1.0f,0
            | Input.Key.S -> 0.0f,-5
            | Input.Key.W -> 0.0f,5
            | _ -> 0.0f,0
        ampl.Width <- ampl.Width + delta_s
        ampl.Height <- ampl.Height + delta_s
        ampl.Color <- Color.FromArgb(byte (clamp 0 255 ((int ampl.Color.A) + delta_a)),ampl.Color.R,ampl.Color.G,ampl.Color.B)
        wnd.Plot.ForceRefresh()
    )
    
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

let addCrosshair (plot:D3DPlot2D) (xrange:Range<double>) (yrange:Range<double>) brush =
    let cnv = plot.cnvMain
    let line otation = 
        let l = Line(Stroke = brush, StrokeThickness = 1.0)
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
    cnv.SizeChanged
    |> Observable.subscribe (fun _ -> 
                                hline.X2 <- cnv.ActualWidth
                                vline.Y2 <- cnv.ActualHeight)
    |> ignore
    let changes = 
        cnv.MouseMove
        |> Observable.map (fun args -> 
            let phy = args.GetPosition(cnv)
            let x = Ticks.PhysicalToLogical(xrange.Min,xrange.Max,cnv.ActualWidth,phy.X,Orientation.Horizontal)
            let y = Ticks.PhysicalToLogical(yrange.Min,yrange.Max,cnv.ActualHeight,phy.Y,Orientation.Vertical)
            x,y
        )
    changes,
    fun (x,y) ->
        let phyx = Ticks.LogicalToPhysical(xrange.Min,xrange.Max,cnv.ActualWidth,x,Orientation.Horizontal)
        let phyy = Ticks.LogicalToPhysical(yrange.Min,yrange.Max,cnv.ActualHeight,y,Orientation.Vertical)
        hline.Y1 <- phyy
        hline.Y2 <- phyy
        vline.X1 <- phyx
        vline.X2 <- phyx
    ,
    fun x -> 
        if x then 
            hline.Visibility <- Visibility.Visible
            vline.Visibility <- Visibility.Visible
        else
            hline.Visibility <- Visibility.Hidden
            vline.Visibility <- Visibility.Hidden

let addSelRect (plot:D3DPlot2D) (xrange:Range<double>) (yrange:Range<double>) xintervals f g =
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
                                    let rectpos = plot.LogicalToPhysical(Rect(Point(tmin,yrange.Min),Point(tmax,yrange.Max)),xrange,yrange)
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

let getStableClusters minclussize clus =
    let ntemp = Seq.head clus |> snd |> Array.length
    let rec rtree (temp,clu,count) (firsttemp,xs) = seq {
        let newxs = ((clu,count)::xs)
        let fatemap (temp,clu) =
            if (temp = ntemp-1) then [] else
                if (temp = -1) then clus else
                    clus |> Array.filter (fun (_,xs) -> Array.get xs temp = clu)
                |> Seq.countBy (fun (_,xs) -> Array.get xs (temp+1))
                |> Seq.filter (fun (_,cnt) -> cnt >=minclussize)
                |> Seq.sortBy fst
                |> Seq.toList
        match fatemap (temp,clu) with
        | [clunum,clucount] -> yield! rtree (temp+1,clunum,clucount) (firsttemp,newxs)
        | fates -> yield firsttemp,newxs|>List.rev
                   for (clunum,clucount) in fates do 
                        yield! rtree (temp+1,clunum,clucount) (temp+1,[])
    }
    rtree (-1,0us,Array.length clus) (-1,[])

let countClusters clus = 
    let ntemp = clus |> Seq.head |> snd |> Array.length
    Array.init ntemp (fun i ->  clus |> Seq.countBy (fun (_,c) -> c.[i]))

let plotline wndtitle dt line =
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    wnd.Title <- wndtitle
    let max = dt*((Array.length line)|>float)
    let xrange = Range(0.0,max)
    let (ymin,ymax) = line|>Seq.min,line|>Seq.max
    let yrange = 
        if (ymin = ymax) then
            Range(line.[0]-1.,line.[0]+1.)
        else
            Range(line|>Seq.min,line|>Seq.max)
    addXAxis wnd xrange Colors.Black "" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    addSubPlot wnd xrange yrange ([getLine Colors.Black 0.0 dt line]) |> ignore   

let plottimeline wndtitle dt line =
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    wnd.Title <- wndtitle
    let max = dt*((Array.length line)|>float)
    let xrange = Range(0.0,max)
    let (ymin,ymax) = line|>Seq.min,line|>Seq.max
    let yrange = 
        if (ymin = ymax) then
            Range(line.[0]-1.,line.[0]+1.)
        else
            Range(line|>Seq.min,line|>Seq.max)
    addTimeXAxis wnd xrange |> ignore
    addScaledYAxis wnd yrange Colors.Black (sprintf "%sV") |> ignore
    addSubPlot wnd xrange yrange ([getLine Colors.Black 0.0 dt line]) |> ignore   

let inline transform (r0:Range<float>) (r1:Range<float>) x =
    r1.Min + (x-r0.Min)/(r0.Max-r0.Min)*(r1.Max-r1.Min)

let aggRange (wnd:D3DPlot2DWindow) (range:Range<float>) rangepos =
    let rs = 
        rangepos |> Array.mapi (fun i ((a,dur),(x0,x1)) -> 
            let r = Range(0.0, 1.0)
            r.Changed.Add(fun _ -> 
                let t = transform r range
                let a,b = t x0, t x1
                rangepos.[i] <- (a,b-a),(x0,x1)
                wnd.Plot.ForceRefresh())
            r)
    let update () =
        rangepos |> Array.iteri (fun i ((a,dur),(x0,x1)) -> 
            let t = transform (Range(a,a+dur)) (Range(x0,x1))
            rs.[i].Set(t range.Min,t range.Max)
            )
    range.Changed.Add (fun _ -> update ())
    update ()
    rs

type SelectEvent<'a> =
    | Down of 'a
    | Move of 'a
    | Up


let addFreeSelRect (wnd:D3DPlot2DWindow) xrange yrange =
    let cnv = wnd.Plot.cnvMain
    let r = new Rectangle(Visibility = Visibility.Collapsed, Stroke = Brushes.Black, StrokeThickness = 1.)
    cnv.Children.Add(r)|>ignore
    cnv.MouseMove |> Observable.map (fun args -> args.GetPosition|>Move)
    |> Observable.merge (cnv.MouseLeftButtonDown |> Observable.map (fun args -> args.GetPosition|>Down))
    |> Observable.merge (cnv.MouseLeftButtonUp |> Observable.map (fun _ -> Up))
    |> Observable.scan (fun (selecting,selected) x ->
        match x with
        | Down pnt ->
            cnv.CaptureMouse()|>ignore
            let pnt = pnt cnv
            Canvas.SetLeft(r,pnt.X)
            Canvas.SetTop(r,pnt.Y)
            r.Visibility <- Visibility.Visible
            r.Width <- 0.
            r.Height <- 0.
            Some pnt,None
        | Move pnt ->
            match selecting with
            | Some initpos ->
                let pnt = pnt cnv
                let inline adjneg a b =
                    if a > b then b,a-b else a,b-a
                let x,dx = adjneg pnt.X initpos.X
                r.Width <- dx
                Canvas.SetLeft(r,x)
                let y,dy = adjneg pnt.Y initpos.Y
                r.Height <- dy
                Canvas.SetTop(r,y)
            | _ -> ()
            selecting,None
        | Up ->
            cnv.ReleaseMouseCapture()
            let selrect =
                wnd.Plot.PhysicalToLogical(Rect(Canvas.GetLeft(r),Canvas.GetTop(r),r.ActualWidth,r.ActualHeight),xrange,yrange)
            r.Visibility <- Visibility.Collapsed       
            None,Some selrect
    ) (None,None)
    |> Observable.choose snd

type TicksEvent =
    | Channel of int
    | SelChanged of Rect

let plotTicks xrange (yrange:Range<double>) col xs =
    let xs,m = 
        xs |> Array.mapi (fun i x -> i,x) |> Array.sortBy (snd>>fst)
        |> fun ys -> ys |> Array.map snd, ys |> Array.map fst
    let m' = m |> Array.mapi (fun i j -> j,i) |> Array.sortBy fst |> Array.map snd
    let wnd = new D3DPlot2DWindow()
    wnd.Show()    
    let npoints = Array.length xs
    let nchans = Array.length (xs.[0]|>snd)
    let inline f b = (b|>float32)/255.0f
    let changeMarkerScale,l =
        xs |> Array.map (fun (x,ys) -> (x,ys.[0]),(0.0,yrange.Interval()/100.0),col)
        |> getMarkerPoints
        |> fun l -> 
            addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
            l
        |> fun l -> 
            (fun x ->
                l.MarkerScaleY <- l.MarkerScaleY*x),l.data
    wnd.KeyDown |> Observable.subscribe (fun args ->
        let f x = 
            changeMarkerScale x
            wnd.Plot.ForceRefresh()
        let inc,dec = 1.25f,0.75f
        match args.Key with
        | Key.S -> f dec
        | Key.W -> f inc
        | _ -> ()
    ) |>ignore
    let chevt = 
        wnd.KeyDown |> Observable.choose (fun args ->
            let f basekey = 
                match (args.Key-basekey|>int) with
                | x when x < nchans && x >= 0 -> Some x
                | _ -> None
            match f Key.NumPad1 with
            | Some x -> Some x
            | None -> f Key.D1
        ) 
    chevt |> Observable.subscribe (fun chnum ->
        for i in 0.. npoints-1 do
            l.[i].y <- (xs.[i]|>snd).[chnum]|>float32
        wnd.Plot.ForceRefresh()
    ) |> ignore
    let selected =
        chevt |> Observable.map Channel
        |> Observable.merge (addFreeSelRect wnd xrange yrange |> Observable.map SelChanged)
        |> Observable.scan (fun (chnum,x) evt ->
            match evt with
            | Channel chnum ->
                chnum,x
            | SelChanged r ->
                let inside = getSpikesInWindow chnum r.Left r.Right r.Top r.Bottom xs
                chnum,Some (inside|>Array.map (fun i -> m.[i]))
        ) (0,None)
        |> Observable.choose snd
    (wnd,l),selected,
    fun i (col:Color) -> 
        let i'= m'.[i]
        l.[i'].a <- f col.A
        l.[i'].r <- f col.R
        l.[i'].g <- f col.G
        l.[i'].b <- f col.B

let plotshapes spikelen nchans yrange =
    let wnd = new D3DPlot2DWindow(Top=0.,Height=1112.,Width=1024.)
    wnd.Show()
    let xrange = Range(0.0,spikelen*nchans-1|>float)
    let gridlines =
        let yrange = Range(0.,1.)
        addSubPlot2 wnd xrange yrange xrange yrange
            ([0..nchans-1]|>Seq.map (fun chnum -> 
                chnum*spikelen+(spikelen/2-1)|>float
                |> fun x -> (x,0.),(x,1.),Colors.LightGray)|>Seq.toArray|>getLineList |> Seq.singleton |> Seq.cast)
    let yrange = Range(yrange)
    addYAxis wnd yrange Colors.Black "Amplitude (uV)" |> ignore
    addXAxis wnd xrange Colors.Black "" |> ignore
    let sp = addSubPlot2 wnd xrange yrange xrange yrange []
    (wnd,(sp,gridlines)),
    fun xs ->
        sp.Lines <-
            xs |> Seq.map (fun (shapes,col) ->
                shapes |> Seq.map (getSpikeLineList col 0. 1. nchans) |> Seq.concat |> Seq.toArray 
            )
            |> Seq.concat |> Seq.toArray |> getLineList |> Seq.singleton |> Seq.cast
        wnd.Plot.ForceRefresh()

let showSortedSpikes spikelen nchans spikess =
    let t0 = spikess |> Array.map (fst>>(fun x->Array.get x 0)>>fst) |> Array.min
    let t1 = spikess |> Array.map (fst>>(fun x->Array.get x (Array.length x - 1))>>fst) |> Array.max
    let xrange = Range(t0,t1)
    let yrange = Range (-400.,400.)
    let initdisplay = 
        spikess |> Array.scan (fun (acc,_) (spikes,(col,todisplay)) ->
            let n = Array.length spikes
            acc+n,Some (todisplay|>Array.map (fun x -> x+acc))
        ) (0,None) |> Array.choose snd
        |> Array.concat
    let initdisplay,allspikes = 
        spikess |> Array.mapi (fun i (spikes,(col,_)) -> spikes |> Array.mapi (fun j spike -> (col,spike),(i,j))) 
        |> Array.concat |> Array.mapi (fun i x -> i,x) |> Array.sortBy (snd>>snd>>fst)
        |> fun x -> 
            let shuffler = x |> Array.mapi (fun i (x,_) -> i,x) |> Array.sortBy snd |> Array.map fst
            initdisplay |> Array.map (fun i -> shuffler.[i]),
            Array.map snd x
    let spikeamps = 
        allspikes |> Array.map (fun ((col,(st,spk)),_) -> 
            st,Array.init nchans (fun ch -> Array.get spk ((spikelen/2-1)*nchans+ch)))
    let (wndshapes,_),plotfun = 
        let wnd,f = plotshapes spikelen nchans yrange 
        wnd,
        Array.map (fun x -> allspikes.[x]|>fst) 
        >>Seq.groupBy fst>>Seq.map (fun (k,v) -> v|>Seq.map (snd>>snd)|>Seq.toArray,k) 
        >>Seq.toArray>>f

    plotfun initdisplay
    wndshapes.Left <- 0.; wndshapes.Width <- 2048.; wndshapes.Height <- 1112.; wndshapes.Top <- 0.
    let (wndamps,_),selected,setcol = plotTicks xrange yrange Colors.LightGray spikeamps
    wndamps.Left <- 2048.; wndamps.Width <- 2048.; wndamps.Height <- 1112.; wndamps.Top <- 0.
    wndamps.Title <- ""
    for i in 0..Array.length allspikes-1 do
        setcol i (allspikes.[i]|>fst|>fst)
    addTimeXAxis wndamps xrange |> ignore
    addScaledYAxis wndamps yrange Colors.Black (sprintf "%sV") |> ignore
    selected |> Observable.subscribe (fun xs ->
        xs |> Array.map (fun x -> allspikes.[x]|>snd) |> Seq.countBy fst |> Seq.toArray |> printfn "%A"
        plotfun xs) |> ignore
    (wndamps,xrange),wndshapes

let plotspikerasters xranges bufferdt dt maxcnt cells = 
    let plot wnd (xrange:Range<float>) (t0,t1) yrange color alignedtimes = 
        let dtbefore,dtafter = -xrange.Min,xrange.Max
        let raster = 
            alignedtimes
            |> Array.mapi (fun i sts -> 
                let y = i|>float
                sts |> Array.map (fun st -> (st,y),(st,y+0.9),color)
            )    
            |> Array.concat
        let height = Array.length alignedtimes|>float
        let peth = 
            let h = Histogram(alignedtimes|>Array.concat,(t1-t0)/dt|>int,t0,t1)
            Array.init (h.BucketCount) (fun i -> (h.[i].LowerBound,h.[i].UpperBound),h.[i].Count)
            |> fun x ->
                let horz =       
                    x |> Array.map (fun ((a1,a2),b)  -> 
                        let y = b/maxcnt
                        (a1,y),(a2,y),Colors.Black
                    )
                let vert =
                    horz |> Seq.pairwise |> Seq.map (fun (((_,y1),_,_),((x,y2),_,_)) -> (x,y1),(x,y2),Colors.Black) |> Seq.toArray
                Array.append horz vert
        let alllines = Array.append raster peth
        alllines
        |> getLineList
        |> fun x -> 
            addSubPlot2 wnd xrange yrange xrange yrange [x]

    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    let ncells = Array.length cells
    let yrange,yrangepos = 
        cells |> Array.scan (fun (_,acc) (cell,_) ->
            let dt = Array.length (Array.get cell 0) |> float 
            Some ((acc,dt),(0.,dt)),
            acc+dt+bufferdt
        ) (None,0.0) |> fun x -> Range(0.,(x.[Array.length x-1]|>snd)),x|>Array.choose fst
    let yranges = aggRange wnd yrange yrangepos
    let xrange,xrangepos = 
        xranges |> Array.scan (fun (_,acc) (t0,t1) ->
            let dt = t1-t0
            Some ((acc,dt),(t0,t1)),
            acc+dt+bufferdt
        ) (None,0.0) |> fun x -> Range(0.,(x.[Array.length x-1]|>snd)),x|>Array.choose fst
    let xrangesagg = aggRange wnd xrange xrangepos 
    let sps = 
        Array.mapi2 (fun i (xs,col) yrange -> 
            Array.map2 (fun (xrange,x) alignedtimes ->
                plot wnd xrange x yrange col alignedtimes
            ) (Array.zip xrangesagg xranges) xs
        ) cells yranges
    wnd,(xrange,xrangesagg),yrange,sps

let medfilt n xs =
    let ys = Array.zeroCreate (Array.length xs)
    IPPS.FilterMedian(xs,ys,n)
    ys

let smooth n xs =
    let filter = FIRFilter(FIRFilterCoefficients(Array.init n (fun _ -> 1.0/(n|>float))))
    let ys = Array.zeroCreate (Array.length xs)
    filter.Filter(xs,ys)
    ys

let imfilter kernel im =
    IPPI.Filter(im,kernel)

let smoothkernel (nx,ny) (rx,ry) =
    let nxf = nx|>float32
    let nyf = ny|>float32
    let k = 
        Array2D.init (2*nx+1) (2*ny+1) (fun i j -> 
            let i = i|>float32
            let j = j|>float32
            let dist = (nxf-i)*(nxf-i)/(rx*rx) + (nyf-j)*(nyf-j)/(ry*ry)
            exp(-dist)
        )    
    let t = k |> Seq.cast |> Seq.sum
    k |> Array2D.map (fun x -> x/t)

let inline imhist (rx,ry) (((x0,x1),xn) as xp) (((y0,y1),yn) as yp) pnts =
    let inline f ((x0,x1),xn) x = (x-x0)/(x1-x0)*(xn|>float)|>int |> max 0 |> min (xn-1)
    let img = Array2D.zeroCreate<float32> yn xn
    pnts |> Seq.iter (fun (x,y) -> 
        let ix = f xp (x|>float)
        let iy = f yp (y|>float)
        img.[iy,ix] <- img.[iy,ix]+1.0f
    )
    let x = imfilter (smoothkernel (rx*2.f|>int,ry*2.f|>int) (rx,ry)) img
    x

let inline convert2Dto1D x = 
    let h,w = Array2D.length1 x,Array2D.length2 x
    let x' = Array.zeroCreate<float32> (w*h)
    Buffer.BlockCopy(x,0,x',0,w*h*sizeof<float32>)
    x'

let inline getheatmap (minx,maxx) (x:float32[,]) =
    let h,w = Array2D.length1 x,Array2D.length2 x
    let x' = convert2Dto1D x
    let x'' = x' |> Array.map (fun x -> (x-minx)/(maxx-minx)*255.f|>fun x -> x |> max 0.0f |> min 255.f |>byte)
    BitmapSource.Create(w, h, 96., 96., PixelFormats.Indexed8, heatmapcolors, x'', w)

let dispimg img = 
    let wnd = 
        Window(
            Width=800.,Height=800.,
            Content=Image(
                Stretch=Stretch.Fill,
                RenderTransformOrigin=Point(0.5,0.5),
                RenderTransform=ScaleTransform(1.0,-1.0),
                Source=img))
    wnd

let saveimage (im:BitmapSource) fname =
    use fs = new FileStream(fname,FileMode.Create)
    let enc = TiffBitmapEncoder()
    enc.Frames.Add(BitmapFrame.Create(im))
    enc.Save(fs)
