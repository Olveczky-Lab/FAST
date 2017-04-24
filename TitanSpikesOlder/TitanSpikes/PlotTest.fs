module PlotTest

open RP.Controls
open System
open System.Windows
open System.Windows.Media

let addXAxis (wnd:D3DPlot2DWindow) xrange = 
    wnd.Plot.Settings.XAxesDisplayState.Add(AxisDisplayState(AxisRange = xrange))
let addYAxis (wnd:D3DPlot2DWindow) yrange color =
    wnd.Plot.Settings.YAxesDisplayState.Add(AxisDisplayState(AxisRange = yrange, AxisBrush = new SolidColorBrush(color)))
let addSubPlot (wnd:D3DPlot2DWindow) xrange yrange lines =
    wnd.Plot.AddSubPlot(SubPlot(DisplayState = PlotSurfaceDisplayState(XRange = xrange, YRange = yrange), 
                                Lines = lines))

let wnd = new D3DPlot2DWindow()
wnd.Show()

let xrange = Range(0.0,1.0)
let yrange1 = Range(-1.0,1.0)
let yrange2 = Range(-1.0,1.0)

addXAxis wnd xrange
addYAxis wnd yrange1 Colors.Blue
addYAxis wnd yrange2 Colors.Red

let dt = 0.0001;
let data f = [|0.0..dt..1.0|] |> Array.map (fun t -> t,sin (2.0*Math.PI*f*t))

addSubPlot wnd xrange yrange1 [PointSeries2D(points=data 10.0,Color=Colors.Blue,LineStyle=RenderType.AntiAliased,Width=3.0f)]
addSubPlot wnd xrange yrange2 [PointSeries2D(points=data 5.0,Color=Colors.Red,LineStyle=RenderType.AntiAliased,Width=3.0f)]

#if COMPILED
[<STAThread()>]
(new Application()).Run() |> ignore
#endif