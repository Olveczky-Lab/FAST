module Plotting

open System.Threading
open System.Windows
open System.Text
open System.Windows.Threading
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Markup
open System.Collections.Generic
open System.Windows.Media
open System
open RP.Controls

let ui =
    let mk () =
        let wh = new ManualResetEvent(false)
        let application = ref null
        let start() =
            let app = Application()
            application := app
            ignore(wh.Set())
            app.Run() |> ignore
        let thread = Thread start
        thread.IsBackground <- false
        thread.SetApartmentState ApartmentState.STA
        thread.Start()
        ignore(wh.WaitOne())
        let app = !application
        //Hack to prevent all windows from being closed
        let wnd = app.Dispatcher.Invoke(fun () -> 
                                           let wnd = new Window()
                                           wnd.Show()
                                           ignore(wh.Set())
                                           wnd
                                       )
        ignore(wh.WaitOne())
        app.Dispatcher.Invoke(fun () -> 
                                wnd.Hide()
                                ()
                             )
        app,thread
    lazy (mk ())

let createPlotWnd () = 
    let app,thread = ui.Force()
    app.Dispatcher.Invoke(fun () -> 
                            let wnd = new D3DPlot2DWindow()
                            wnd.Show()
                            wnd
                            )

let execOnWnd (wnd:Window) f = wnd.Dispatcher.Invoke(f)

let plotY (wnd:D3DPlot2DWindow) color t0 dt data =    
    let l = new FixedTimeSeries(dt = dt, t0 = t0)
    l.data <- data
    l.Color <- ColorHelper.ColorFromHSV(color*360.0, 1.0, 1.0)
    l.LineStyle <- RenderType.AntiAliased
    l.Width <- 0.5f
    wnd.Plot.Lines.Add(l)
    l