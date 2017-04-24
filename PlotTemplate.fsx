#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\SnippetMaster.exe"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\D3DPlot2D.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\D3DPlot.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\bin\Release\RPCommon.dll"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\Math.Net Numerics\MathNet.Numerics.dll"
#r "UIAutomationTypes"
#r "PresentationCore"
#r "PresentationFramework"
#r "WindowsBase"
#r "System.Xaml"

#load @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikes\TitanSpikes\PlotHelper.fs"

open Helper
open PlotHelper
open System.Windows.Media
open RP.Controls

let axl = getAXL Local @"Z:\badlands\635276396558304920\AXL" 0UL (7500*300)
let wnd = D3DPlot2DWindow()
wnd.Show()
let xrange = Range(0.,300.)
let yrange = Range(0.,100000.)
addSubPlot wnd xrange yrange
    [getLine Colors.Blue 0.0 (1./7500.) (axl.[0..,0] |> Array.map (fun x -> (x|>float)))]
addXAxis wnd xrange Colors.Black "Time(s)"
addYAxis wnd yrange Colors.Black "AXL X"
