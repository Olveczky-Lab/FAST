#if COMPILED
module EIBElectrodePlacement
#else
#r @"WindowsBase.dll"
#r @"PresentationCore.dll"
#r @"PresentationFramework.dll"
#r @"System.Xaml.dll"
#r @"UIAutomationTypes.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"
#endif

open System.Linq
open System.Xml.Linq
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
open Microsoft.FSharp.Linq.NullableOperators
open System

let xn n = XName.op_Implicit(n)
let savenTrodes (fname:string) =
    let getnTrodes () = 
        let f = @"D:\Rajesh\Ephys\RHD2000\eib\eib.brd"
        let x = XElement.Load(f)

        let getElementPos i j = 
            let n = sprintf "%d_%d" i j
            let e = 
                x.Descendants(xn "element")
                    .First(fun a -> a.Attribute(xn "name").Value = n)
            e.Attribute(xn "x")|>float,e.Attribute(xn "y")|>float

        let (minX,maxX,minY,maxY) = (0.0,18.0,5.0,24.0)

        let wnd = new Window(ResizeMode=ResizeMode.NoResize)
        let width = 800.0
        let height = 800.0
        let elsize = 15.0

        let transform (x,y) =
            ((x-minX)*width/(maxX-minX)-elsize/2.0),((y-minY)*height/(maxY-minY)-elsize/2.0)

        let cnv = new Canvas()
        x.Descendants(xn "element")
            .Where(fun (a:XElement) -> a.Attribute(xn "package").Value = "PAD_20_37")
        |> Seq.iter (fun pad -> 
                        let (x,y) = (pad.Attribute(xn "x")|>float,pad.Attribute(xn "y")|>float) |> transform
                        let el = new Ellipse(Fill=Brushes.Green,Width=elsize,Height=elsize)
                        Canvas.SetLeft(el,x)
                        Canvas.SetTop(el,y)
                        cnv.Children.Add(el) |> ignore)
        let txts = 
            [0..1] |> List.map (fun j -> [0..31] |> List.map (fun i ->
                                                                let x,y = (getElementPos i j) |> transform
                                                                let txt = new TextBox(Width=25.0,
                                                                                        BorderThickness=Thickness(2.0),
                                                                                        BorderBrush=Brushes.Black,
                                                                                        TextAlignment=TextAlignment.Center)
                                                                Canvas.SetLeft(txt,x-5.0)
                                                                Canvas.SetTop(txt,y+elsize+2.0)
                                                                cnv.Children.Add(txt) |> ignore
                                                                txt))
            |> List.concat
            |> List.mapi (fun i txt -> (i,txt))

        let btn = new Button(Content="OK",Width=30.0,Height=25.0)
        btn.Click.Add(fun e -> wnd.DialogResult <- Nullable true)
        Canvas.SetLeft(btn,width/2.0-15.0)
        Canvas.SetTop(btn,height/2.0-12.5)
        cnv.Children.Add(btn) |> ignore
        wnd.Content <- cnv
        wnd.Width <- width
        wnd.Height <- height
        wnd.Title <- "EIB Electrode Mapping"
        let r = wnd.ShowDialog()
        if (r ?= true) then        
            Some (txts |> List.map (fun (i,txt) -> (i,txt.Text))
                       |> Seq.groupBy snd |> Seq.toList 
                       |> List.map (fun (x,ys) -> (x,ys |> Seq.map fst |> Seq.toList)))
        else
            None

    let r = getnTrodes ()    
    match r with
    | None -> ()
    | Some l -> 
        XElement(xn "EIBElectrodePlacement", 
                 l |> List.map (fun (name,channels) -> 
                                    XElement(xn "ElectrodeSet",XAttribute(xn "Name",name),
                                             channels |> List.map (fun chnum -> XElement(xn "Channel",chnum))))).Save(fname)
