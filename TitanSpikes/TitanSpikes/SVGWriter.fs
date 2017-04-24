module SVGWriter

open System.Xml.Linq
open System.Text
open RP.Controls
open System.Windows.Media

let inline xnel s = XNamespace.op_Addition (XNamespace.op_Implicit @"http://www.w3.org/2000/svg",s)
let inline xnat s = XName.op_Implicit s
let svgdoctype = 
    XDocumentType("svg", "-//W3C//DTD SVG 1.1//EN", "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd", null)

let inline svgtransform width height (xrange:Range<float>) (yrange:Range<float>) =
    let xt x = ((x|>float)-xrange.Min)*width/xrange.Interval()
    let yt y = height - ((y|>float)-yrange.Min)*height/yrange.Interval()
    xt,yt


let inline getSVGPolyLine style width height xrange yrange d =
    let xt,yt = svgtransform width height xrange yrange
    let sb = new StringBuilder()
    d |> Seq.iter (fun (x,y) -> sb.Append((xt x).ToString()+","+(yt y).ToString()+" ") |> ignore)
    XElement(xnel "polyline",XAttribute(xnat "points",sb.ToString()), XAttribute(xnat "style",style))

let inline col2string (col:Color) =
    sprintf @"#%02X%02X%02X" col.R col.G col.B

let getSVGLine xt1 yt1 xt2 yt2 (marker:Marker) = 
    let f a = a*255.F|>int
    XElement(xnel "line",
        XAttribute(xnat "x1",marker.x |> xt1),
        XAttribute(xnat "x2",marker.z |> xt2),
        XAttribute(xnat "y1",marker.y |> yt1),
        XAttribute(xnat "y2",marker.w |> yt2),
        XAttribute(xnat "style",sprintf "stroke-width:1;stroke:%s" 
            (sprintf @"#%02X%02X%02X" (f marker.r) (f marker.g) (f marker.b))
        )
    )

let getSVGXHair xt yt (marker:Marker) = 
    let f a = a*255.F|>int
    XElement(xnel "g",
        XAttribute(xnat "style",sprintf "stroke-width:2;stroke:%s" 
                (sprintf @"#%02X%02X%02X" (f marker.r) (f marker.g) (f marker.b))
        ),
        XElement(xnel "line",
            XAttribute(xnat "x1",(marker.x |> xt) - 10.),
            XAttribute(xnat "x2",(marker.x |> xt) + 10.),
            XAttribute(xnat "y1",marker.y |> yt),
            XAttribute(xnat "y2",marker.y |> yt)            
        ),
        XElement(xnel "line",
            XAttribute(xnat "x1",marker.x|> xt),
            XAttribute(xnat "x2",marker.x|> xt),
            XAttribute(xnat "y1",(marker.y |> yt) - 10.),
            XAttribute(xnat "y2",(marker.y |> yt) + 10.)
        )
    )

let inline getSVGPoint (x,y) r =
    XElement(xnel "circle",
        XAttribute(xnat "cx",x),
        XAttribute(xnat "cy",y),
        XAttribute(xnat "r",r)
    )    

let getSVGLineGroup width height xrange1 yrange1 xrange2 yrange2 (line:MarkerSeries2D) =    
    let xt1,yt1 = svgtransform width height xrange1 yrange1
    let xt2,yt2 = svgtransform width height xrange2 yrange2
    let f = if line.LineStyle = RenderType.XHair then getSVGXHair xt1 yt1 else getSVGLine xt1 yt1 xt2 yt2
    XElement(xnel "g", line.data|> Seq.map f)

let inline getSVGPointList width height xrange yrange r col points =
    let xt,yt = svgtransform width height xrange yrange
    XElement(xnel "g", XAttribute(xnat "style",sprintf "stroke:none;fill:%s" (col2string col)), 
             points |> Seq.map (fun (x,y) -> getSVGPoint (x|>xt,y|>yt) r))

let getSVGSubPlot width height (subplot:SubPlot2) =
    let ds = subplot.DisplayState :?> PlotSurfaceDisplayState2
    XElement(xnel "g",subplot.Lines |>Seq.cast |>Seq.map 
        (getSVGLineGroup width height
            ds.XRange ds.YRange ds.XRange2 ds.YRange2))

let getSVGBare contents = XElement(xnel "svg",contents)

let getSVG width height (contents:seq<XElement>) =
    XElement(xnel "svg", 
//        XElement(xnel "rect",
//            XAttribute(xnat "x",0.),
//            XAttribute(xnat "y",0.),
//            XAttribute(xnat "width",width),
//            XAttribute(xnat "height",height),
//            XAttribute(xnat "style","fill:none;stroke:gray;stroke-width:2")),
        contents)