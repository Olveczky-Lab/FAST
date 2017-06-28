module SVGWriter

open System.Xml.Linq
open System.Text
open RP.Controls

let inline xn s = XName.Get(s)

let getSVGPolyLine style d =
    let sb = new StringBuilder()
    d |> Seq.iter (fun (x,y) -> sb.Append(x.ToString()+","+y.ToString()+" ") |> ignore)
    XElement(xn "polyline",XAttribute(xn "points",sb.ToString()), XAttribute(xn "style",style))