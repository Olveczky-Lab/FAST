#r "System.Xml.Linq"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
fsi.ShowDeclarationValues <- false
open System.Xml.Linq
open System.IO
open Helper

let fpath = @"Z:\Data\Champakali\635273160844487866"
let firstblock = 8000UL
let offset chgroup = getOffset Local (sprintf @"%s\ChGroup_%s\SpikeTimes" fpath chgroup) (firstblock*450000UL+3000UL)

let settingsfile = sprintf @"%s\SnippeterSettings.xml" fpath
let settings = XElement.Load(settingsfile)
settings.Element(xn "FirstBlock").Value <- sprintf "%d" firstblock
settings.Element(xn "EIBElectrodePlacement").Elements(xn "ElectrodeSet")
|> Seq.iter (fun x -> 
    let chgroup = x.Attribute(xn "Name").Value
    let off = offset chgroup
    printfn "%d" off
    x.Attributes(xn "Offset")|>Seq.iter(fun a -> a.Remove())
    x.Add(XAttribute(xn "Offset",off))
)
settings.Save(settingsfile)
