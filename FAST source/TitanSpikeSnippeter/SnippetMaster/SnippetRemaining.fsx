#r "System.Xml.Linq"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
fsi.ShowDeclarationValues <- false
open System.Xml.Linq
open System.IO
open Helper

let xs =   
  [|("635469861334333804", 241UL); ("635469948728671677", 2841UL);
    ("635470614534919782", 940UL); ("635470899844432701", 540UL);
    ("635471475517413661", 190UL); ("635471542399831755", 257UL);
    ("635471617433503161", 31UL); ("635473503060629065", 9878UL)|]

for fnum,firstblock in xs.[1..] do
let fpath = sprintf @"Y:\Data\Gunakari\%s" fnum
let offset chgroup = getOffset Local (sprintf @"%s\ChGroup_%s\SpikeTimes" fpath chgroup) (firstblock*450000UL+3000UL)

let settingsfile = sprintf @"%s\SnippeterSettings.xml" fpath
let settings = XElement.Load(settingsfile)
let numblocks = (FileInfo(sprintf "%s.rhd" fpath).Length/176L-3000L)/(450000L)
settings.Element(xn "FirstBlock").Value <- sprintf "%d" firstblock
settings.Element(xn "LastBlock").Value <- sprintf "%d" (numblocks-1L)
let es = settings.Element(xn "EIBElectrodePlacement")
let newes = 
    es.Elements(xn "ElectrodeSet")
    |> Seq.choose (fun x -> 
        let chgroup = x.Attribute(xn "Name").Value
        if (chgroup|>int) >= 8 then None else
        let off = offset chgroup
        printfn "%s %d" chgroup off
        x.Attributes(xn "Offset")|>Seq.iter(fun a -> a.Remove())
        x.Add(XAttribute(xn "Offset",off))
        Some x
    ) |> Seq.toArray
es.Elements().Remove()
es.Add(newes)
settings.Element(xn "MedianReferenceGroups").Elements()|>Seq.nth 1|>fun x -> x.Remove()
settings.Element(xn "Thresholds").Elements() |> Seq.filter (fun x -> 
    (x.Attribute(xn "Number")|>int) >= 32
) |> fun x -> x.Remove()
File.Copy(settingsfile,sprintf "%s.old" settingsfile)
settings.Save(settingsfile)
