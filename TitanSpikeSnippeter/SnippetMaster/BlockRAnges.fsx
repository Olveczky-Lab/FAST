#r "System.Xml.Linq"
#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\TitanSpikeSnippeter\SnippetMaster\bin\Release\SnippetMaster.exe"
fsi.ShowDeclarationValues <- false
open System.Xml.Linq
open System.Text.RegularExpressions
open System.IO
open Helper

let serverpath = sprintf @"/root/data/asheshdhawale/Data/%s/%s"
let blockranges = 
    let lines = File.ReadAllLines(@"Z:\Data\BlockRanges.txt")
    lines |> Array.fold (fun (x,r) l ->
        match x with
        | None -> 
            let [|rat;fnum|] = l.Split(' ')
            Some ((rat,fnum),[]),r
        | Some ((rat,fnum),ranges) ->
            match l with
            | "" ->
                None,((rat,fnum),ranges|>List.rev)::r
            | _ ->
                let [|offset;nblocks|] = l.Split(' ')
                Some ((rat,fnum),((offset|>uint64),(nblocks|>int))::ranges),r
    ) (None,[]) |> snd |> List.rev

let files =
    let files = 
        File.ReadAllLines(@"C:\Users\rpoddar\Downloads\Snippeting18.txt")
        |> Array.map (fun x -> x.Split())
        |> Array.fold (fun state x -> 
            match x with
            | [|""|] -> state
            | [|rat|] when (Regex.IsMatch(rat,"^\d+$")|>not) -> 
                (rat,[])::state
            | _ -> 
                match state with
                | (rat,xs)::ys ->
                    let file::rest = x |> Array.filter (fun a -> a <> "")  |> Array.toList
                    let rest = rest |> List.map int64
                    let chstoexclude = 
                        rest |> List.choose (fun r -> 
                            if r <= 64L then Some (r-1L|>int) else None
                        ) |> List.toArray
                    (rat,((file,chstoexclude)::xs))::ys
                | _ -> failwith "Not Possible"
        ) [] |> List.map (fun (rat,xs) -> rat,xs|>Map.ofList) |> Map.ofList
    blockranges |> List.map (fun ((rat,fnum),ranges) ->
        let fnum,o =
            let index = fnum.LastIndexOf('_')
            if index > -1 then fnum.Substring(0,index),(fnum.Substring(index+1)|>uint64)*176UL
            else fnum,0UL
        let chstoexclude = files.[rat].[fnum]
        ranges |> List.choose (fun (offset,nblocks) ->
            if offset > 0UL && nblocks > 1 then
                Some (rat,(fnum,chstoexclude,Some ((nblocks|>int64)*450000L),Some (o+offset)))
            else None
        )
    ) |> List.concat 
    |> Seq.groupBy fst |> Seq.map (fun (a,b) -> a,(b|>Seq.map snd|>Seq.toArray)) |> Seq.toArray

let r () =
    for rat,fnum,nblocks in blockranges |> List.choose (fun ((rat,fnum),ranges) ->
        match ranges with
        | (0UL,nblocks)::_ -> Some (rat,fnum,nblocks)
        | _ -> None
        ) do
        let fpath = sprintf @"Z:\Data\%s\%s" rat fnum
        let settingsfile = sprintf @"%s\SnippeterSettings.xml" fpath
        let settings = XElement.Load(settingsfile)
        let lastblock = settings.Element(xn "LastBlock").Value |> int
        if lastblock < nblocks-1 then
            printfn "'%s'," (serverpath rat fnum)
            let firstblock = lastblock+1|>uint64
            printfn "%d" (nblocks-lastblock-1)
            let offset chgroup = getOffset Local (sprintf @"%s\ChGroup_%s\SpikeTimes" fpath chgroup) (firstblock*450000UL+3000UL)
            settings.Element(xn "FirstBlock").Value <- sprintf "%d" firstblock
            settings.Element(xn "LastBlock").Value <- sprintf "%d" (nblocks-1)
            settings.Element(xn "EIBElectrodePlacement").Elements(xn "ElectrodeSet")
            |> Seq.iter (fun x -> 
                let chgroup = x.Attribute(xn "Name").Value
                let off = offset chgroup
                printfn "%s %d" chgroup off
                x.Attributes(xn "Offset")|>Seq.iter(fun a -> a.Remove())
                x.Add(XAttribute(xn "Offset",off))
            )
            settings.Save(settingsfile)
