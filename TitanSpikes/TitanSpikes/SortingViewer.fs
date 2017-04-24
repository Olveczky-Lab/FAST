#if COMPILED
module SortingViewer
#else
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open System.IO
open Helper
open System.Xml.Linq
open AutoSortHelper
open ClusterHelper
open PlotHelper
open Cluster
open Nessos.FsPickler
open AutoSortHelper
open ClusterTreeHelper
open System.Windows.Media
open RP.Controls
open System.Text.RegularExpressions

let hostname = Remote "140.247.178.94:5900"
let ratpath = sprintf @"/root/data/steffenwolff/RAT20/%s"
let fnums = 
  [|"635533990252658654"|]

let mergertree = 
    fnums |> Array.map (fun fnum ->
        use ms = new MemoryStream(getArray<byte> hostname (ratpath (sprintf @"%s/allmergers" fnum)) 0UL -1)
        FsPickler().Deserialize<string * (string * Tree<Set<string> * (int * float) option> []) []>(ms)
    )

let allmergers =
    mergertree |> Array.map (fun (fnum,xss) ->
        fnum,(xss |> Array.map (fun (chgroup,xs) -> chgroup,xs|>Array.map (fun (Node((a,_),_)) -> a |> Set.toArray)) |> Map.ofArray)
    ) |> Map.ofArray

let sms = Map.empty
//    use ms = new MemoryStream(getArray<byte> hostname (ratpath "OpConHRTwoTapSMTimes") 0UL -1)
//    FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms) |> Map.ofArray

let plotl2stuff = 
    let chgroup = "1"
    let spikelen = 64
    let nchans,fnums =
        mergertree |> Seq.groupBy (fun (fnum,_) ->
            let esets =
                let fpath = sprintf @"%s/%s" (ratpath (sprintf "%s" fnum)) 
                use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
                loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
            esets |> Array.find (fst>>((=)chgroup)) |> snd |> Array.length
        ) |> Seq.nth 0 |> fun (x,y) -> x, (y|>Seq.map fst|>Seq.toArray)
    let spikess = 
        fnums 
        |> Array.map (fun fnum ->
            printfn "%s %d" fnum nchans
            let fpath = sprintf @"%s/%s" (ratpath (sprintf "%s/ChGroup_%s" fnum chgroup))
            let sortedspikenums = 
                allmergers.[fnum].[chgroup] |> Array.mapi (fun i x -> 
                    getSpikeTimes hostname (fpath (sprintf @"MergedChains/%d.l2snums" i)) 0UL -1
                )
            let n = Array.length sortedspikenums
            let unsortedspikenums =  
                let nspikes = (getSize hostname (fpath @"Level2/SpikeTimes") |> Async.RunSynchronously)/8L|>uint64
                Set.difference (Set.ofArray [|0UL..nspikes-1UL|]) (Set.ofArray (sortedspikenums |> Array.concat)) |> Set.toArray
            getSpikeSubsets hostname (fpath @"Level2/Spikes") (fpath @"Level2/SpikeTimesRaw") (spikelen*nchans) 
                (Array.append sortedspikenums [|unsortedspikenums|])
            |> Array.map (Array.Parallel.map (fun (st,spk) ->
                st,spk|>Array.map (fun x -> 0.195*(x|>float))|>scalespike nchans)
            )
        )
    let offsets = 
        let start = fnums.[0] |> uint64
        fnums |> Array.map (fun fnum ->
            ((fnum |> uint64) - start)*3UL/1000UL
        )
    let (l2spikes:(_*(_*int[]))[]) = 
        Array.map2 (fun n xs ->
            let nxs = Array.length xs
            let x = xs |> Array.map (Array.map (fun (st,spk) -> (st+n|>float)/30000.,spk))
            x.[0..nxs-2],x.[nxs-1]
        ) offsets spikess
        |> fun xs -> 
            Array.append 
                (xs |> Array.map fst |> Array.concat |> Array.mapi (fun i x -> 
                    x,((brightColors.[i%(Array.length brightColors)]),[||]))) 
                [|xs |> Array.map snd |> Array.concat,(Colors.LightGray,[||])|]
        
    let (wnd,xrange),_ = showSortedSpikes spikelen nchans l2spikes
    let yrange = Range(0.,1.)
    let l =
        Array.map2 (fun offset fnum -> 
            sms |> Map.toArray |> Array.choose (fun (_,(x,(t0,t1))) ->
                if fnum = x then Some [|(t0|>uint64)+offset;(t1|>uint64)+offset|] else None
            )
        ) offsets (fnums|>Array.map int64) |> Array.concat |> Array.concat
        |> Array.map (fun t ->
            let t = (t|>float)/30000.
            (t,0.),(t,1.),Colors.LightGray)|> getLineList
    addSubPlot2 wnd xrange yrange xrange yrange [l]

let plotrawstuff chgroup ((exptid,smid) as sm,cellss) =
    let fnum,((t0,t1) as trange) = 
        let fnum,(t0,t1) = sms.[sm]
        fnum,(t0|>uint64,t1|>uint64)
    let fpath = sprintf @"%s/%s" (ratpath (sprintf "%d" fnum)) 
    let esets =
        use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
        loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
    let spikelen = 64
    printfn "%d %d" exptid smid
    if Array.length cellss > 0 then
        let nchans = esets |> Array.find (fst>>((=)chgroup)) |> snd |> Array.length
        let allspikes =
            let spikenumss = 
                cellss |> Array.mapi (fun i cells ->
                    cells |> Array.map (fun cell ->             
                        let snumfname,stfname = 
                            let f x = fpath (sprintf "ChGroup_%s/MergedChains/%s.%s" chgroup cell x)
                            f "snums",f "stimes"
                        let firstindx,lastindx = getSubSetindx hostname stfname trange
                        let n = lastindx-firstindx+1UL|>int   
                        cell,
                        getSpikeTimes hostname snumfname firstindx n
                    )
                )
                |> Array.concat
            let f x = fpath (sprintf "ChGroup_%s/%s" chgroup x)
            getSpikeSubsets hostname (f "Spikes") (f "SpikeTimes") (nchans*spikelen) (spikenumss |> Array.map snd)
            |> Array.map (Array.Parallel.map (fun (st,spk) ->
                (st-t0|>float)/30000.,spk|>Array.map (fun x -> 0.195*(x|>float))|>scalespike nchans)
            )
            |> fun x -> Array.zip (spikenumss |> Array.map fst) x
            |> Map.ofArray
        let spikess =
            cellss |> Array.mapi (fun i cells ->
                cells |> Array.map (fun cell ->
                    allspikes.[cell]
                ) |> Array.concat |> Array.sortBy fst
                ,((brightColors.[i%(Array.length brightColors)]),[||])
            )
        let (wnd1,_),wnd2 = showSortedSpikes spikelen nchans spikess
        wnd1.Title <- sprintf "%d %d" exptid smid
        wnd2.Title <- sprintf "%d %d" exptid smid

let savegoodcellspiketimes () =
    let goodcells =
        let cellnums =
            seq {
                for chgroup in 0..15 do
                    let chgroup = sprintf "%d" chgroup
                    yield
                        chgroup,
                        mergertree |> Seq.groupBy fst |> Seq.map (fun (fnum,_) ->
                            let n = allmergers.[fnum].[chgroup]|>Array.length
                            [|0..n-1|] |> Array.map (fun i -> fnum,i)
                        ) |> Array.concat |> Array.mapi (fun i x -> i,x) |> Map.ofArray
            } |> Map.ofSeq
        use ms = new MemoryStream(getArray<byte> hostname (ratpath "GoodCells.txt") 0UL -1)
        use sr = new StreamReader(ms)
        let lines = sr.ReadToEnd().Split('\n')
        let chgroups =
            lines
            |> Array.mapi (fun i x -> i,x)
            |> Array.choose (fun (i,x) ->
                let m = Regex.Match(x,@"^ChGroup_([0-9]+)")
                if m.Success then Some (i,m.Groups.[1].Value) else None
            )
        let goodcells = 
            seq{yield! chgroups|>Array.map fst;yield Array.length lines} |> Seq.pairwise
            |> Seq.zip (chgroups|>Array.map snd)
            |> Seq.map (fun (chgroup,(i,j)) -> 
                chgroup,
                lines.[i+1..j-1] |> Array.map (fun l ->
                    let l = if l.Contains(@"//") then l.Substring(0,l.IndexOf(@"//")) else l
                    Regex.Split(l,@"\W+")
                    |> Array.choose (fun x -> 
                        match System.Int32.TryParse(x) with
                        | true,x -> Some x
                        | _ -> None
                    )
                )
            )
            |> Seq.toArray
        goodcells |> Array.map (fun (chgroup,xss) ->
            chgroup,
            xss |> Array.map (Array.map (fun x ->
                printfn "%s %d" chgroup x
                let fnum,cellnum = cellnums.[chgroup].[x]
                getSpikeTimes hostname (ratpath (sprintf @"%s/ChGroup_%s/MergedChains/%d.stimes" fnum chgroup cellnum)) 0UL -1
                |> Array.map (fun st -> (fnum|>int64),st)
            )>>Array.concat>>Array.sort)
        )
    use ms = new MemoryStream()
    FsPickler().Serialize(ms,goodcells)
    writeArray hostname (ratpath @"GoodCellsSpikeTimes") 0UL (ms.ToArray()) |> Async.RunSynchronously

let savegoodcellsessionspiketimes () =
    let goodcells =
        let cellnums =
            let mergertree = 
                use ms = new MemoryStream(getArray<byte> hostname (ratpath "allmergers") 0UL -1)
                FsPickler().Deserialize<(string * (string * Tree<Set<string> * (int * float) option> []) []) []>(ms)
            let allmergers =
                mergertree |> Array.map (fun (fnum,xss) ->
                    fnum,(xss |> Array.map (fun (chgroup,xs) -> chgroup,xs|>Array.map (fun (Node((a,_),_)) -> a |> Set.toArray)) |> Map.ofArray)
                ) |> Map.ofArray
            seq {
                for chgroup in 0..15 do
                    let chgroup = sprintf "%d" chgroup
                    yield
                        chgroup,
                        mergertree |> Seq.groupBy fst |> Seq.map (fun (fnum,_) ->
                            let n = allmergers.[fnum].[chgroup]|>Array.length
                            [|0..n-1|] |> Array.map (fun i -> fnum,i)
                        ) |> Array.concat |> Array.mapi (fun i x -> i,x) |> Map.ofArray
            } |> Map.ofSeq
        let goodcells = 
            use ms = new MemoryStream(getArray<byte> hostname (ratpath "GoodCells.txt") 0UL -1)
            use sr = new StreamReader(ms)
            let lines = sr.ReadToEnd().Split('\n')
            let chgroups =
                lines
                |> Array.mapi (fun i x -> i,x)
                |> Array.choose (fun (i,x) ->
                    let m = Regex.Match(x,@"^ChGroup_([0-9]+)")
                    if m.Success then Some (i,m.Groups.[1].Value) else None
                )
            seq{yield! chgroups|>Array.map fst;yield Array.length lines} |> Seq.pairwise
            |> Seq.zip (chgroups|>Array.map snd)
            |> Seq.map (fun (chgroup,(i,j)) -> 
                chgroup,
                lines.[i+1..j-1] |> Array.map (fun l ->
                    let l = if l.Contains(@"//") then l.Substring(0,l.IndexOf(@"//")) else l
                    Regex.Split(l,@"\W+")
                    |> Array.choose (fun x -> 
                        match System.Int32.TryParse(x) with
                        | true,x -> Some x
                        | _ -> None
                    )
                )
            )
            |> Seq.toArray
        goodcells |> Array.map (fun (chgroup,xss) ->
            chgroup,
            xss |> Array.mapi (fun i xs -> 
                printfn "%s %d" chgroup i
                sms |> Map.toArray |> Array.choose (fun ((exptid,smid),(smfnum,(t0,t1))) ->
                    let data =
                        xs |> Array.map (fun x ->
                            let fnum,cellnum = cellnums.[chgroup].[x]
                            if smfnum <> (fnum|>int64) then [||] else
                            let stfname = ratpath (sprintf @"%s/ChGroup_%s/MergedChains/%d.stimes" fnum chgroup cellnum)
                            let firstindx,lastindx = getSubSetindx hostname stfname (t0|>uint64,t1|>uint64)
                            if lastindx < firstindx then [||] 
                            else getSpikeTimes hostname stfname firstindx (lastindx-firstindx+1UL|>int)
                        )
                    let sts = data|>Array.concat|>Array.sort
                    if Array.length sts > 10 then 
                        printfn "%d" (Array.length sts)
                        Some ((exptid,smid),sts)
                    else None
                )
            )
        )
    use ms = new MemoryStream()
    FsPickler().Serialize(ms,goodcells)
    writeArray hostname (ratpath @"GoodCellsSessionSpikeTimes") 0UL (ms.ToArray()) |> Async.RunSynchronously
