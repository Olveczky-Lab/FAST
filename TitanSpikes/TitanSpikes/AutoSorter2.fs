#if COMPILED
module AutoSorter2
#else
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open Helper
open System.IO
open System.Xml.Linq
open Nessos.FsPickler
open AutoSortHelper

System.Environment.CurrentDirectory <- @"D:\Rajesh\Ephys\Data Analysis\FSharp\ZMQRPC\ZMQRPC"
let exptid = 91
let sms = 
  [|(25, (635364677791445671L, (2278052672L, 2403760127L)));
    (33, (635364677791445671L, (4222062049L, 4347759164L)));
    (36, (635364677791445671L, (4870089869L, 4995870157L)));
    (47, (635364677791445671L, (6814170543L, 6939800842L)));
    (50, (635364677791445671L, (7462122302L, 7587758762L)));
    (58, (635364677791445671L, (9406162205L, 9531795271L)));
    (61, (635364677791445671L, (10054114144L, 10179779732L)));
    (69, (635364677791445671L, (11998118070L, 12123811697L)));
    (72, (635364677791445671L, (12646132889L, 12771839492L)));
    (80, (635364677791445671L, (14590036387L, 14715756331L)));
    (83, (635364677791445671L, (15238227051L, 15363833208L)));
    (91, (635364677791445671L, (17182106267L, 17307829386L)));
    (94, (635364677791445671L, (17829997917L, 17955908799L)));
    (102, (635364677791445671L, (19774114541L, 19899864908L)));
    (105, (635364677791445671L, (20422200952L, 20547846376L)));
    (262, (635383542263428056L, (116558426L, 242475027L)));
    (270, (635383542263428056L, (2060663845L, 2186496105L)));
    (273, (635383542263428056L, (2708668963L, 2834425390L)));
    (284, (635383542263428056L, (4652663871L, 4778428701L)));
    (287, (635383542263428056L, (5300598285L, 5426285296L)));
    (295, (635383542263428056L, (7244717937L, 7370367988L)));
    (298, (635383542263428056L, (7892687181L, 8018374522L)));
    (306, (635383542263428056L, (9836796488L, 9962493802L)));
    (309, (635383542263428056L, (10484663449L, 10610452775L)));
    (317, (635383542263428056L, (12428757831L, 12554500327L)));
    (320, (635383542263428056L, (13076820097L, 13202535675L)))|]
   |> Seq.groupBy (snd>>fst)
   |> Seq.toArray
   |> Array.map (fun (key,xs) -> key,xs|>Seq.map (fun (smid,(_,x)) -> smid,x)|>Seq.toArray)

let hostname = Remote "140.247.178.94:5900"
let localpath = @"Y:\Data\GaudMalhar"
let remotepath = @"/root/data/asheshdhawale/Data/GaudMalhar"

for fnum,sessions in sms do
    let fnum = sprintf "%d" fnum
    let fnumserverpath = sprintf "%s/%s" remotepath fnum
    let fnumlocalpath = sprintf "%s\%s\%s" localpath fnum
    let fpath = sprintf "%s/%s" fnumserverpath 
    let esets =
        use ms = new MemoryStream(getArray<byte> hostname (fpath "SnippeterSettings.xml") 0UL -1)
        loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
    let spikelen = 64

    let savesortall () = 
        let savesort chgroup sessionblocknum = 
            printfn "Doing %s" chgroup
            let nchans = esets|>Array.find (fun (x,_) -> x = chgroup)|>snd|>Array.length
            let numperblock = 1000
            let fname x = fpath (sprintf @"ChGroup_%s/%d/%s" chgroup sessionblocknum x)
            let l2sfname,l2stfname,(l2clusfname,l2clustemp),l2snumfname,outpath,stfname = 
                fname "Level2/Spikes", fname "Level2/SpikeTimes", (fname "Level2/Clusters",10),(fname @"Level2/ClusterSpikeNums"),
                (fpath (sprintf @"ChGroup_%s/AutoSort" chgroup)),(fpath (sprintf @"ChGroup_%s/SpikeTimes" chgroup))
            if File.Exists(sprintf @"%s\%s\ChGroup_%s\%d\Level2\Clusters" localpath fnum chgroup sessionblocknum) then
                let ntotalspikes = (getSize hostname (sprintf "%s.indx" l2clusfname) |> Async.RunSynchronously)/8L|>int    
                let firstblock,numblocks = 0,ntotalspikes/numperblock
                printfn "%d" numblocks
                if numblocks > 1 then
                    let maxtempnum = 2
                    calcAndSaveChainSort hostname l2sfname l2stfname l2snumfname stfname outpath (sprintf "%d_" sessionblocknum)
                        spikelen nchans l2clusfname l2clustemp maxtempnum numperblock firstblock numblocks
            else
                printfn "0"

        for _,(first,_) in sessions do
            for chgroup,chans in esets do
                let localf = sprintf @"%s\%s\ChGroup_%s\%s" localpath fnum chgroup
                let sfname = localf "SpikeTimes"
                let firstblock = getOffset Local sfname (first|>uint64)/1000UL
                printfn "Doing %d" firstblock
                savesort chgroup (firstblock|>int)
    
    
    let savesortmeta () =
        let x = 
            esets |> Array.map (fun (chgroup,_) ->
                printfn "Doing %s" chgroup
                Directory.GetFiles(fnumlocalpath (sprintf @"ChGroup_%s\AutoSort" chgroup),"*.stimes")
                |> Array.map (fun fname -> 
                    let ntotalspikes = (getSize Local fname |> Async.RunSynchronously)/8L|>uint64
                    (FileInfo(fname).Name |> fun x -> x.[0..Seq.length x-1-Seq.length ".stimes"]),
                    (getSpikeTimes Local fname 0UL 1 |> Seq.head,
                        getSpikeTimes Local fname (ntotalspikes-1UL) 1 |> Seq.head)))
        let fsp = new FsPickler()
        use ms = new MemoryStream()
        fsp.Serialize(ms,Array.zip (esets|>Array.map fst) x)
        let bytes = ms.ToArray()
        writeArray Local (fnumlocalpath "AutoSort.meta") 0UL bytes |> Async.RunSynchronously
    
    let savemergers () = 
        let minclussize = 15
        let minoverlap = 15
        let spikelen = 64
        let imax = 5
        let maxd = 0.
        let mergers =
            esets |> Array.map (fun (chgroup,chans) ->
                let nchans = Array.length chans
                let x = 
                    printfn "Starting ChGroup %s" chgroup
                    sessions |> Array.map (fun (_,(t0,t1)) ->
                        let merger = getMerger hostname fnumserverpath spikelen minclussize minoverlap imax maxd (t0|>uint64,t1|>uint64) chgroup nchans
                        merger)
                    |> Array.concat
                chgroup,x
                )
            |> Map.ofArray

        use ms = new MemoryStream()    
        mergers |> fun x -> FsPickler().Serialize(ms,x)
        writeArray Local (fnumlocalpath "sessionmergers") 0UL (ms.ToArray()) |> Async.RunSynchronously    
    
    let savegoodcells () =
        let taptimes = 
            use ms = new MemoryStream(getBytes hostname (sprintf @"%s/Behavior/%dLeverPressOpCon.tmap" fnumserverpath exptid) 0UL -1 |> Async.RunSynchronously)
            (new FsPickler()).Deserialize<Map<int,(int64*int64)>>(ms)

        let getGoodSpikeTimes ((t0,t1) as trange) = 
            let alltaps = taptimes |> Map.toArray |> Array.map (snd>>fst) |> Array.filter (fun x -> x >= (t0|>int64) && x <= (t1|>int64))
            let spiketimes = 
                getMergedCells trange hostname fnumserverpath
                |> Array.map (fun x -> x,loadSpikeTimes trange hostname fnumserverpath x)
                |> Array.sortBy (snd>>Array.length) |> Array.rev

            let rec coincidences n (xs:int64 list) (ys:int64 list) =
                match xs,ys with
                | hx::tx,hy::ty when (hx-hy)|>abs <= 5L -> coincidences (n+1) tx ty
                | hx::tx,hy::ty when hx > hy -> coincidences n xs ty
                | hx::tx,hy::ty when hx < hy -> coincidences n tx ys
                | _ -> n

            let goodspiketimes =
                let x = 
                    spiketimes
                    |> Array.filter (fun (_,sts) ->
                        let numtapswithspikes = 
                            alltaps |> Array.Parallel.choose (fun tt ->
                                sts |> Array.tryFind (fun st -> (st-tt)|>abs < 5L*30000L)
                            ) |> Array.length
                        numtapswithspikes > (Array.length alltaps)*9/10
                    )
                let ncells = Array.length x
                let cs =
                    Array.Parallel.init (ncells*ncells) (fun indx ->
                        let f i  = x.[i]|>snd|>Array.toList
                        let i,j = indx/ncells,indx%ncells
                        if i>=j then (i,j),0. else
                        let xi,xj = f i,f j
                        let n = coincidences 0 xi xj
                        (i,j),(n|>float)/(min (List.length xi) (List.length xj) |> float)
                    )
                    |> Array.choose (fun ((i,j),c) -> if c > 0.5 then Some (Set.ofList [i;j]) else None)
                mergeSets cs [|0..ncells-1|]
                |> List.map (fun set -> set |> Set.map (fun i -> x.[i]) |> Set.toArray |> Array.sortBy (fst>>fst>>int) |> Seq.head)
                |> List.toArray
            goodspiketimes

        let goodcells =
            Array.map (fun (smid,(t0,t1)) ->
                printfn "%d" smid
                let sts = getGoodSpikeTimes (t0|>uint64,t1|>uint64)
                printfn "%d" (Array.length sts)
                smid,sts |> Array.map fst
            ) sessions
        let fsp = new FsPickler()
        use ms = new MemoryStream()
        fsp.Serialize(ms,goodcells)
        writeArray Local (fnumlocalpath "sessiongoodcells") 0UL (ms.ToArray()) |> Async.RunSynchronously

    savegoodcells ()