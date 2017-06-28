#if COMPILED
module ClusterTreeViewer
#else
#load "Subject.fs"
#load "ClusterViewerHelper.fs"
fsi.ShowDeclarationValues <- false
#time "on"
#endif

open System.IO
open Cluster
open ClusterHelper
open ClusterViewerHelper
open ClusterTreeHelper
open System.Windows
open RP.Controls
open PlotHelper
open Helper
open System.Windows.Media
open Nessos.FsPickler
open System.Xml.Linq
open SequentialClusterHelper
open SegmentationFusion
open System.Windows.Input
open Microsoft.FSharp.Linq
open AutoSortHelper
System.Environment.CurrentDirectory <- @"D:\Rajesh\Ephys\Data Analysis\FSharp\ZMQRPC\ZMQRPC"

let hostname = Remote "140.247.178.94:5900"
let fpath = @"/root/data/asheshdhawale/Data/Gandhar/635330705373882279"
let esets =
    use ms = new MemoryStream(getArray<byte> hostname (sprintf @"%s/SnippeterSettings.xml" fpath) 0UL -1)
    loadnTrodes (XElement.Load(ms).Element(xn "EIBElectrodePlacement"))
let chgroup,spikelen = "1",64
let nchans = esets|>Array.find (fun (x,_) -> x = chgroup)|>snd|>Array.length
let ntemp = 10
let numperblock = 1000

let fname = sprintf @"%s/ChGroup_%s/Level2/%s" fpath chgroup
let sfname,stfname,(clusfname,clustemp) = 
    fname "Spikes", fname "SpikeTimes", (fname "Clusters",10)
let ntotalspikes = (getSize hostname (sprintf "%s.indx" clusfname) |> Async.RunSynchronously)/8L|>int
printfn "Total Blocks = %d" (ntotalspikes/numperblock)
    
let getBlockNums () =
    [|20000000000UL|]
    |> Array.map (fun offset ->
        getOffset hostname (sprintf @"%s/ChGroup_%s/SpikeTimes" fpath chgroup) (offset|>uint64)
    )
    |> fun x -> 
        printfn "%A" x
        x
    |> getMedianOffset hostname (fname "ClusterSpikeNums")
    |> Array.map (int>>fun x -> x/numperblock)
    |> printfn "%A"


let firstblock,numblocks = 
    0,(ntotalspikes/numperblock)

let yrange = Range(-400.0,400.0)

let xrange = Range((firstblock|>float)-0.5,(firstblock+numblocks|>float)-0.5)

let variancePartition shapes (Node(t,_) as x) =
    let rec loop (Node(t,sts)) =
        if Array.length sts = 0 then [|t.Items,t.Shape|] else
        sts |> Array.map loop |> Array.concat
        |> mergeBad (getscore getvar) 20. shapes (t.Items,t.Shape)
        |> List.toArray
    let root = {t with Stability=t.Items|>Array.length}
    let leaves = 
        loop x|>Array.map (fun (items,shape) -> 
            let cnt = Array.length items
            Node((makeStableCluster 0 items cnt shape),[||])
        )
    Node(root,leaves)

let clusterblocks,shapes,tranges,spikeamps,chains = 
    let clustertrees,clus = 
        loadClusterTree hostname sfname stfname spikelen nchans clusfname clustemp numperblock firstblock numblocks
    let shapes = clus |> Array.map (Array.map (fst>>snd)) |> Array.concat
//    let clustertrees =
//        clustertrees |> Array.Parallel.map (mapTree (fun (_,clu) _ -> clu)>>variancePartition shapes>>NumberNodes)
    let clustertrees =
        let rec loop (Node(t,sts)) =                
            Node(t,sts|>Array.filter (fun (Node((_,clu),_)) -> clu.Temp <= 7)|> Array.map loop)
        clustertrees |> Array.Parallel.map (fun clustertree ->
            loop clustertree
        )
    let chains = []//chainClusterTrees clustemp nchans clustertrees
                
    let clusterblocks = 
        Array.Parallel.init numblocks (fun blocknum -> 
//            let initstate =
//                clustertrees.[blocknum] |> mapTree (fun (_,clu) _ -> if clu.Temp <= 0 then (Visible,clu) else (Hidden,clu))
            let initstate =
                clustertrees.[blocknum] |> mapTree (fun (_,clu) _ -> if clu.Temp <= 7 then (Visible,clu) else (Hidden,clu))
            chains |> List.concat |> List.filter (fst>>((=)blocknum)) |> List.map snd
            |> List.fold (fun tree x -> 
                let rec loop (Node((_,clu),sts)) path =
                    match path with 
                    | [] -> Node((Visible,clu),sts)
                    | i::tail -> 
                        sts.[i] <- loop sts.[i] tail
                        Node((Visible,clu),sts|>Array.map (fun (Node((_,clu),sts)) -> Node((Visible,clu),sts)))
                loop tree x
            ) initstate
            |> fun clustertree -> 
            {StableClusters=clustertree;XPos=(blocknum+firstblock|>float)-0.5,1.0;})

    let tranges = clus |> Array.map (fun xs -> xs.[0]|>fst|>fst,xs.[Array.length xs-1]|>fst|>fst)
    let spikeamps = 
        clus |> Array.map (Array.map (fun ((st,spk),_) -> 
            st,Array.init nchans (fun ch -> Array.get spk (31*nchans+ch)))) |> Array.concat        
    clusterblocks,shapes,tranges,spikeamps,chains
     
let clustertrees = clusterblocks|>Array.map (fun x -> x.StableClusters |> mapTree (fun (_,x) _ -> x))

let (wnd,_),xranges,yranges,cluchanged,save = plotStableClusters (spikelen,nchans,ntemp) xrange yrange clusterblocks
changechainsv wnd xrange xranges yranges nchans ntemp clustertrees chains
let wndspikes = 
    plotClusterSpikeAmps xrange tranges 
        (clusterblocks|>Array.map (fun x -> x.StableClusters|> mapTree (fun (_,clu) _ -> clu.Items),x.XPos)) 
        yrange spikeamps cluchanged     
plotClusterSpikeShapes spikelen nchans ntemp yrange clustertrees shapes cluchanged

//let stragglers = getStragglers nchans numperblock shapes clustertrees chains

#if COMPILED
[<System.STAThread()>]
(new Application()).Run() |> ignore
#endif

let startsave savefile initsavefileoffset =
    let fsp = new FsPickler()
    save 
    |> Observable.scan (fun nb x ->
        printfn "%A" x
        use ms = new MemoryStream()
        fsp.Serialize(ms,(x|>Array.map (fun (a,b) -> a+firstblock,b)))
        let bytes = ms.ToArray()
        writeArray hostname savefile nb bytes |> Async.RunSynchronously
        nb + (Array.length bytes|>uint64)
    ) initsavefileoffset
    |> Observable.subscribe(fun _ -> ()) |> ignore

let showstragglers stragglers = 
    let (wndstrag,_),plotfun = plotshapes spikelen nchans yrange
    wndstrag.KeyDown |> Observable.filter (fun args -> args.Key = Key.Space)
    |> Observable.scan (fun xs _ ->
        match xs with
        |((i,(blocknum,clupath)),v)::tail ->
            let x = getNode clustertrees.[blocknum] clupath
            wndstrag.Title <- sprintf "Distance = %.3f" v
            plotfun 
                [|x.Items|>Array.map(fun i -> shapes.[i]),Colors.LightGray;[|x.Shape|],Colors.Black; [|shapes.[i]|],Colors.Blue|]
            tail
        | _ -> []
    ) (stragglers |> Array.toList)
    |> Observable.subscribe (fun _ -> ())
    |> ignore
    
