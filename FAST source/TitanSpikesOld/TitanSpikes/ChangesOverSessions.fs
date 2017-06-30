#if COMPILED
module ChangesOverSessions
#else

#load "Helper.fs"
#load "HDF5Helper.fs"
#load "ClusterHelper.fs"
#load "BehaviorHelper.fs"
#load "PlotHelper.fs"
#load "EphysSync.fs"
#load "MathHelper.fs"
#load "ComputeAXLPower.fs"
#load "TwoTapHelper.fs"
#load "SvgWriter.fs"

#time
#endif

open System.IO
open FsCoreSerializer
open EphysSync
open TwoTapHelper
open ClusterHelper
open RP.HDF5.H5TypeProviderRuntime

let h5path = @"C:\EphysData\Sullivan\Ashesh\Bairagi"
let exptid = 2
let clocks,nictrtimes = 
    let fname = sprintf @"%s\clocks" h5path
    use file = new FileStream(fname,FileMode.Open)
    FsCoreSerializer.Deserialize(file) :?> ((int64*int64[])[])*(BehaviorHelper.DIEventTime[])

let gettapsamplenum = 
    getSampleNumFromRCEID clocks (nictrtimes|>Array.map (fun x -> x.EventTime))>>Option.get>>snd

let chgroup = "26"
let inline getspiketimes f0 smid startindx numperclustering numclusterwindows =
    let fname = sprintf @"%s\%d.h5" h5path f0 
    let sorting = loadSorting (sprintf "%s.%d.%d.%s.sorting" fname exptid smid chgroup)
    let cnt = (numperclustering|>uint64)*(numclusterwindows|>uint64)
    let clustered = readData fname (sprintf "./ChGroup_%s/ClusteredSpikes" chgroup) [|startindx|] null [|cnt|] :?> uint64[]
    let clusraw = readData fname (sprintf "./ChGroup_%s/Clusters" chgroup) [|startindx;0UL|] null [|cnt;20UL|] :?> uint16[,]                    
    let numclustered,numtemp = Array2D.length1 clusraw,Array2D.length2 clusraw
    let clus =
        Array.zip (clustered|>Array.map (fun spikenum -> spikenum-clustered.[0] |> int)) 
                    (Array.init numclustered (fun i -> Array.init numtemp (fun temp -> clusraw.[i,temp])))
    let startindx = clustered.[0]
    let endindx = clustered.[Array.length clustered-1]
    let st = readData fname (sprintf "./ChGroup_%s/SpikeTimes" chgroup) 
                    [|startindx|] null [|endindx-startindx+1UL|] :?> int64[]
    label numperclustering sorting clus (Array.length st)
    |> Array.zip st
    |> Array.filter (snd>>(=) GoodCluster)
    |> Array.map fst

type UnitSMInfo = {
    SMID: int
    FileNum: int64
    FirstSample: int64
    LastSample: int64
    ClusOffset: uint64
    NumWindows: int
}

let unitinfos =
    File.ReadAllLines(sprintf @"%s\Ch26Sessions.txt" h5path)
    |> Array.choose (fun l ->
        match l.Split('\t') with
        | [|smid;f0;s0;s1;chgroup;o;_;n|] when chgroup="26"-> 
            Some {SMID=smid|>int;FileNum=f0|>int64;FirstSample=s0|>int64;LastSample=s1|>int64;ClusOffset=o|>uint64;NumWindows=n|>int}
        | _ -> None)

let fs_spk = 30000.0
let spiketimes =
    unitinfos
    |> Array.map (fun info -> getspiketimes info.FileNum info.SMID info.ClusOffset 200 info.NumWindows)
let parsedsms = 
    unitinfos
    |> Array.map (fun info ->
        let parsed,_,_ = ParseSM exptid (info.SMID)
        parsed) 
let dtbefore,dtafter,dt = 2.0,2.0,0.05
open MathNet.Numerics.Statistics
let alignedpsths = 
    let nbuckets = (dtbefore+dtafter)/dt|>int
    Array.zip parsedsms spiketimes
    |> Array.map (fun (parsed,sts) ->
        parsed 
        |> Seq.filter (fun t -> t.Taps|>List.length > 0)
        |> Seq.pairwise 
        |> Seq.filter (function
                       | (t1,t2) when (Option.isSome t1.Reward) && (Option.isSome t2.Reward) && (t2.Taps|>List.length = 2) -> true
                       | _ -> false)
        |> Seq.map (fun (_,trial) ->
            let tapsamples = trial.Taps |> List.map (fun (down,up) -> gettapsamplenum down,gettapsamplenum up)
            match tapsamples with
            (u1,d1)::(u2,d2)::_ -> 
                let s t = ((t-d1)|>float)/fs_spk
                sts |> Array.choose (fun t ->
                                        match (s t) with
                                        | x when x >= -dtbefore && x <= dtafter -> Some x
                                        | _ -> None)
            | _ -> failwith "Not Possible")
        |> Seq.toArray
        |> fun x ->
            Array.length x, Array.concat x            
    )
    |> Array.map (fun (cnt,xs) ->
        cnt,
        Histogram(xs,nbuckets,-dtbefore,dtafter)
    )
//    |> fun x ->
//        Array.init ((Array.length x)/2) (fun i ->
//            (x.[2*i]|>fst)+(x.[2*i+1]|>fst),
//            Histogram(Array.append (x.[2*i]|>snd) (x.[2*i+1]|>snd),nbuckets,-dtbefore,dtafter))

open SVGWriter
open System.Xml.Linq
let x = 
    let svgmap pnts =
        let xmap t = (t + dtbefore)/(dtbefore+dtafter)*300.0
        let ymap y = 150.0 - y*2.0
        pnts |> Seq.map (fun (x,y) -> xmap x,ymap y)
        |> getSVGPolyLine "fill:none;stroke:black;stroke-width:1"
        |> fun x -> XElement(xn "g",x)

    let svgline scale (h:Histogram) =
        Array.init (h.BucketCount) (fun i -> (h.[i].LowerBound+h.[i].UpperBound)/2.0,h.[i].Count*scale)
        |> svgmap
    let getx (cnt,h) = svgline (70.0/(cnt|>float)) h
    XElement(xn "svg",getx alignedpsths.[1],getx alignedpsths.[2],getx alignedpsths.[4],
             svgmap [-1.0,-10.0;1.0,-10.0], svgmap [0.0,-10.0;0.0,-15.0], svgmap [1.05,0.0;1.05,105.0])

    

open PlotHelper
alignedpsths |> Array.iteri (fun i (cnt,h) -> 
    let wnd,yrange = plotbarhist (1.0/(cnt|>float)/0.05) h
    wnd.Title <- sprintf "%d" unitinfos.[i].SMID
    wnd.Height <- 640.
    wnd.Width <- 640.
    yrange.Set(0.,30.)
)
