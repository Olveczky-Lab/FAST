#if COMPILED
module Gather_2_data
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

open RP.HDF5.H5TypeProviderRuntime
open ClusterHelper
open System.IO
open EphysSync
open RP.Controls
open System.Windows.Media
open PlotHelper
open TwoTapHelper
open FsCoreSerializer
open MathHelper
open ComputeAXLPower
open System

open RP.HDF5

let h5path = @"C:\EphysData\Sullivan\Ashesh\Bairagi"
let exptid = 2
let clocks,nictrtimes = 
    let fname = sprintf @"%s\clocks" h5path
    if File.Exists fname then
        use file = new FileStream(fname,FileMode.Open)
        FsCoreSerializer.Deserialize(file) :?> ((int64*int64[])[])*(BehaviorHelper.DIEventTime[])
    else
        let offset = 78
        let fnums = File.ReadAllLines(sprintf "%s\DataFileNums.txt" h5path) |> Array.map int64
        let c,n = getClocks exptid offset fnums |> fun (x,y) -> (x,y|>List.toArray)
        use file = new FileStream(fname,FileMode.Create)
        FsCoreSerializer.Serialize(file,(c,n))
        c, n

open BehaviorHelper
let aoStartTriggers = getEventTime exptid 0 1uy
let inline getaosamplenum rceid samplenum =
    let nictrtime = 
        ((samplenum|>float)*2.5|>int64) + 
        ((aoStartTriggers |> Seq.filter (fun evt -> evt.RCEID < rceid) |> Seq.last)|>fun x -> x.NICtrTime)
    getSampleNum clocks nictrtime |> Option.get |> snd

let gettapsamplenum = 
    getSampleNumFromRCEID clocks (nictrtimes|>Array.map (fun x -> x.EventTime))>>Option.get>>snd

let smid = 52
let parsed,_,tapdowns = ParseSM exptid smid

[<Literal>]
let fulltrajfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\trajfull.h5"
type fulltrajfile = HDF5File<fulltrajfname>
let fulltrajdata =
    [444986524L,fulltrajfile.Trial1.ReadData();444995287L,fulltrajfile.Trial2.ReadData()]|>Map.ofList

[<Literal>]
let trajfname = @"C:\EphysData\Sullivan\Ashesh\Bairagi\traj.h5"
type trajfile = HDF5File<trajfname>
let trajdata = 
    let rawdata = trajfile.TRAJ.ReadData()
    let numframes = Array2D.length1 rawdata
    let taptimes = trajfile.TapTimes.ReadData()
    Array.init (Array2D.length1 taptimes) (fun i ->
        let t = taptimes.[i,1]|>int64
        let samplenum = Map.find t tapdowns|>gettapsamplenum
        match Map.tryFind t fulltrajdata with
        | Some raw -> 
            samplenum-120L*300L,Array.init (Array2D.length2 raw) (fun j -> raw.[0,j],raw.[1,j])
        | None ->
            samplenum,Array.init numframes (fun j -> rawdata.[j,i*2],rawdata.[j,i*2+1]))



let f0,s0,s1 = 635025802044762119L,6090372245L,6198198272L
let startindx,numperclustering = 0UL,200
let fname = sprintf "%s\%d.h5" h5path f0
let chgroups = [|("1",1);("8",1);("13",0);("15",0);("22",1);("26",0)|]

let fs_spk = 30000.0
let spiketimes = 
    let numclus =
        File.ReadAllLines(sprintf @"%s\Ch26Sessions.txt" h5path)
        |> Array.choose (fun l ->
            match l.Split('\t') with
            | [|str_smid;f0;s0;s1;chgroup;o;_;n|] when (str_smid|>int=smid) -> 
                Some (chgroup,n|>int)
            | _ -> None)
        |> Map.ofArray
    chgroups 
    |> Array.map (fun (chgroup,_) ->
        let sorting = loadSorting (sprintf "%s.%d.%d.%s.sorting" fname exptid smid chgroup)
        let cnt = (numperclustering|>uint64)*(Map.find chgroup numclus|>uint64)
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
    )

let fs_spkrate = 7500.0
let spikerates =
    spiketimes |> Array.map (fun sts ->
        let frates = Array.zeroCreate (((s1-s0+1L)/4L)|>int)
        sts |> Array.iter (fun st -> 
            let indx = (st-s0)/4L|>int
            for i in indx-750..indx+750 do
                if (i >= 0) then frates.[i] <- frates.[i]+1.0)
        frates 
    )

let inline u x = uint64 x
let fs_axl = 7500.0
let axlData =    
    readData fname "./AXL" [|u s0/4UL;0UL|] null [|(u s1-u s0+1UL)/4UL;3UL|] :?> uint16[,]

let fs_axlpower = 75.0;
let axlPower = getaxlpower fname (u s0/4UL) ((u s1-u s0+1UL)/4UL)

let fs_lfp = 300.0
let lfpData =
    let lfp = readData fname "./LFP" [|u s0/100UL;0UL|] null [|(u s1-u s0+1UL)/100UL;64UL|] :?> int16[,]
    let getchnum (chgroup,chindx) =
        readData fname (sprintf "./ChGroup_%s/ChNums" chgroup) null null null :?> int[]
        |> fun x -> x.[chindx]
    let chnums = chgroups |> Array.map getchnum    
    Array2D.init (Array2D.length1 lfp) (Array.length chgroups)
        (fun i j -> lfp.[i,chnums.[j]])

let getspkrate chnum secoff numsec =
    Array.init (fs_spkrate*numsec|>int) (fun i -> spikerates.[chnum].[i+(secoff*fs_spkrate|>int)]|>float)
    |> Array.map (fun x -> x*x)

let getlfp chnum secoff numsec =
    Array.init (fs_lfp*numsec|>int) (fun i -> lfpData.[i+(secoff*fs_lfp|>int),chnum]|>float|>fun x -> x*3.74e-5)
    |> Array.map (fun x -> x*x)
    
let getaxl chnum secoff numsec =
    Array.init (fs_axl*numsec|>int) (fun i -> axlData.[i+(fs_axl*secoff|>int),chnum]|>float|>fun x -> x*3.74e-5)
    |>filtfilt axl_bandpass

let getaxlpower secoff numsec = 
    Array.init (fs_axlpower*numsec|>int) (fun i -> axlPower.[i+(fs_axlpower*secoff|>int)]|>float|>abs|>sqrt)

let getforepaw secoff numsec =
    let s t = ((t-s0)|>float)/fs_spk
    Array.zip [|0..Array.length trajdata-1|] trajdata    
    |> Array.choose (fun (i,(t,xs)) ->
        match (s t) with
        | x when x >= secoff && x <= (secoff+numsec) -> 
            Some (x,xs)
        | _ -> None)

open SVGWriter
open System.Xml.Linq

let r2 = 
    let wnd = new D3DPlot2DWindow()    
    wnd.Title <- "Correlations?"
    wnd.Show()
    let scalet t = (float (t-s0))/fs_spk
    let xrange = Range(s0|>scalet,s1|>scalet)
    addTimeXAxis wnd xrange |> ignore
    let yrangelever = Range(-13.0,1.5)
    addYAxis wnd yrangelever Colors.DeepPink "Lever Presses" |> ignore
    let getscaledtap = gettapsamplenum>>scalet

    let tapboxes =
        let gettapbox t1 t2 color =
            getLinexy color [t1;t1;t2;t2;t1] [0.0;1.0;1.0;0.0;0.0]
        parsed
        |> List.filter (fun trial -> List.length trial.Taps > 0)
        |> List.map (fun trial -> 
                        trial.Taps
                        |> List.map (fun (down,up) -> 
                            gettapbox (getscaledtap down) (getscaledtap up) 
                                (if Option.isSome trial.Reward then Colors.DeepPink else Colors.DeepSkyBlue)))
        |> List.concat
        |> fun x ->
            x|>Seq.cast|> addSubPlot wnd xrange yrangelever|> ignore
            x

    let rewardtones =
        parsed 
        |> List.filter (fun trial -> Option.isSome trial.Reward)
        |> List.map (fun trial ->
                let r = Option.get trial.Reward
                let ts = getaosamplenum r.ToneRCEID r.SampleNum|>scalet
                getLinexy Colors.Black [ts;ts] [0.;1.])
        |> Seq.cast
        |> addSubPlot wnd xrange yrangelever
        |> ignore
        

    let secoff = 20.
    let numsec = 15.

    let svgmap height offset miny maxy pnts =
        let xmap t = (t - secoff)/numsec*1000.0
        let ymap y = 
            if (maxy=miny) then offset
            else (1.0-(y-miny)/(maxy-miny))*height+offset
        pnts |> Seq.map (fun (x,y) -> xmap x,ymap y)

    let svgline height offset lines =
        let ys = lines |> Seq.map (fun (ps:PointSeries2D) -> ps.data |> Seq.map snd) |> Seq.concat
        let miny,maxy =  
            Seq.min ys, Seq.max ys
        lines 
        |> Seq.map (fun ps -> 
            svgmap height offset miny maxy ps.data
            |> getSVGPolyLine "fill:none;stroke:black;stroke-width:1"
        )
        |> fun x -> XElement(xn "g",x)

    let tapboxlines = 
        tapboxes
        |>Seq.filter (fun box -> 
            match box.data.[0]|>fst with
            | x when x >= secoff && x <= secoff+numsec -> true
            | _ -> false
        )
        |> svgline 50.0 400.0

    xrange.Set(secoff,numsec+secoff)

    let fs_forepaw = 100.0            
    let plotforepaw f yrange =
        addYAxis wnd yrange Colors.Black "Forepaw" |> ignore
        getforepaw secoff numsec
        |> Array.map (fun (t,xs) ->
            getLine Colors.Black (t-40.0/fs_forepaw) (1.0/fs_forepaw) (xs|>Array.map f))
        |> fun x ->
            x |> Seq.cast |> addSubPlot wnd xrange yrange |> ignore
            x

    let pawxline = plotforepaw fst (Range(0.0,480.0)) |> svgline 50.0 300.0
    let pawyline = plotforepaw (snd>>(fun x -> 640.0-x)) (Range(0.0,640.0)) |> svgline 50.0 350.0

    let plotaxl chnum yrangeaxl =
        addYAxis wnd yrangeaxl Colors.Red (sprintf "Axl %d" chnum) |> ignore
        addSubPlot wnd xrange yrangeaxl [getLine Colors.Red secoff (1.0/fs_axl) 
            (getaxl chnum secoff numsec)]
        |> ignore

    plotaxl 0 (Range(-1.0,1.0))
    plotaxl 1 (Range(-1.0,1.0))
    plotaxl 2 (Range(-1.0,1.0))
    
    let plotaxlPower yrangeaxl =
        addYAxis wnd yrangeaxl Colors.Red "Axl" |> ignore
        [getLine Colors.Red secoff (1.0/fs_axlpower) 
            (getaxlpower secoff numsec)]
        |> fun x -> 
            addSubPlot wnd xrange yrangeaxl (x|>Seq.cast) |> ignore
            x

    let axlline = plotaxlPower (Range(-1.0,1.0)) |> svgline 50.0 200.0
    
    let plotlfp chnum =
        let yrangelfp = Range(-1.0,1.0)
        addYAxis wnd yrangelfp Colors.ForestGreen (sprintf "LFP %d" chnum) |> ignore
        [getLine Colors.ForestGreen secoff (1.0/fs_lfp) 
            (getlfp chnum secoff numsec)]
        |> fun x ->
            addSubPlot wnd xrange yrangelfp (x|>Seq.cast) |> ignore
            x

    let lfpline = plotlfp 0 |> svgline 50.0 250.0
        
    let plotspikerates chnum =
        let yrangefrate = (Range(0.0,100.0))
        addYAxis wnd yrangefrate Colors.Black (sprintf "Firing Rate %d" chnum) |> ignore
        addSubPlot wnd xrange yrangefrate [getLine (brightColors.[chnum]) secoff (1.0/fs_spkrate)
            (getspkrate chnum secoff numsec)]
        |> ignore

    plotspikerates 0

    let spikelines =
        let yrangespikes = Range(-1.0,15.0)
        addYAxis wnd yrangespikes Colors.Blue "Spikes" |> ignore
        spiketimes
        |> Array.mapi (fun i sts -> 
                        let fi = i|>float
                        sts
                        |> Array.filter (fun t -> (t-s0) >= (secoff*fs_spk|>int64) && (t-s0) < ((numsec+secoff)*fs_spk|>int64))
                        |> Array.map (fun t -> 
                                        let st = scalet t
                                        getLinexy Colors.Blue [st;st] [0.1+fi;0.9+fi]))
        |> Array.concat
        |> fun x -> 
            addSubPlot wnd xrange yrangespikes (x|>Seq.cast) |> ignore
            x |> svgline 200.0 0.0
    //XElement(xn "svg",spikelines,axlline,lfpline,pawxline,pawyline,tapboxlines,svgline 0.0 500.0 [getLine Colors.Black (secoff+0.5) 2.0 [0.0;0.0]])
    ()

open MathNet.Numerics.Statistics

let r4 = 
    let dtbefore,dtafter,dt = 3.0,3.0,0.05
    let nbuckets = (dtbefore+dtafter)/dt|>int
    let goodtrials = 
        parsed 
        |> Seq.filter (function t -> t.Taps|>List.length > 0)
        |> Seq.pairwise 
        |> Seq.filter (function
                        | (t1,t2) when (Option.isSome t1.Reward) && (Option.isSome t2.Reward) && (t2.Taps|>List.length = 2) -> true
                        | _ -> false)
        |> Seq.map (fun (_,trial) ->
            let tapsamples = trial.Taps |> List.map (fun (down,up) -> gettapsamplenum down,gettapsamplenum up)
            tapsamples)
        |> Seq.toArray
    let ntrials = Array.length goodtrials
    let s t t0 = ((t-t0)|>float)/fs_spk
    let hists =
        spiketimes.[4..4]
        |> Array.map (fun sts ->
            goodtrials |> Array.map (fun tapsamples ->
                match tapsamples with
                (t1d,t1u)::(t2d,t2u)::_ ->
                    let t0s = [|t1d;t1u;t2d;t2u|] 
                    let x t0 = 
                        sts |> Array.choose (fun t ->
                                                match (s t t0) with
                                                | x when x >= -dtbefore && x <= dtafter -> Some x
                                                | _ -> None)
                    t0s |> Array.map x
                | _ -> failwith "Not Possible")
            |> fun x ->
                [|for i in 0..3 -> Histogram(Array.concat (x|>Array.map (fun a -> a.[i])),nbuckets,-dtbefore,dtafter)|]
        )

    hists 
    |> Seq.iteri (fun i hs -> 
        let scale = 1./dt/(ntrials|>float)
//        for j in 0..nbuckets-1 do
//            let x i = (hs.[i].[j].Count*scale)
//            printfn "%.2f\t%.2f\t%.2f\t%.2f" (x 0) (x 1) (x 2) (x 3)
        hs |> Seq.iteri (fun j h ->
            let wnd,yrange = plotbarhist scale h
            wnd.Height <- 600.
            wnd.Width <- 400.
            yrange.Set(0.,20.)
            wnd.Title <- sprintf "5 %d" j
        )
    )
    
//    let svgmap pnts =
//        let xmap t = (t + dtbefore)/(dtbefore+dtafter)*300.0
//        let ymap y = 150.0 - y/2.0
//        pnts |> Seq.map (fun (x,y) -> xmap x,ymap y)
//        |> getSVGPolyLine "fill:none;stroke:black;stroke-width:1"
//        |> fun x -> XElement(xn "g",x)
//
//    let svgline (h:Histogram) =
//        Array.init (h.BucketCount) (fun i -> (h.[i].LowerBound+h.[i].UpperBound)/2.0,h.[i].Count)
//        |> svgmap
//
//    let x = XElement(xn "svg",svgline hists.[1],svgline hists.[2],svgline hists.[4],svgline hists.[5], 
//                     svgmap [-3.0,-10.0;3.0,-10.0], svgmap [0.0,-10.0;0.0,-15.0],
//                     svgmap [3.5,0.0;3.5,280.0])
    ()

let r1 =
    let wnd = new D3DPlot2DWindow()
    wnd.Title <- "Correlations?"
    wnd.Show()
    let dtbefore,dtafter = 2.0,3.0
    let xrange = Range(-dtbefore,dtafter)
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    let yrange = Range (0.0,10.0)
    addYAxis wnd yrange Colors.Black "Axl 0" |> ignore
    parsed
    |> List.filter (fun trial -> List.length trial.Taps > 0)
//    |> List.sortBy (fun trial ->
//        let (_,_)::(a,b)::_ as taps = trial.Taps |> List.map (fun (down,up) -> gettapnictrtime down, gettapnictrtime up)
//        b-a)
    |> List.mapi (fun i trial -> 
                    let ((a,_)::_) as taps = trial.Taps |> List.map (fun (down,up) -> gettapsamplenum down, gettapsamplenum up)
                    let fi = i|>float
                    let s t = ((t-a)|>float)/fs_spk
                    let axlline =
                        let ds_axl = fs_spk/fs_axl|>int
                        let axlsampnum = (a-s0)/(ds_axl|>int64)|>int
                        Array.init (fs_axl*(dtbefore+dtafter)|>int) 
                            (fun i -> //axlPower.[axlsampnum-(fs_axl*dtbefore|>int)+i]|>fun x -> fi+(sqrt x)*5.0)
                                        axlData.[axlsampnum-(fs_axl*dtbefore|>int)+i,0]|>float|>fun x -> fi+(x/65536.0-0.5)*5.0)
                        |> getLine Colors.Black -dtbefore (1.0/fs_axl)
                    let axlline chnum =
                        let ds_axl = fs_spk/fs_axl|>int
                        let axlsampnum = (a-s0)/(ds_axl|>int64)|>int
                        Array.init (fs_axl*(dtbefore+dtafter)|>int) 
                            (fun i -> //axlPower.[axlsampnum-(fs_axl*dtbefore|>int)+i]|>fun x -> fi+(sqrt x)*5.0)
                                        axlData.[axlsampnum-(fs_axl*dtbefore|>int)+i,chnum]|>float|>fun x -> fi+(x/65536.0-0.5)*5.0)
                        |> getLine (brightColors.[chnum]) -dtbefore (1.0/fs_axl)
                    let gettapbox t1 t2 =
                        getLinexy Colors.DeepPink [s t1;s t1;s t2;s t2;s t1] [fi;0.9+fi;0.9+fi;fi;fi]
                    let spikerateline chnum = 
                        let ds_spkrate = fs_spk/fs_spkrate|>int
                        let spkrate_sampnum = (a-s0)/(ds_spkrate|>int64)|>int
                        Array.init (fs_spkrate*(dtbefore+dtafter)|>int) 
                            (fun i -> spikerates.[chnum].[spkrate_sampnum-(fs_spkrate*dtbefore|>int)+i]|>fun x -> fi+x/10.0)
                        |> getLine Colors.Blue -dtbefore (1.0/fs_spkrate)
                    let spikelines chnum color =
                        spiketimes.[chnum] |> Array.choose (fun t ->
                                                            match (s t) with
                                                            | x when x >= -dtbefore && x <= dtafter -> Some x
                                                            | _ -> None)
                                        |> Array.map (fun st -> getLinexy color [st;st] [0.1+fi;0.8+fi])
                                        |> Array.toList
                    axlline 0::(taps|>List.map (fun (d,u) -> gettapbox d u))@
                    spikelines 2 Colors.Blue@
                    if Option.isSome trial.Reward then 
                        [trial.Reward.Value.PulseTrain.Value.NumPulses|>float|>fun x -> x/5.0+fi
                            |>fun x -> getLinexy Colors.Brown [dtafter;dtafter] [fi;x]]
                    else []
                 )
    |> Seq.concat
    |> Seq.cast
    |> Seq.toList
    |> addSubPlot wnd xrange yrange
    |> ignore       

let plotcs = 
    let corrcoef offset length lag =    
        let x = getaxl 2 ((if lag >= 0.0 then 0.0 else -lag)+offset) length
        let y = getspkrate 4 ((if lag >= 0.0 then lag else 0.0)+offset) length   
        Correlation.Pearson(x,y)
    let isnan = System.Double.IsNaN

    let windowsize = 0.5
    let lags = [|-2.0..0.1..2.0|]
    let cs = 
        lags 
        |> Array.Parallel.map (fun lag -> 
            lag, 
            [0.0..windowsize..3400.0]
            |>List.map (fun offset -> 
                corrcoef offset windowsize lag)|>List.filter (isnan>>not)|>List.average)
    ()