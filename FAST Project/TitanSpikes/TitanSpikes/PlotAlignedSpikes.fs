#if COMPILED
module PlotAlignedSpikes
#else
#load "DynProgAlign.fs"
#load "HighResTimingHelper.fs"
#load "SVGWriter.fs"
#time
fsi.ShowDeclarationValues <- false
#endif

open Helper
open BehaviorHelper
open TwoTapHelper
open Nessos.FsPickler
open System.IO
open RP.Controls
open HighResTimingHelper
open ClusterTreeHelper
open PlotHelper
open System.Windows.Media
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra.Double
open SVGWriter
open System.Windows.Media.Imaging
let rnd = System.Random()
let rand i n = 
    Array.init n (fun _ -> rnd.Next(i))
let randperm n =
    let xs = Array.init n id
    for i in n-1..-1..0 do
        let tmp = xs.[i]
        let j = rnd.Next(i)
        xs.[i] <- xs.[j]
        xs.[j] <- tmp
    xs
let cdf xs =
    let xs = Array.sort xs
    let n = Array.length xs |> float
    fun x ->
        if x < xs.[0] then 0.0 else 
        if x >= xs.[Array.length xs-1] then 1.0 else
        match System.Array.BinarySearch(xs,x) with
        | x when x >= 0 -> 
            let rec loop x =
                if x < Array.length xs-1 && xs.[x+1] = xs.[x] then loop (x+1) else (x+1|>float)/n
            loop x
        | x -> (~~~x|>float)/n
let modulation (xss:float[][]) =
    let all = xss |> Array.concat
    let nall = Array.length all
    let m = all |> Array.average
    let ntrials = Array.get xss 0 |> Array.length
    let ecdf =
        let nullxs = 
            Array.init 100000 (fun _ -> rand nall ntrials |> Array.averageBy (fun i -> all.[i]))
        cdf nullxs
    xss |> Array.map (fun xs ->
        let dm = m - (xs |> Array.average)
        let dma = abs dm
        dm,(1. - ecdf (m+dma)) + (ecdf (m-dma))
    )
let inline transpose xss =
    if Array.length xss = 0 then xss else
    Array.init (Array.length (Array.get xss 0)) (fun i -> xss |> Array.map (fun xs -> xs.[i]))
let classerror xs ys =
    let nxs, nys = Array.length xs, Array.length ys
    let zs = [|Array.map (fun x -> true,x) xs;Array.map (fun y -> false,y) ys|] |> Array.concat |> Array.sortBy snd
    zs |> Seq.pairwise|> Seq.fold (fun ((nx,ny),ne) ((zi1,z1),(zi2,z2)) ->
        let nx,ny = if zi1 then nx+1,ny else nx,ny+1
        let ne = 
            if z1 = z2 then ne else
            min ne (min (nx + nys-ny) (ny + nxs-nx))
        (nx,ny),ne
    ) ((0,0),min nxs nys) |> snd
let getAlignedTimes (dtbefore,dtafter) taligns spiketimes = 
    taligns
    |> Array.Parallel.map (fun talign ->
        spiketimes |> Array.filter (fun x -> x > talign+(dtbefore*30000.|>int64) && x < talign+(dtafter*30000.|>int64))
        |> Array.map (fun t -> (t-talign|>float)/30000.))
let plotaligned xranges xs =
    let wnd,(xrange,xranges),yrange,sps = 
        plotspikerasters xranges 0.25 0.1 10. xs
    let plots =
        sps |> Array.map (Array.map (getSVGSubPlot 600. (50.*(xs|>Array.length|>float)))) |> Array.concat
    for xrange in xranges do
        addSubPlot2 wnd xrange yrange xrange yrange 
            [[|(0.,yrange.Min),(0.,yrange.Max),Colors.Gray|]|>getLineList] |> ignore  
    wnd.Width <- 600.
    wnd.Height <- 600.
    addXAxis wnd xrange Colors.Black "Time (s)" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    plots
//let tranges = [|-0.5,0.5;-0.5,0.5;-0.5,0.5;-0.5,0.5;-1.0,1.0|]
let tranges = [|-3.0,5.0;-0.1,0.1;-0.1,0.1;-0.1,0.1;-0.1,0.1|]
let getspikecounts dt =
    Array.map2 (fun (t0,t1) xss ->
        xss |> Array.map (fun xs ->
            let h = Histogram(xs,(t1-t0)/dt|>int,t0,t1)
            Array.init (h.BucketCount) (fun i -> h.[i].Count)
        ) |> transpose
    ) tranges
let getprecision dt nbins counts =    
    counts |> Array.mapi2 (fun itrange (t0,t1) (counts:float[][]) ->
        let ntrials = Array.get counts 0 |> Array.length
        let totalcnts k =
            Array.init ntrials (fun i -> Array.init nbins (fun j -> counts.[k+j].[i]) |> Array.sum)
        let errs = 
            seq{
                for i in 0..((t1-t0)/dt|>int)-nbins-nbins do
                    let err = (classerror (totalcnts i) (totalcnts (i+nbins))|>float)/(2*ntrials|>float)
                    yield ((itrange,(i+nbins|>float)*dt+t0),err)
            } |> Seq.minBy snd
        errs
    ) tranges 
    |> Array.min
let getpeths spikecounts = 
    spikecounts |> Array.map (Array.map Array.average)
    |> Array.map (fun xs ->
        let a,b = Array.min xs, Array.max xs
        xs |> Array.map (fun (x:float) -> (x-a)/(b-a))
    )
let alignedtimes =    
    let gettimes hostname fpath =
        let sms = 
            use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConHRTwoTapSMTimes") 0UL -1)
            FsPickler().Deserialize<((int*int)*(int64*(int64*int64)))[]>(ms)
        let trials =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConTwoTapTrials") 0UL -1)
            FsPickler().Deserialize<((int * int) * (Trial [] * Map<int,int64>) option) []>(ms)
            |> Array.choose (fun (a,b) -> b |> Option.map (fun x -> a,x)) |> Map.ofArray
        let goodcells =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "GoodCellsSessionSpikeTimes") 0UL -1)
            FsPickler().Deserialize<(string*((int*int)*uint64[])[][]) []>(ms)
        let hrtimes =
            use ms = new MemoryStream(getArray<byte> hostname (fpath "OpConTwoTapHRTimes") 0UL -1)
            FsPickler().Deserialize<Map<int,Map<int,int64>>>(ms)
        let taligns = 
            sms |> Array.map (fun ((exptid,smid) as sm,_) ->
                let hrtimes = hrtimes.[exptid]
                trials.[sm] |> fst |> Array.choose (fun trial ->
                    trial.Reward |> Option.bind (fun r -> 
                        r.PulseTrain|>Option.map (fun p -> 
                            seq {
                                match trial.Taps with
                                | [t1d,t1u;t2d,t2u] ->
                                    yield! [t1d;t1u;t2d;t2u] 
                                |_ -> ()
                                yield p.PulseRCEID
                            } |> Seq.toArray |> Array.map (fun rceid -> hrtimes.[rceid])
                        )
                    )            
                ) |> Array.sortBy (fun xs -> xs.[2] - xs.[0]) 
                |> fun xss -> Array.length xss,transpose xss
            ) |> Array.zip (sms |> Array.map fst) |> Map.ofArray
        taligns,
        goodcells |> Array.map (fun (chgroup,cells) ->
            cells |> Array.mapi (fun i xs ->
                xs |> Array.choose (fun ((exptid,smid) as sm,smsts) ->
                    let ntrials,taligns = taligns.[sm]
                    if (ntrials <= 10) then None else                    
                        let aligned = 
                            Array.zip tranges taligns |> Array.map (fun (trange,taligns) -> 
                                getAlignedTimes trange taligns (smsts|>Array.map int64)) 
                        let nspikes = Array.init ntrials (fun i -> aligned |> Array.sumBy (fun x -> Array.length x.[i]))
                        if nspikes |> Array.filter ((=)0) 
                            |> Array.length > 5 then None else Some ((chgroup,i,exptid,smid),aligned)
                )
            ) |> Array.filter (Array.length>>(fun x -> x > 0))
        ) |> Array.filter (Array.length>>(fun x -> x > 0))

    [|Remote "140.247.178.16:5900", sprintf @"/root/data/rpoddar/badlands/%s"
      Remote "140.247.178.16:5900", sprintf @"/root/data/rpoddar/arches/%s"
      Remote "140.247.178.94:5900", sprintf @"/root/data/asheshdhawale/Data/Gorakh/%s"
      Remote "140.247.178.94:5900", sprintf @"/root/data/asheshdhawale/Data/Gunakari/%s"|]
    |> Array.map (fun (hostname,fpath) -> gettimes hostname fpath)
let colstrings = 
    [|"#FF9900";"#FF0000";"#0099CC";"#0000FF"|]
let cols =
    colstrings
    |> Array.map (fun str -> ColorConverter.ConvertFromString(str) :?> System.Windows.Media.Color)
let mtrials = 25
let goodtrials = 
    alignedtimes |> Array.map (fun (taligns,cells) ->
        taligns |> Map.map (fun _ (ntrials,xs) ->
            if ntrials < mtrials then None else
            let ipis = Array.init ntrials (fun i -> xs.[2].[i] - xs.[0].[i])
            [|0..ntrials-mtrials|] |> Array.map (fun i-> i,ipis.[i+mtrials-1]-ipis.[i])
            |> Array.minBy snd |> fst |> Some
        )
    )        

let pca =
    let cellratids,cells = 
        alignedtimes |> Array.map (snd>>Array.concat>>Array.concat>>Array.map snd)
        |> Array.mapi (fun i xs -> xs |> Array.map (fun x -> i,x))
        |> Array.concat |> fun x -> Array.map fst x, Array.map snd x
    let cellsms =
        alignedtimes |> Array.map (snd>>Array.concat>>Array.concat>>
            Array.map (fun ((_,_,exptid,smid),_) -> exptid,smid))
        |> Array.mapi (fun i xs -> xs |> Array.map (fun x -> i,x))
        |> Array.concat
    let stablecellids =
        alignedtimes |> Array.map (snd>>Array.concat)
        |> Array.concat |> Array.mapi (fun i x -> x |> Array.map (fun _ -> i))
        |> Array.concat |> Array.mapi (fun i x -> i,x)
        |> Seq.groupBy snd |> Seq.choose (fun (_,x) -> 
            let x = x |> Seq.toArray
            if Array.length x > 1 then Some (x|>Array.map fst|>Array.sort) else None
        ) |> Seq.toArray
    let spikecounts =
        let dt = 0.1
        cells |> Array.map (Array.map2 (fun (t0,t1) xss ->
            xss |> Array.map (fun xs ->
                let h = Histogram(xs,(t1-t0)/dt|>int,t0,t1)
                Array.init (h.BucketCount) (fun i -> h.[i].Count)
            ) |> transpose
        ) tranges>>Array.concat)

    let peths = getpeths spikecounts
    let dists = 
        let n = Array.length peths
        seq {
            for i in 0..n-1 do
                for j in i+1..n-1 do
                    yield (i,j),Array.map2 (fun x y -> (x-y)*(x-y)) peths.[i] peths.[j] |> Array.sum
        } |> Map.ofSeq
    let stabledists =
        let ecdf = cdf (dists|>Map.toArray|>Array.map snd)
        stablecellids |> Array.map (fun xs ->
            let n = Array.length xs
            xs,
            seq {
                for i in 0..n-1 do
                    for j in i+1..n-1 do
                        yield dists.[xs.[i],xs.[j]]
            } |> Seq.max |> ecdf
        ) |> Array.sortBy snd
        
    let mpeth = peths |> transpose |> Array.map Array.average
    let ncells,nbins = Array.length peths,Array.length mpeth
    let peths' = peths |> Array.map (Array.map2 (fun m x -> x-m) mpeth)
    let sc = 1./sqrt (ncells-1|>float)
    let data = (DenseMatrix.ofRows ncells nbins peths').Transpose()
    let x = sc*(data.Transpose())
    let svd = x.Svd(true)
    let u,s,v' = svd.U(), svd.S(), svd.VT()
    let signals = v'*data
    
    let xi,yi = 0,1 
    let wnd = new D3DPlot2DWindow()
    let xs,ys = signals.Row(xi) |> Vector.toArray,signals.Row(yi) |> Vector.toArray
    let pnts = Array.zip xs ys
    let pntsi = pnts |> Array.mapi (fun i x -> i,x)
    let expand (a,b) = 
        let c = (b-a)*0.1
        Range(a-c,b+c) 
    let xrange = (Array.min xs,Array.max xs) |> expand
    let yrange = (Array.min ys,Array.max ys) |> expand
    wnd.Show()    
    let scatterplot cellcols = 
        Array.zip cellcols pnts 
        |> Array.map (fun (col,a) -> a,(xrange.Interval()/100.,yrange.Interval()/100.),col)
        |> getMarkerPoints
        |> fun l -> 
            addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
            Array.zip cellcols pnts |> Seq.groupBy fst |> Seq.map (fun (col,xs) ->
                getSVGPointList 300. 300. xrange yrange 2. col (xs |> Seq.map snd)
            )
    //let plot = scatterplot (cellratids |> Array.map (fun i -> cols.[i]))
    let groupnums = [|21;31;33;45;46;50;51|]
    let plot =
        let cols = Array.init (Array.length pnts) (fun _ -> Colors.LightGray)
        groupnums |> Array.iteri (fun i groupnum ->
            stabledists.[groupnum] |> fst |> Array.iter (fun j ->
                cols.[j] <- brightColors.[i]
            )
        )
        scatterplot cols
    (getSVG 300 300 plot).Save(@"Z:\FiguresForDAC\StableCells.svg")
    let savepethplots =
        stabledists |> Array.sortBy (fst>>Array.length) |> Array.rev 
        |> fun x -> x.[0..19] |> Array.iteri (fun gnum (cellnums,_) -> 
            let xs = 
                cellnums
                |> Array.map (fun cellnum ->
                    cells.[cellnum] |> Array.choose (fun x -> 
                        let ratid,sm = cellsms.[cellnum]
                        goodtrials.[ratid].[sm]|>Option.map (fun trialnum ->
                            x.[trialnum..trialnum+mtrials-1]
                        )
                    )
                ) |> Array.filter (fun x -> Array.length x > 1)
                |> Array.mapi (fun i x -> x,brightColors.[i%Array.length brightColors]) |> plotaligned tranges
            (getSVG 600 (50*(Array.length xs)) xs).Save(sprintf @"Z:\FiguresForDAC\StablePETHs%d.svg" gnum)
        )
    addXAxis wnd xrange Colors.Black (sprintf "PC %d" (xi+1)) |> ignore
    addYAxis wnd yrange Colors.Black (sprintf "PC %d" (yi+1)) |> ignore
    addFreeSelRect wnd xrange yrange |> Observable.subscribe (fun r ->
        pntsi |> Array.choose (fun (i,(x,y)) -> 
            if x >= r.Left && x <= r.Right && y <= r.Bottom && y >= r.Top then 
                Some i
            else None
        ) 
        |> fun x -> 
            printfn "%A" x
            x
        |> Array.mapi (fun i cell -> cells.[cell],brightColors.[i%Array.length brightColors]) 
        |> plotaligned tranges |> ignore
    )

let uniquecells =
    let cellratids,cells = 
        alignedtimes |> Array.map (snd>>Array.concat>>Array.map(fun x -> x.[0])>>Array.map snd)
        |> Array.mapi (fun i xs -> xs |> Array.map (fun x -> i,x))
        |> Array.concat |> fun x -> Array.map fst x, Array.map snd x
    let cellsms =
        alignedtimes |> Array.map (snd>>Array.concat>>
            Array.map(fun x -> x.[0])>>Array.map (fun ((_,_,exptid,smid),_) -> exptid,smid))
        |> Array.mapi (fun i xs -> xs |> Array.map (fun x -> i,x))
        |> Array.concat
    let precision =
        let dt = 0.001
        Array.zip goodtrials alignedtimes |> Array.map (fun (goodtrials,(_,cells)) ->
            cells |> Array.concat |> Array.map (fun x -> 
                let (_,_,exptid,smid),sts = x.[0]
                goodtrials.[exptid,smid] |> Option.map (fun trialnum ->
                    let counts = sts |> Array.map (fun x -> x.[trialnum..trialnum+mtrials-1]) |> getspikecounts dt
                    let rec loop nbins =
                        match getprecision dt nbins counts |> snd with
                        | x when x > 0.2 -> nbins
                        | _ -> loop (nbins/2)
                    loop 256
                )
            )
        ) |> Array.concat

    let np = 
        Array.zip cellratids precision |> Array.choose (fun (ratid,p) ->
            p |> Option.map (fun x -> ratid = 0 || ratid = 1,x)
        ) |> Seq.countBy id |> Seq.toArray |> Array.sort

    let savepethplots =
        let cellnums =
            precision |> Array.mapi (fun i x -> x |> Option.map (fun x -> i,x)) |> Array.choose id
            |> Array.sortBy snd
        let nperplot = 10
        let ncells = Array.length cellnums
        [|0..nperplot..ncells-1|] |> Array.iter (fun io -> 
            printfn "%d" io
            let xs = 
                cellnums.[io..(min (io+10) ncells)-1]
                |> Array.mapi (fun i (cellnum,_) ->
                    cells.[cellnum] |> Array.map (fun x -> 
                        let ratid,sm = cellsms.[cellnum]
                        let trialnum = goodtrials.[ratid].[sm]|>Option.get
                        x.[trialnum..trialnum+mtrials-1]),brightColors.[i%Array.length brightColors]
                ) |> plotaligned tranges
            (getSVG 600 (50*(Array.length xs)) xs).Save(sprintf @"Z:\FiguresForDAC\PETHs%d.svg" io)
        )
            
    let dt = 0.1
    let spikecounts = 
        cells |> Array.map (Array.map2 (fun (t0,t1) xss ->
            xss |> Array.map (fun xs ->
                let h = Histogram(xs,(t1-t0)/dt|>int,t0,t1)
                Array.init (h.BucketCount) (fun i -> h.[i].Count)
            ) |> transpose
        ) tranges>>Array.concat)
    let mods = spikecounts |> Array.map modulation
    let plotmod = 
        let modfrac = 
            let nbins = Array.length spikecounts.[0]
            Array.init nbins (fun i -> 
                Array.zip cellratids mods |> Array.map (fun (rat,xs) -> 
                    let dm,p = xs.[i]
                    rat = 0 || rat = 1, if p < 1e-5 then sign dm else 0
                ) |> Seq.countBy id |> Map.ofSeq
            )
        let lines =
            let x = 
                [|(true,1),209;(true,-1),209;(false,1),115;(false,-1),115|] |> Array.map (fun (x,n) ->
                    modfrac |> Array.map (fun m ->
                        match Map.tryFind x m with
                        | Some x -> (x|>float)/(n|>float)
                        | None -> 0.
                    )
                )
            let cols = 
                [|"#669900";"#CC3399";"#00CC66";"#9933FF"|]
                |> Array.map (fun str -> ColorConverter.ConvertFromString(str) :?> System.Windows.Media.Color)
            let nbins = tranges |> Array.map (fun (t0,t1) -> (t1-t0)/dt|>int)
            let offs = Array.scan (fun acc x -> x+acc) 0 nbins
            let pos = Array.scan (fun acc x -> x+acc+3) 0 nbins
            let xrange = Range(0.0,pos.[Array.length pos-1]|>float)
            let yrange = Range(0.0,0.5)
            Seq.zip3 nbins offs pos |> Seq.map (fun (n,o,p) ->
                Array.zip cols x |> Array.map (fun (col,x) -> seq{
                    let pnts = (x.[o..o+n-1] |> Array.mapi (fun i x -> (i+p|>float),x))
                    yield getSVGPointList 600. 300. xrange yrange 3. col pnts
                    yield getSVGPolyLine (sprintf "fill:none;stroke-width:2;stroke:%s" (col2string col)) 600. 300. xrange yrange pnts
                }) |> Seq.concat
            ) |> Seq.concat
        ()

    let peths = getpeths spikecounts
    
    let saveformatlab () =
        writeArray Local @"Z:\FiguresForDAC\uniquecellspeth" 0UL 
            (Array2D.init (Array.length peths) (Array.length peths.[0]) (fun i j -> peths.[i].[j])) 
        |> Async.RunSynchronously

    let order =
        getArray<int> Local @"Z:\FiguresForDAC\uniquecellsorder" 0UL -1
        |> Array.map (fun x -> x - 1)
    
    let saveimgs =
        let x = Array2D.init (Array.length peths) (Array.length peths.[0]) (fun i j -> peths.[order.[i]].[j] |> float32)
        //Do this for all 5 tranges
        let hm = getheatmap (0.0f,1.0f) (x.[0..,20..29])
        saveimage hm @"Z:\FiguresForDAC\pethheatmap3.tif"

    let saveratids =
        let palette = BitmapPalette(cols)
        let x = order |> Array.map (fun i -> cellratids.[i] |> byte)
        let im = BitmapSource.Create(1, Array.length peths, 96., 96., PixelFormats.Indexed8, palette, x, 1)
        saveimage im @"Z:\FiguresForDAC\pethratids.tif"

    let rasters =
        [|0..Array.length cells-1|] 
        |> Array.mapi (fun i cell -> 
            let ratnum,sm = cellsms.[cell]
            let trialnum = goodtrials.[ratnum].[sm].Value
            cells.[cell] |> Array.map (fun xss -> xss.[trialnum..trialnum+mtrials-1]),
            brightColors.[i%Array.length brightColors])
        |> plotaligned tranges
    ()
