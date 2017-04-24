#if COMPILED
module RisaBehaviorPlotter
#else
#load "RisaBehavior.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open RisaBehavior
open IntelIPP
open PlotHelper
open System.Windows.Media
open System.Windows.Media.Imaging
open RP.Controls
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open TwoTapHelper
open MathNet.Numerics.Statistics

let inline getipis (ts,m) = 
    let inline f x = Map.find x m
    ts |> Array.choose (fun t -> 
        match t.Taps with
        | (t1,_)::(t2,_)::_ -> Some (f t2 - f t1 |> float)
        | _ -> None
    )

let inline getpostunrewardedipis (ts,m) =
    let inline f x = Map.find x m
    ts |> Seq.pairwise |> Seq.choose (fun (t1,t2) ->
        if t1.Reward.IsSome then None else
        match t1.Taps,t2.Taps with
        | _::(x1,_)::(x2,_)::_,_
        | _::(x1,_)::[],(x2,_)::_ -> 
            Some (f x2 - f x1 |> float)
        | _ -> None           
    )
    |> Seq.toArray    

let inline getpressdurations (ts,m) =
    let inline f x = Map.find x m
    ts |> Array.map (fun t -> t.Taps |> List.toArray) |> Array.concat
    |> Array.map (fun (t1,t2) -> f t2 - f t1|>float)

let inline splitTrials (ts,m) =
    let inline f x = Map.find x m
    ts |> Seq.pairwise |> Seq.mapi (fun i t ->
        match t with
        | (t1,t2) when t1.Reward.IsSome || (f (t2.Taps|>List.head|>fst) - f (t1.Taps|>List.rev|>List.head|>snd)) > 10000L ->
            Some i
        | _ -> None
    )
    |> Seq.choose id
    |> fun x -> seq {yield -1;yield! x;yield Array.length ts - 1}
    |> Seq.pairwise |> Seq.map (fun (i1,i2) ->
        seq {
            for i in i1+1..i2 do
            for td,tu in ts.[i].Taps do
                yield td;yield tu
        }
        |> Seq.pairwise |> Seq.map (fun (t1,t2) ->
            f t2 - f t1
        )
        |> Seq.toArray
    )
    |> Seq.toArray

let trials,sms = loadAllData ()

let ratsintact = ratsintact |> List.toArray |> Array.map fst
let ratslesioned = ratslesioned |> List.toArray |> Array.map fst

let x = 
    let data =
        trials |> Map.map (fun rat sms ->
            let sms = if rat = "Cyprus" then sms.[8..] else sms
            let a = 
                sms |> Seq.scan (fun n (xs,_) -> 
                    n+(xs |> Array.filter (fun t -> t.Taps |> List.length >= 2) |> Array.length)
                ) 0 |> Seq.findIndex (fun n -> n > 1000)
            let ntaps = 
                sms.[0..a-1] |> Array.map (fun (ts,_) -> ts |> Array.sumBy (fun t -> t.Taps |> List.length))
                |> Array.filter (fun x -> x > 5)
                |> Array.map float |> Array.average
            let xs = 
                seq {
                    for (ts,m) in sms.[0..a-1] do
                        let inline f x = Map.find x m
                        for t in ts do
                            let rt = 
                                t.Reward |> Option.bind (fun x -> 
                                    x.PulseTrain |> Option.map (fun y -> f y.PokeRCEID - f x.ToneRCEID |> float)
                                )
                            match t.Taps with
                            | (t1,_)::(t2,_)::_ -> yield (f t2 - f t1 |> float), ((t.Reward.IsSome,(t.Taps |> List.length)),rt)
                            | _ -> ()                         
                } 
                |> Seq.take 1000
                |> Seq.toArray
            let mean = xs |> Array.map fst |> SpecialFunctions.Mean
            let std = xs |> Array.map fst |> SpecialFunctions.StdDev
            let cv = std/mean
            let n = xs |> Array.length |> float
            let fcrit = 
                (xs |> Array.map fst |> Array.filter (fun x -> x >= 700.*0.9 && x <= 700.*1.1) |> Array.length |> float) 
                / n
            let freward = 
                (xs |> Array.filter (snd>>snd>>Option.isSome) |> Array.length |> float )
                / n
            let treward = xs |> Array.choose (snd>>snd) |> Array.average
            let xsr,xsur = xs |> Array.partition (snd>>fst>>fst)
            let fntaps x = (x |> Array.map (snd>>fst>>snd>>float) |> Array.average) - 2.
            [|mean;cv;ntaps;freward;treward;fcrit;fntaps xsr;fntaps xsur|]
        )
    let plotdata = 
        Array.init 8 (fun i -> 
            let f rats = 
                let xs = rats |> Array.map (fun rat -> data.[rat].[i])
                let m = xs |> SpecialFunctions.Mean
                let s = xs |> SpecialFunctions.StdDev
                m,s
            f ratsintact,f ratslesioned
        )
    let wnd = new D3DPlot2DWindow()
    let xrange = Range(0.,1.)
    let yrange = Range(0.,1000.)
    wnd.Show()    
    let pnts x col rats = rats |> Array.map (fun rat -> (x,data.[rat].[2]),((xrange.Interval()/100.,yrange.Interval()/100.0)),col)
    Array.append (pnts 0.25 Colors.Blue ratsintact) (pnts 0.75 Colors.Red ratslesioned)
    |> getMarkerPoints
    |> fun l -> 
        addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
    addXAxis wnd xrange Colors.Black "" |> ignore
    addYAxis wnd yrange Colors.Black "IPI (ms)" |> ignore

let y =
    let wnd = new D3DPlot2DWindow()
    let xrange = Range(0.,1200.)
    let yrange = Range(0.,1.)
    wnd.Show()    
    let pnts = 
        seq {
            for (rat,(a,b)) in learning |> Map.toArray do
                let sms = if rat = "Cyprus" then trials.[rat].[8..] else trials.[rat]
                let ipis = sms |> Array.map getipis |> Array.concat
                let f xs = 
                    let m = xs |> SpecialFunctions.Mean
                    let std = xs |> SpecialFunctions.StdDev
                    m,std/m
                let (m1,s1) = f ipis.[0..999]
                let (m2,s2) = f ipis.[b-1000..b-1]
                printf "%A %A %A %A %A %A; " a b m1 s1 m2 s2
                yield (m1,s1),(xrange.Interval()/100.,yrange.Interval()/100.),Colors.Blue 
                yield (m2,s2),(xrange.Interval()/100.,yrange.Interval()/100.),Colors.Red 
        } |> Seq.toArray
    pnts
    |> getMarkerPoints
    |> fun l -> 
        addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
    //addSubPlot2 wnd xrange yrange xrange yrange [getLinexy2 Colors.Blue [(0.,0.);(25000.,25000.)]] |> ignore
    addXAxis wnd xrange Colors.Black "# Trials to Learn IPI" |> ignore
    addYAxis wnd yrange Colors.Black "# Trials to learn ITI" |> ignore

let get2tappoints rat =
    seq {
        for (x,m) in trials.[rat] do
            let inline f a = Map.find a m
            for t in x do
                match t.Taps with
                | (t1d,t1u)::(t2d,t2u)::_ ->
                    let a = ((f t2d - f t1u |> float),((f t1u - f t1d |> float)))
                    let nr = 
                        match t.Reward with
                        | Some x ->
                            match x.PulseTrain with
                            | Some y -> y.NumPulses
                            | _ -> 0
                        | _ -> 0
                    yield a
                | _ -> ()
    }
    |> Seq.filter (fun (a,b) -> a > 10.0 && b > 10.0)
    |> Seq.map (fun (a,b) -> log a,log b)
    |> Seq.toArray

let plottrialhmap rat =
    let xs = get2tappoints rat
    let n = Array.length xs    
    let wndsize = 3000
    let step = 1000
    let bmp i = 
        let x = imhist (4.f,4.f) ((log 20.0,log 1200.0),200) ((log 20.0,log 1200.0),200) xs.[i..i+wndsize-1]
        getheatmap (0.0f,5.0f) x
    let img im = 
        Image(
            Stretch=Stretch.Fill,
            RenderTransformOrigin=Point(0.5,0.5),
            RenderTransform=ScaleTransform(1.0,-1.0),
            Source=im)
    let grd = new Grid()
    for i in 0..4 do
        grd.ColumnDefinitions.Add(ColumnDefinition())
        grd.RowDefinitions.Add(RowDefinition())
    for i in 0..24 do
        if n >= i*step+wndsize then
            let im = bmp (i*step)
            let add x = 
                grd.Children.Add(x) |> ignore
                Grid.SetRow(x,i/5)
                Grid.SetColumn(x,i%5)
            add (img im)
    let wnd = 
        Window(
            Width=1000.,Height=1000.,
            Title=rat,
            Content=grd)
    wnd.Show()
ratsintact |> Array.iter (fun rat -> printfn "%s" rat;plottrialhmap rat)
ratslesioned |> Array.iter (fun rat -> printfn "%s" rat;plottrialhmap rat)

let plot2tapcoverage =
    let entropy x = 
        let s = x |> Array.sum
        let p = x |> Array.map (fun a -> a/s)
        p |> Array.sumBy (fun a -> if a = 0.f then 0.f else -a*(log a)/(log 2.f))
    let getcoverage rat =
        let xs = get2tappoints rat
        let n = Array.length xs    
        let wndsize = 3000
        let step = 1000
        let c = 
            seq {
                for i in 0..24 do
                    if n >= i*1000+wndsize then
                        let H = 
                            imhist (4.f,4.f) ((log 20.0,log 1200.0),200) ((log 20.0,log 1200.0),200) xs.[i*step..i*step+wndsize-1]
                            |> convert2Dto1D |> entropy
                        yield H

            } |> Seq.toArray
        c       
    let f i = log 20. + (i|>float)/200.*(log 1200. - log 20.)
    let maxentropy = 
        Array2D.init 200 200 (fun i j ->
            if exp(f i)+exp(f j) < 1200. then 1.0f else 0.0f 
        ) |> convert2Dto1D |> entropy
    let minentropy =
        imhist (4.f,4.f) ((log 20.0,log 1200.0),200) ((log 20.0,log 1200.0),200) [|log 500.,log 500.|]
        |> convert2Dto1D |> entropy
    let maxcoverage = 
        seq {
            for i in 0..200-1 do
                let x = f i
                for j in 0..200-1 do
                    let y = f j
                    if exp(x)+exp(y) < 1200. then yield 1 else yield 0 
        } |> Seq.sum |> float
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    wnd.Title <- ""
    let xrange = Range(0.0,25000.0)
    let yrange = Range(0.0,1.0)
    addXAxis wnd xrange Colors.Black "Trial" |> ignore
    addYAxis wnd yrange Colors.Black "Entropy (normalized)" |> ignore
    let f col rat = 
        getLine col 0.0 1000.0 (getcoverage rat|>Array.map (fun x -> (x-minentropy)/(maxentropy-minentropy)|>float))
    let a = ratsintact |> Array.Parallel.map (f Colors.Blue)
    let b = ratslesioned |> Array.Parallel.map (f Colors.Red)
    addSubPlot wnd xrange yrange          
        (Array.append a b |> Seq.cast)
    |> ignore   

let plotrewardscatter rat = 
    let cols = [|Colors.Black;Colors.Blue;Colors.Cyan;Colors.Green;Colors.Yellow;Colors.Red|]
    let xs = 
        seq {
            for (x,m),(_,(_,(l,u,_))) in Array.zip trials.[rat] sms.[rat] do
                let inline f a = Map.find a m
                for t in x do
                    match t.Taps with
                    | (t1d,t1u)::(t2d,t2u)::_ ->
                        let a = ((f t2d - f t1u |> float),((f t1u - f t1d |> float)))
                        let nr = 
                            match t.Reward with
                            | Some x ->
                                match x.PulseTrain with
                                | Some y -> y.NumPulses
                                | _ -> 0
                            | _ -> 0
                        yield (a, cols.[0]),(l,u)
                    | _ -> ()
        }
        |> Seq.skipWhile (fun (_,(l,_)) -> l < 400)
        |> Seq.toArray
    let n = Array.length xs    
    let wndsize,step = 3000,100
    let ys i =  xs.[i*step..i*step+wndsize-1]
    let xrange = Range(0.,1200.)
    let yrange = Range(0.,1200.)
    let inline f b = (b|>float32)/255.0f
    let wnd = new D3DPlot2DWindow()
    wnd.Show()    
    let initpos = 0
    let ys' = ys initpos
    wnd.Title <- sprintf "%s %d %d" rat (ys'.[0]|>snd|>fst) (ys'.[0]|>snd|>snd)
    let l =
        ys initpos |> Array.map (fun ((y,col),(_)) -> y,(xrange.Interval()/400.,yrange.Interval()/400.0),col)
        |> getMarkerPoints
        |> fun l -> 
            addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
            l.data
    wnd.KeyDown |> Observable.scan (fun i args ->
        min (i+1) ((n-wndsize)/step-1)
    ) initpos
    |> Observable.subscribe (fun i ->
        let ys' = ys i
        for j in 0..(Array.length l-1) do
            let ((x,y),col),_ = ys'.[j]
            l.[j].x <- x |> float32
            l.[j].y <- y |> float32
            l.[j].r <- f col.R
            l.[j].g <- f col.G
            l.[j].b <- f col.B            
        wnd.Title <- sprintf "%s %d %d" rat (ys'.[0]|>snd|>fst) (ys'.[0]|>snd|>snd)
        wnd.Plot.ForceRefresh()
    ) |> ignore
    addXAxis wnd xrange Colors.Black "IPI (ms)" |> ignore
    addYAxis wnd yrange Colors.Black "PD (ms)" |> ignore

ratsintact |> Array.iter plotrewardscatter
ratslesioned |> Array.iter plotrewardscatter

let plotrewardboundaries = 
    let data = 
        sms |> Map.map (fun _ sms ->
            sms |> Array.map (fun (_,(_,(l,u,_))) -> l|>float,u|>float)
        )
    
    let wnd = new D3DPlot2DWindow()
    wnd.Show()
    wnd.Title <- ""
    let xrange = Range(0.0,100.0)
    let yrange = Range(0.0,900.0)
    addXAxis wnd xrange Colors.Black "" |> ignore
    addYAxis wnd yrange Colors.Black "" |> ignore
    let f col = getLine col 0.0 1.0
    let a = ratsintact |> Array.map (fun rat -> data.[rat]|>Array.map (fun (a,b) -> a) |> f Colors.Blue)
    let b = ratslesioned |> Array.map (fun rat -> data.[rat]|>Array.map (fun (a,b) -> a) |> f Colors.Red)
    addSubPlot wnd xrange yrange          
        (Array.append a b |> Seq.cast)
    |> ignore   


    let xrange = Range(0.,1200.)
    let yrange = Range(0.,1200.)
    let rats = Array.append (ratsintact|>Array.map (fun x -> x,Colors.Blue)) (ratslesioned|>Array.map (fun x -> x,Colors.Red))
    let xs = rats |> Array.map (fun (rat,col) -> data.[rat],col)
    let wnd = new D3DPlot2DWindow()
    wnd.Show()    
    let inline f b = (b|>float32)/255.0f
    let l =
        xs |> Array.map (fun (ys,col) -> ys.[0],(xrange.Interval()/100.,yrange.Interval()/100.0),col)
        |> getMarkerPoints
        |> fun l -> 
            addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
            l.data
    wnd.KeyDown |> Observable.scan (fun i args ->
        i+1
    ) 0
    |> Observable.subscribe (fun i ->
        for ratnum in 0..Array.length l - 1 do
            let x,y = 
                let p = xs.[ratnum]|>fst
                p.[min i (Array.length p - 1)]
            l.[ratnum].x <- x |> float32
            l.[ratnum].y <- y |> float32
        wnd.Title <- sprintf "%d" i
        wnd.Plot.ForceRefresh()
    ) |> ignore
    let selected =
        addFreeSelRect wnd xrange yrange
        |> Observable.subscribe (fun r ->
            for ratnum in 0..Array.length l - 1 do
            if r.Contains(l.[ratnum].x|>float,l.[ratnum].y|>float) then printfn "%s" (rats.[ratnum]|>fst)
        )
    addXAxis wnd xrange Colors.Black "Lower Bound (ms)" |> ignore
    addYAxis wnd yrange Colors.Black "Upper Bound (ms)" |> ignore

let plotratscatter = 
    let data = 
        trials |> Map.map (fun _ sms ->
            let ts = 
//                sms |> Array.map getipis |> Array.concat
//                sms |> Array.map getpressdurations |> Array.concat |> Array.filter (fun x -> x > 10.)
                sms |> Array.map (fun (ts,m) ->
                    ts |> Array.choose (fun t ->
                        if t.Reward |> Option.isSome then None else
                        t.Taps |> List.length |> Some
                    )
                ) |> Array.concat
                |> Array.filter (fun x -> x > 1)
            let n = Array.length ts
            let wndsize = 1000
            let st = 100
            Array.init ((n-wndsize)/st) 
                (fun i -> 
                    let curts = ts.[i*st..i*st+wndsize-1] 
                    curts |> Array.map float |> Array.average,0.0)
//                    curts |> Array.filter (fun t -> t < 100.) |> Array.length |> float |>fun x -> x/(wndsize|>float),0.0)
//                (fun i -> ts.[i*st..i*st+wndsize-1] |> Array.sort)
//            |> Array.map (fun x ->
//                let nx = Array.length x
//                x.[nx/2],x.[nx*67/100]-x.[nx*33/100]
//            )
        )

//    let xrange = Range(0.,1200.)
//    let yrange = Range(0.,1200.)
    let xrange = Range(0.,1.)
    let yrange = Range(-1.,1.)
    let rats = Array.append (ratsintact|>Array.map (fun x -> x,Colors.Blue)) (ratslesioned|>Array.map (fun x -> x,Colors.Red))
    let xs = rats |> Array.map (fun (rat,col) -> data.[rat],col)
    let wnd = new D3DPlot2DWindow()
    wnd.Show()    
    let inline f b = (b|>float32)/255.0f
    let l =
        xs |> Array.map (fun (ys,col) -> ys.[0],(xrange.Interval()/100.,yrange.Interval()/100.0),col)
        |> getMarkerPoints
        |> fun l -> 
            addSubPlot2 wnd xrange yrange (Range(0.0,1.0)) (Range(0.0,1.0)) [l] |> ignore
            l.data
    wnd.KeyDown |> Observable.scan (fun i args ->
        i+1
    ) 0
    |> Observable.subscribe (fun i ->
        for ratnum in 0..Array.length l - 1 do
            let x,y = 
                let p = xs.[ratnum]|>fst
                p.[min i (Array.length p - 1)]
            l.[ratnum].x <- x |> float32
            l.[ratnum].y <- y |> float32
        wnd.Title <- sprintf "%d" i
        wnd.Plot.ForceRefresh()
    ) |> ignore
    let selected =
        addFreeSelRect wnd xrange yrange
        |> Observable.subscribe (fun r ->
            for ratnum in 0..Array.length l - 1 do
            if r.Contains(l.[ratnum].x|>float,l.[ratnum].y|>float) then printfn "%s" (rats.[ratnum]|>fst)
        )
    addXAxis wnd xrange Colors.Black "Median IPI (ms)" |> ignore
    addYAxis wnd yrange Colors.Black "IQR IPI (ms)" |> ignore

let plotheatmaps = 
    let data =
        trials |> Map.map (fun rat xs ->        
            let ((y0,y1),yn) as yp = (10.,1200.),1000
            let ipis = 
                xs |> Array.map getipis |> Seq.concat
//                |> Seq.filter (fun x -> x > 0.)
//                |> Seq.map log
                |> Seq.filter (fun ipi -> ipi > y0 && ipi < y1)
                |> Seq.toArray
                //|> fun x -> x.[0..19000-1]
            printfn "%s %d" rat (Array.length ipis)
            let (_,xn) as xp = (0.0,Array.length ipis-1|>float),1000
            let x = imhist (4.f,4.f) xp yp (ipis |> Array.mapi (fun i x -> i|>float,x))
            let x' = x |> convert2Dto1D
            getheatmap (x' |> Array.min, x' |> Array.max) x
        )
    Seq.append ratslesioned ratsintact
    |> Seq.iter (fun rat ->
        let wnd = dispimg data.[rat]
        wnd.Title <- rat
    )

