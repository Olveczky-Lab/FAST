module ClusterViewerHelper

open PlotHelper
open RP.Controls
open System.Windows
open System.Windows.Media
open System.Windows.Input
open ClusterTreeHelper
open ClusterHelper
open Cluster
open Subject

let plotXChLines wnd xrange yranges initstate xs =
    xs
    |> Seq.groupBy (fun ((x1,(y1,ch1),x2,(y2,ch2)),col) -> (ch1,ch2))
    |> Seq.map (fun ((ch1,ch2),xs) -> 
        let l = xs |> Seq.map (fun ((x1,(y1,ch1),x2,(y2,ch2)),col) -> (x1,y1),(x2,y2),col) |> Seq.toArray |> getLineList 
        let sp = addSubPlot2 wnd xrange (Array.get yranges ch1) xrange yranges.[ch2] ([l])
        sp.Order <- -1.0
        sp.DisplayState.PlotVisible <-initstate
        sp
    )
    |> Seq.toArray

let togglespvisibility (wnd:D3DPlot2DWindow) (sps:SubPlot2[]) v =
    for sp in sps do
        sp.DisplayState.PlotVisible <- v
    wnd.Plot.ForceRefresh()


type ClusterState =
    | Selected
    | Visible
    | Hidden
    | Deleted

type ClusterBlock = 
    {
        StableClusters:Tree<ClusterState*StableCluster>
        XPos:float*float
    }

let plotselectedshapes spikelen nchans ntemp yrange =
    let (wnd,_),plotfun = plotshapes spikelen nchans yrange
    wnd.Left <- 2048.//-960.
    //wnd.Width <- 960.
    //wnd.Height <- 1040.
    fun ((clu:StableCluster,cluspikes),clus) -> 
        plotfun
            [|cluspikes,Colors.LightGray;[|clu.Shape|],Colors.Black;clus|>Array.map (fun clu -> clu.Shape),Colors.Blue;|]
        wnd.Title <- sprintf "Count=%d Temp=%d Stability=%.3f Var = %.2f" (clu.Items|>Array.length) clu.Temp (ClusterValue ntemp clu) clu.Var

type EventType<'A> =
| NewCluster of 'A
| LeftClick
| Save
| Reset

type SelectEventType =
    | Changed 
    | Added
    | Removed
    | ResetAll

let getClusPos nchans ntemp clu = 
    let ch,amp = 
        let ch,amp = Array.init nchans (fun i -> i,clu.Shape.[31*nchans+i]) |> Array.maxBy (snd>>abs)
        ch,amp
    let x = (clu.Temp+1|>float)/(ntemp|>float)
    (x,amp,ch)

let plotStableClusters (spikelen,nchans,ntemp) xrange yrange clusterblocks =
    let nblocks = Array.length clusterblocks
    let wnd = new D3DPlot2DWindow(Title = "TitanSpikes",Left = 0.0,Width=2048.,Top=0.0,Height=1112.)
    wnd.Show()

    addYAxis wnd yrange Colors.Black "" |> ignore
    let yranges = 
        let (ymin,ymax) = yrange.Min,yrange.Max
        aggRange wnd yrange (Array.init nchans (fun chnum -> (ymin,ymax-ymin),(ymin,ymax)))

    let inline tglobal ch = transform yrange (yranges.[ch])

    let toggle initstate f key =
        wnd.KeyDown |> Observable.filter (fun args -> args.Key = key) 
        |> Observable.scan (fun state _ -> not state) initstate |> Observable.subscribe f

    let colors = 
        [|"#FF0000";"#37B6CE";"#FFBE00";"#00CC00"|]
        |> Array.map (fun str -> ColorConverter.ConvertFromString(str) :?> System.Windows.Media.Color)
        |> fun xs ch -> xs.[ch]

    addXAxis wnd xrange Colors.Black "Block" |> ignore
    let xranges = clusterblocks |> Array.map (fun x -> (x.XPos,(0.0,1.0))) |> aggRange wnd xrange
    let getBlockMarkers xrangelocal clustertree = 
        let _,pathlist,clusters = 
            let rec loop (Node(_,(vis,_) as t,sts)) =                
                Node(t,sts|>Array.filter (fun (Node((_,(vis,_)),_)) -> vis<>Hidden && vis<>Deleted)|> Array.map loop)
            loop (clustertree|>NumberNodes)
            |> NumberPaths
        let paths = 
            pathlist |> Seq.map (Seq.pairwise) |> Seq.concat |> Seq.distinct |> Seq.toArray

        let links i = paths |> Array.filter (fst>>(=)i) |> Array.map snd

        let clus = clusters |> Array.map (snd>>snd>>getClusPos nchans ntemp)
        let getPathPoints i =
            let t = transform xrangelocal xrange
            let x1,y1,ch1 = Array.get clus i
            links i |> Array.map (fun j ->
                let x2,y2,ch2 = clus.[j]
                let tx1,tx2 = t x1, t x2
                let midx = (tx1+tx2)/2.0
                [|(tx1,(y1,ch1),midx,(y1,ch1));(midx,(y1,ch1),midx,(y2,ch2));(midx,(y2,ch2),tx2,(y2,ch2))|]
            ) |> Array.concat

        Array.mapi (fun i (x,amp,ch) -> (x,amp,ch,(clusters.[i],getPathPoints i))) clus

    let addClusterMarkerSubplots (sx,sy) (r,curclus) = 
        [|0..nchans-1|]
        |> Array.map (fun ch ->
            curclus
            |>Array.choose (fun (x,amp,curch,((_,(vis,clu)),_)) ->
                if curch = ch then
                    let dx = (clu.Items|>Array.length|>float)/4000./(ntemp|>float)
                    let dy = (clu.Stability|>float)/(clu.Items|>Array.length|>float)
                    let col = if vis = Selected then Colors.Black else colors ch
                    ((x |> transform r xrange,amp),(2.*dx,2.*dy),col) |> Some
                else None
            )
            |> getMarkerPoints
            |> fun l -> 
                l.MarkerScaleX <- sx
                l.MarkerScaleY <- sy
                addSubPlot2 wnd xrange (yranges.[ch]) xrange (yranges.[ch]) [l],l
        )
        |> fun xs ->
            let changeMarkerScale =
                fun (x,y) ->
                    for _,l in xs do
                        l.MarkerScaleX <- l.MarkerScaleX*x
                        l.MarkerScaleY <- l.MarkerScaleY*y
            let sub = 
                wnd.KeyDown |> Observable.subscribe (fun args ->
                    let f x = 
                        changeMarkerScale x
                        wnd.Plot.ForceRefresh()
                    let inc,dec = 1.25f,0.75f
                    match args.Key with
                    | Key.W -> f (1.0f,inc)
                    | Key.S -> f (1.0f,dec)
                    | Key.A -> f (dec,1.0f)
                    | Key.D -> f (inc,1.0f)
                    | _ -> ()
                )
            xs |> Array.map fst,
            fun () -> 
                for sp,_ in xs do
                    wnd.Plot.RemoveSubPlot(sp)
                sub.Dispose()
                xs |> Seq.head |> snd |> fun x -> x.MarkerScaleX,x.MarkerScaleY

    let addpathSubPlots vis curclus =
        let col = Color.FromRgb(220uy,220uy,220uy)
        curclus |> Array.map (fun (_,_,_,(_,f)) -> f) |> Array.concat
        |> Array.map (fun a -> a,col)
        |> plotXChLines wnd xrange yranges vis
        |> fun x -> 
            let sub = toggle vis (fun v -> 
                togglespvisibility wnd x v) Key.Z
            x,
            fun () ->
                for sp in x do
                    wnd.Plot.RemoveSubPlot(sp)
                sub.Dispose()
                if Array.length x = 0 then vis else
                (x |> Seq.head).DisplayState.PlotVisible

    let clustertrees = clusterblocks |> Array.map (fun x -> x.StableClusters)
    let blockMarkers = 
        Array.zip clustertrees xranges |> Array.map (fun (clustertree,r) -> 
            (r,getBlockMarkers r (clustertree)))    

    let inline tlocal ch = transform (Array.get yranges ch) yrange
    let getnearest (x,y) =
        let blocknum = clusterblocks|>Array.mapi (fun i x -> (i,x.XPos))  |> Array.minBy (fun (c,(a,b)) -> a+b/2. - x |> abs) |> fst
        let r,curblock = blockMarkers.[blocknum]
        curblock|>Array.minBy (fun (a,b,ch,clu) -> 
            let curyrange = yranges.[ch]
            let tb = tlocal ch b
            let ta = transform r xrange a
            let dx = (x-ta)/xrange.Interval()
            let dy = (y-tb)/yrange.Interval()
            dx*dx+dy*dy)
        |> fun (x,y,ch,((clupath,_),_)) -> 
            let curyrange = yranges.[ch]
            (x|>transform r xrange,y|>tlocal ch),(blocknum,clupath)
    let cluchanged =
        let mousemove,snap,changexhairv = addCrosshair wnd.Plot xrange yrange (Brushes.LightGray)
        toggle true changexhairv Key.X |> ignore
        mousemove |> Observable.map getnearest
        |> Observable.pairwise
        |> Observable.filter (fun ((_,a),(_,b)) -> a<>b)
        |> Observable.map snd
        |> fun x ->
            x |> Observable.subscribe (fun (newpos,a) -> snap newpos) |> ignore
            x |> Observable.map snd

    let addsubplots (scale,vis) (r,curclus) =
        let sp1,f1 = addClusterMarkerSubplots scale (r,curclus)
        let sp2,f2 = addpathSubPlots vis curclus
        (sp1,sp2),
        fun () -> f1 (),f2 ()
    
    let isCtrlPressed () = Keyboard.IsKeyDown(Key.LeftCtrl) || Keyboard.IsKeyDown(Key.RightCtrl)
    let rec isChildSelected (Node(_,sts) as node) =
        let rec isChildSelectedR (Node(_,sts) as node) i = 
            if i < 0 then false else
            let (Node((vis,_),ssts)) = sts.[i]
            match vis with
            | Selected | Deleted -> true
            | Visible when isChildSelected sts.[i] -> true
            | _ -> isChildSelectedR node (i-1)
        isChildSelectedR node (Array.length sts - 1) 
    let sps,x = 
        let xs = blockMarkers |> Array.mapi (fun i x -> i, addsubplots ((3.f,0.5f),true) x)
        xs |> Array.map (snd>>fst),
        (None, xs |> Array.map (fun (i,(_,x)) -> i,x)|> Map.ofArray,Set.empty,None,None)
    let seloper,saveoper = 
        let obs = 
            cluchanged |> Observable.map (NewCluster)
            |> Observable.merge (wnd.KeyDown |> Observable.choose (fun args -> 
                match isCtrlPressed (), args.Key with
                | true, Key.S -> Some Save
                | true, Key.R -> Some Reset
                | _ -> None
            ))
            |> Observable.merge (wnd.Plot.cnvMain.MouseLeftButtonDown |> Observable.map (fun _ -> LeftClick))
            |> Observable.scan (fun ((state,spremover,sel,_,_) as oldstate) evt ->
                let update spremover (blocknum,clupath) newnode =
                    let r = xranges.[blocknum]
                    let dispstate = (spremover |> Map.find blocknum) ()
                    clustertrees.[blocknum] <- setTree clustertrees.[blocknum] clupath newnode
                    blockMarkers.[blocknum] <- r,getBlockMarkers r (clustertrees.[blocknum])
                    spremover |> Map.remove blocknum |> Map.add blocknum (addsubplots dispstate blockMarkers.[blocknum] |> snd)
                match evt with
                | Save | Reset->
                    let sela = 
                        sel |> Set.toArray |> Array.map (fun ((blocknum,clupath) as x) -> 
                            x,getTree clustertrees.[blocknum] clupath
                        )
                    let newnodes =
                        if evt=Reset then sela |> Array.map (fun (x,(Node((_,clu),sts))) -> x,Node((Visible,clu),sts))
                        else sela |> Array.map (fun (x,(Node((_,clu),sts))) -> x,Node((Deleted,clu),[||]))
                    state,
                    newnodes |> Array.fold (fun state (x,newnode) -> 
                        update state x newnode) spremover,
                    Set.empty,state|>Option.map (fun x -> ResetAll,x),Some (sela|>Array.map fst)
                | LeftClick ->
                    match state with 
                    | Some (blocknum,clupath) ->
                        let (Node((vis,clu),sts) as node) = getTree clustertrees.[blocknum] clupath
                        if isChildSelected node |> not then
                            let newnode,newsel,outevt = 
                                match vis,isCtrlPressed () with
                                | Selected,true -> Node((Visible,clu),sts),sel|>Set.remove (blocknum,clupath),Removed|>Some
                                | Visible,true when List.length clupath > 0 ->
                                    Node((Selected,clu),sts|>Array.map (fun (Node((vis,clu),ssts)) -> 
                                        Node(((if vis=Deleted then Deleted else Hidden),clu),ssts))),
                                    sel|>Set.add (blocknum,clupath),
                                    Added|>Some
                                | Visible,false ->
                                    Node((Visible,clu),sts|>Array.map (fun (Node((vis,clu),ssts)) -> 
                                        Node(((if vis=Deleted then Deleted else if vis = Visible then Hidden else Visible),clu),ssts))
                                    ),sel,None
                                | _ -> node,sel,None
                            let newspremover = update spremover (blocknum,clupath) newnode                                  
                            state,
                            newspremover,
                            newsel,
                            outevt |> Option.map (fun x -> x,(blocknum,clupath)),None
                        else
                            state,spremover,sel,None,None
                    | _ ->
                        state,spremover,Set.empty,None,None         
                | NewCluster x -> Some x,spremover,sel,(Changed,x)|>Some,None
            ) x
        let sub = new Subject<_>()
        obs.Subscribe(sub) |> ignore
        sub |> Observable.choose (fun (_,_,_,selevt,_) -> selevt),
        sub |> Observable.choose (fun (_,_,_,_,saveevt) -> saveevt)
    (wnd,sps),xranges,yranges,seloper,saveoper

let getTreeinBlocks blocks (blocknum,clupath) = getTree (Array.get blocks blocknum) clupath |> fun (Node(x,_)) -> x


let plotClusterSpikeAmps (xrange:Range<double>) tranges clusterblocks yrange spikeamps evts = 
    let n2 = Array.length tranges
    let numblocks = Array.length clusterblocks
    let nspikes = Array.length spikeamps
    let trange = Range(spikeamps.[0]|>fst,spikeamps.[Array.length spikeamps-1]|>fst)
    let (wndspikes,l),selected,_ = 
        plotTicks trange yrange Colors.LightGray spikeamps
    let inline setcol i (col:Color) =
        let inline f b = (b|>float32)/255.0f
        l.[i].a <- f col.A
        l.[i].r <- f col.R
        l.[i].g <- f col.G
        l.[i].b <- f col.B
    wndspikes.Left <- 3072.//-1920.
    wndspikes.Width <- 1024.//960.
    wndspikes.Height <- 1112.//1040.
    wndspikes.Top <- 0.0
    addTimeXAxis wndspikes trange |> ignore
    addScaledYAxis wndspikes yrange Colors.Black (sprintf "%sV") |> ignore

    xrange.Changed |> Observable.subscribe (fun _ ->
        match clusterblocks|>Array.tryFindIndex (fun (_,x) -> xrange.Min < (x|>fst)) with
        | Some i ->
            let i = max 0 (i-1)
            match clusterblocks|>Array.rev|>Array.tryFindIndex (fun (_,x) -> xrange.Max > ((x|>fst)+(x|>snd))) with
            | Some j ->
                let j = max 0 (j-1) 
                let last = numblocks-1-j
                trange.Set(tranges.[i]|>fst,tranges.[last]|>snd)
            | None -> ()
        | None -> ()
    ) |> ignore

    let getsc = getTreeinBlocks (clusterblocks|>Array.map fst)
    //prefix operator
    let rec (&<=) xs ys = 
        match xs,ys with
        | xh::xt,yh::yt when xh=yh -> xt &<= yt
        | [],_ -> true
        | _ -> false
    evts
    |> Observable.scan (fun (tempsel,sel) (evt,x) ->
        for i in tempsel do
            setcol i Colors.LightGray
        let newstate = 
            match evt with
            | Changed ->                 
                let newtempsel =
                    let blocknum,clupath = x
                    sel |> Set.filter (fun (b,c) -> b=blocknum && clupath &<= c)
                    |> Set.fold (fun state x ->
                        Set.difference state (getsc x |> Set.ofArray)
                    ) (getsc x |> Set.ofArray)
                for i in newtempsel do
                    setcol i Colors.Black
                newtempsel,sel
            | Added -> 
                for i in getsc x do
                    setcol i Colors.Red
                Set.empty,sel |> Set.add x
            | Removed -> 
                for i in getsc x do
                    setcol i Colors.LightGray
                Set.empty,sel |> Set.remove x
            | ResetAll ->
                for x in sel do
                    for i in getsc x do
                        setcol i Colors.LightGray
                Set.empty,Set.empty
        wndspikes.Plot.ForceRefresh()
        newstate
    ) (Set.empty,Set.empty)
    |> Observable.subscribe (fun _ -> ())
    |> ignore
    wndspikes

let plotClusterSpikeShapes spikelen nchans ntemp yrange stableclusters spikeshapes evts =
    let plotselectedshapes = plotselectedshapes spikelen nchans ntemp yrange
    let getsc = getTreeinBlocks stableclusters
    evts
    |> Observable.scan (fun ((cur,sel),n) (evt,x) ->
        let newstate = 
            match evt with
            | Changed -> Some x,sel
            | Added -> cur,((x,n)::sel)
            | Removed -> cur,(sel|>List.filter (fun (a,_) -> a<>x))
            | ResetAll -> cur,[]
        newstate,n+1
    ) ((None,[]),0)
    |> Observable.choose (fun ((x,y),z) -> x |> Option.map (fun a -> 
        a,y|>List.sortBy snd |> List.rev |> List.toArray |> fun x -> x.[0..(min 5 (Array.length x))-1] |> Array.map fst)
    )
    |> Observable.subscribe (fun (cur,sel) ->
        let curclu = getsc cur
        plotselectedshapes 
            ((curclu,curclu.Items|>Array.map (fun i -> 
                Array.get spikeshapes i)),(sel|>Array.map getsc))
    )
    |> ignore

let changechainsv wnd xrange xranges yranges nchans ntemp clusters chains = 
    let col v = 
        let a = 0uy//(1.-v)*255.0|>byte
        Color.FromRgb(a,a,a)
    let cs = 
        chains
        |> Seq.groupBy List.length |> Seq.sortBy fst |> Seq.map (snd>>Seq.toArray) |> Seq.toArray |> Array.rev
    let chainlines = 
        cs |> Array.map (Array.map (Seq.pairwise>>Seq.map (fun (((i1,j1) as clupath1),((i2,j2) as clupath2)) ->
            let t i = transform (Array.get xranges i) xrange
            let clu1 = getTreeinBlocks clusters clupath1
            let x1,y1,ch1 = getClusPos nchans ntemp clu1
            let clu2 = getTreeinBlocks clusters clupath2
            let x2,y2,ch2 = getClusPos nchans ntemp clu2
            let v = LinkValue clu1.Shape clu2.Shape
            ((t i1 x1,(y1,ch1),t i2 x2,(y2,ch2))),(col v)
            )>>Seq.toArray)>>Array.concat>>(plotXChLines wnd xrange yranges false)>>togglespvisibility wnd)
    wnd.KeyDown |> Observable.scan (fun displevel x ->
        match x.Key with
        | Key.I when displevel < Array.length chainlines-1 ->
            chainlines.[displevel+1] true
            displevel+1
        | Key.K when displevel >= 0 -> 
            chainlines.[displevel] false
            displevel-1
        | _ -> displevel
    ) -1 |> Observable.subscribe (fun _ -> ()) |> ignore

