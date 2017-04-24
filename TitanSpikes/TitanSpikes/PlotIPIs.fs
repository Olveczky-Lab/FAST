#if COMPILED
module PlotIPIs
#else
#load "PlotHelper.fs"
#time
fsi.ShowDeclarationValues <- false
#endif

open TwoTapHelper
open PlotHelper
open RP.Controls
open System.Windows.Media
open System.Windows.Input

let plotallipis cstr exptid sms =
    let db2tap = db2tap(cstr)
    let l = id
    let plotipis wnd xrange yrange offset (ipis,lower,upper) =
        let n = Array.length ipis |> float
        let nipis = ipis|>Array.mapi (fun i ipi -> (i,ipi))
        let plotpoints size color f =
            getPoints size color (nipis|>Array.filter (fun (_,ipi) -> f (ipi.Type))
                                       |>Array.map (fun (i,ipi) -> float i + offset,float ipi.Duration|>l))
        let size = 5.0f
        let lines = 
            [|getLine Colors.Black (offset-0.5) n [|lower|>float|>l;lower|>float|>l|];
             getLine Colors.Black (offset-0.5) n [|upper|>float|>l;upper|>float|>l|];
             plotpoints size (Colors.Red) ((=)(UnRewarded 0));
             plotpoints size (Colors.Black) (function | UnRewarded n when n >= 1 -> true |_ -> false);
             plotpoints size (Colors.Black) ((=)NewTrialAfterFailure);
             plotpoints size (Colors.Blue) ((=)NewTrialAfterSingleTap);
             plotpoints size (Colors.DarkGreen) ((=)Rewarded);
             plotpoints size (Colors.Aqua) ((=)NewTrialAfterReward)
            |]
        addSubPlot wnd xrange yrange (lines|>Seq.cast) |> ignore
        lines
    let ipis = 
        sms |> List.map (fun (smid,(lower,upper,target)) -> db2tap.getipis(exptid,smid),lower,upper) 
        |> List.toArray       
    
    let wnd = new D3DPlot2DWindow()
    wnd.Show()

    let xrange = Range(0.0,0.0)
    addXAxis wnd xrange Colors.Black "Trial Number" |> ignore

    let yrange = Range(100.0|>l,6000.0|>l)
    addYAxis wnd yrange Colors.Black "Interval (ms)" |> ignore

    let ntotal,lines = 
        ipis |> Array.fold (fun (n,xs) ((curipis,_,_) as x) -> 
            let lines = plotipis wnd xrange yrange (n|>float) x
            n+100+(Array.length curipis),lines::xs) (0,[])
    let nlines = List.head lines |> Array.length
    xrange.Set(0.0,ntotal|>float)
    wnd.KeyDown.Add (fun x -> 
        match (x.Key-Key.D1|>int) with
        | a when (a < nlines) -> 
            lines |> Seq.iter (fun xs -> xs.[a].Visible <- (xs.[a].Visible |> not))
            wnd.Plot.ForceRefresh()
        | _ -> ())

let cstr = "Data Source=140.247.178.225;Initial Catalog=OpConASHESHDHAWALE1;User ID=asheshdhawale;Password=elawahdhsehsa!"
plotallipis (cstr,"TwoTapTrialAndErrorV2") 79 (db2tap(cstr,"TwoTapTrialAndErrorV2").getTwoTapSMs 79 |> List.filter (fun (smid,_) -> smid >= 312 && smid <= 405)) 
