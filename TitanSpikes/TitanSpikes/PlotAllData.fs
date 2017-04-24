#if COMPILED
module PlotAllData
#else
#load "PlotHelper.fs"
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open System
open RP.Controls
open System.Windows.Media
open System.Windows
open Helper
open PlotHelper
open System.Xml.Linq
open VideoAlignment
open System.IO
open VideoAlignment
open ClusterHelper

let endpadding = 0.1
let inline t2sep t sr = (t-endpadding)*sr|>uint64
let inline dt2s dt sr = dt*sr|>int

let hostname = Local
let ephysfnum = 635276396558304920L
//let chgroup,cellnum = "5",@"1\73"
//let chgroup,cellnum = "5",@"1\49"
let chgroup,cellnum = "0",@"2"
let ephyspath = @"Z:\badlands"
let fpath =  sprintf @"%s\%d" ephyspath ephysfnum
let esets = loadnTrodes (XElement.Load(sprintf @"%s\SnippeterSettings.xml" fpath).Element(xn "EIBElectrodePlacement")) 

//let t0,dt = 13223594382./30000.,5.0
//let t0,dt = 8039910702./30000.,5.0
let t0,dt = 9982482852./30000.+50.,30.
let sr = 30000.
let lfpsr = sr/100.
let axlsr = sr/4.

let inline t2s t = t*sr|>uint64
let f x = (x|>float)/30000.-t0

let spiketimes = 
    getSpikeTimes hostname (sprintf @"%s\ChGroup_%s\Sorted\%s.stimes" fpath chgroup cellnum) 0UL -1 
    |> Array.map f |> Array.filter (fun t -> t >= 0. && t <= dt)

let axls = getAXL hostname (sprintf @"%s\AXL" fpath) (t2sep t0 axlsr) (dt2s dt axlsr)
let lfps = esets |> Array.map snd |> Array.concat |> Array.map (fun ch -> getLFP hostname (sprintf @"%s\LFP\Ch_%d" fpath ch) (t2sep t0 lfpsr) (dt2s dt lfpsr))
let ttls = [|1;2;4;5|]|>Array.map (fun ch -> getTTLChangesInRange Local (sprintf @"%s\TTLChanges\Ch_%d" fpath ch) (t0|>t2s) (dt|>t2s)|>snd)

let width,height = 500,656
let wnd = D3DPlot2DWindow(Title="All Data",Width=(width|>float)+16.,Height=(height|>float)+38.)
wnd.Background <- Brushes.White
wnd.Show()
let xrange = Range(0.0,dt)
let toplot = 
    seq{
        let getaxlch ch = [|getLine Colors.Black 0.0 (1.0/axlsr) (axls.[0..,ch]|>Array.map (fun x -> (x|>float)*0.0000374))|]
        yield getaxlch 0,(1.75,2.25)
        yield getaxlch 1,(1.50,2.00)
        yield getaxlch 2,(1.75,2.25)
//        for ch in [0] do
//            yield [|getLine Colors.Blue 0.0 (1.0/lfpsr) (lfps.[ch]|>Array.map (fun x -> (x|>float)*0.195))|],(-2e2,2e2)
        for ch in 1..3 do
            let gettapbox t1 t2 =
                let t1,t2 = f t1,f t2
                getLinexy Colors.Red [|t1,0.0;t1,1.0;t2,1.0;t2,0.0;t1,0.0|]
            yield (Array.init (Array.length ttls.[ch]/2) (fun i -> gettapbox ttls.[ch].[i*2] ttls.[ch].[i*2+1])),(-0.1,1.1)
        yield 
            spiketimes 
            |> Array.map (fun t -> getLinexy Colors.Blue [|t,0.0;t,1.0|])
            ,(-0.1,1.1)
    } |> Seq.toArray
let yrange = Range(-1.0,Array.length toplot |> float)
let yranges = 
    aggRange wnd yrange (toplot |> Array.mapi (fun i (_,x) -> (i|>float,1.0),x))
Array.zip toplot yranges |> Array.iter (fun ((lines,_),range) -> addSubPlot wnd xrange range (lines|>Seq.cast) |> ignore)

//addTimeXAxis wnd xrange |> ignore
//addYAxis wnd yrange Colors.Black "" |> ignore

let datafile = @"Z:\badlands\datafile.png"
let audiofile = @"Z:\badlands\spikes.pcm"
let videofile = @"Z:\badlands\video.mkv"

open System.Windows.Media.Imaging
let saveimg () = 
    let rect = Rect(wnd.PlotImage.RenderSize)
    let rtb = RenderTargetBitmap((int)rect.Right,(int)rect.Bottom,96.0,96.0,PixelFormats.Default)
    rtb.Render(wnd)
    let enc = new PngBitmapEncoder()
    enc.Frames.Add(BitmapFrame.Create(rtb))
    use fs = new FileStream(datafile,FileMode.Create)
    enc.Save(fs)

let spikeshape = 
    let nchans = esets|>Array.find(fun (x,_) -> x=chgroup)|>snd|>Array.length
    let shape = 
        getSpikeTimes hostname (sprintf @"%s\ChGroup_%s\Sorted\%s.snums" fpath chgroup cellnum) 0UL -1
        |> fun x -> 
            getSpikeSubsets hostname (sprintf @"%s\ChGroup_%s\Spikes" fpath chgroup) 
                (sprintf @"%s\ChGroup_%s\SpikeTimes" fpath chgroup) 
                (64*nchans) [|x.[0..99]|]
        |> fun x -> x.[0] |> Array.map (snd>>Array.map float>>fun x -> 1.,x) |> avgShape
    let ch,amp = Array.init nchans (fun i -> i,shape.[31*nchans+i]) |> Array.maxBy (snd>>abs)
    Array.init 64 (fun i -> shape.[i*nchans+ch]/amp)

let videofrate = 120.
let slowfact = 4.

let saveaudio =
    let sr = 44100.
    let f = 10000.
    let data = Array.zeroCreate<float> (dt*sr*slowfact|>int)
    spiketimes |> Array.iter (fun t -> 
        let indx = (t*sr*slowfact|>int) - ((Array.length spikeshape)/2)
        if (indx > 0 && indx < Array.length data - Array.length spikeshape) then
            System.Array.Copy(spikeshape,0,data,indx,Array.length spikeshape)
    )
    writeArray Local audiofile 0UL data |> Async.RunSynchronously

let videos = 
    //[|635280804161107294L,7,@"Z:\badlands\Cam2",9u,"transpose=2,mp=eq2=1.2:2.0:0.3:2.5",524;635280804160877294L,6,@"Z:\badlands\Cam1",1u,"transpose=1,mp=eq2=1.0:2.0:0.3:2.5",524;|]
    //[|635279076170867523L,7,@"Z:\badlands\Cam2",7u,"transpose=2,mp=eq2=1.2:2.0:0.3:2.5",524;635279076170677523L,6,@"Z:\badlands\Cam1",2u,"transpose=1,mp=eq2=1.0:2.0:0.3:2.5",524;|]
    [|635279724190817725L,7,@"Z:\badlands\Cam2",8u,"transpose=2,mp=eq2=1.2:2.0:0.3:2.5",524;635279724190557725L,6,@"Z:\badlands\Cam1",0u,"transpose=1,mp=eq2=1.0:2.0:0.3:2.5",524;|]
    |> Array.map (fun (videofnum,syncch,videopath,offset,transpose,videowidth) ->
        let frames = getAlignedFrameTimes videofnum ephysfnum syncch videopath ephyspath offset (1./videofrate) |> Option.get
        getVideoFiles frames (t0|>t2s) (dt|>t2s)
        |> Array.map (fun (f,a,b,c) -> sprintf @"%s\%d\%d.mkv" videopath videofnum f,a,b,c)
        |> fun x -> (videowidth,transpose),x
    )
ffmpegargs height datafile width videos (dt*120.|>int) videofile (videofrate/slowfact)
|> printfn ".\\ffmpeg.exe %s"
printfn ".\\ffmpeg.exe -f f64le -ar 44.1k -i %s -i %s -vcodec copy %s" audiofile videofile @"Z:\badlands\audiovideo.mkv"

#if COMPILED
[<System.STAThread()>]
(new Application()).Run() |> ignore
#endif

//let files = Array.zip videos frametimes |> Array.map (fun ((videopath,videofnum,_,_),(frames,_)) ->
//    getVideoFiles frames (t2s t0) (t2s dt)
//    |> Array.map (fun (file,first,last,offset) ->
//        sprintf "%s\%d\%d.mkv" videopath videofnum file,first,last,offset
//    )
//)
//
//let cmd = 
//    ffmpegargs 656 @"Z:\badlands\test.png" (width|>int) 
//        [|(524,"transpose=3,mp=eq2=1.0:2.9:0.7:3.0"),files.[0]|]  (dt*120.|>int) @"Z:\badlands\out.mkv" 30.0
//printfn "%s" cmd
//
