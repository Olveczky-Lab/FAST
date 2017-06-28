#load @"D:\Rajesh\Ephys\FlexiPhys\PlotBin\PlotHelper.fsx"
fsi.ShowDeclarationValues <- false
open System.IO
open System
open PlotHelper
open RP.Controls
open System.Windows.Media
let fname = @"Z:\badlands\635276396558304920.rhd"

let bytespersample = 176
let numPerFrame = 300000 //fs.Length/(bytespersample|>int64)|>int
let timestampOffset = 8
let ttlInOffset = 8+4+72*2+16

let data = 
    use fs = new FileStream(fname,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
    let numToSkip = 0UL //12574391802L//fs.Length/176L-(numPerFrame|>int64)//30000.*3600.*9.975|>int64//
    let d = Array.zeroCreate<byte> (numPerFrame*bytespersample)
    fs.Seek((numToSkip|>int64)*(bytespersample|>int64),SeekOrigin.Begin)|>ignore
    fs.Read(d,0,d.Length)|>ignore
    d
let axlOffset = 8+4+6
let vddOffset = 8+4+4
let axls = Array2D.init (numPerFrame/4) 3 (fun i j -> BitConverter.ToUInt16(data,(i*4+j+1)*bytespersample+axlOffset))
           |> Array2D.map (fun x -> (x|>float)*0.0000374)

let vdd = Array2D.init (numPerFrame/60) 2 (fun i j -> BitConverter.ToUInt16(data,(i*60+28)*bytespersample+vddOffset+j*2))
           |> Array2D.map (fun x -> (x|>float)*0.0000748)

let temp =
    Array2D.init (numPerFrame/60) 2 (fun i j ->
        let inline temp x = BitConverter.ToUInt16(data,(i*60+x)*bytespersample+vddOffset+j*2)
        (temp 20 - temp 12 |> float)/98.9-273.15
    )

PlotHelper.plottimeline "" (1.0/500.) (temp.[0..,1])

let magic = Array.init numPerFrame (fun i -> BitConverter.ToUInt64(data,i*bytespersample))
magic |> Array.tryFindIndex (fun x -> x <> 14308245636707916098UL) |> printfn "%A"

//let rec getTimeStamp skipback o =
//    if (data.[o] = 0x91uy && data.[o+1] = 0xc6uy) then getTimeStamp skipback (o+2) else BitConverter.ToUInt32(data,o-(if skipback then 2 else 0))
//
//let rec getTTLIn o =
//    if (data.[o] = 0x0auy && data.[o-1] = 0x00uy) then getTTLIn (o-2) else BitConverter.ToUInt16(data,o-1)
//
//let magicoffsets = 
//    seq {
//        for o in 0..2..(Array.length data)-1000 do
//            if (BitConverter.ToUInt64(data,o) = 14308245636707916098UL) then yield o
//    }
//    |> Seq.toArray

//magicoffsets |> Seq.pairwise |> Seq.map (fun (a,b) -> b-a|>float) |> Seq.toArray |> PlotHelper.plottimeline "" (1./30000.)

//let timestamps =
//    magicoffsets |> Array.map (fun o -> BitConverter.ToUInt32(data,o+timestampOffset))
//let ttlIns =
//    magicoffsets |> Array.map (fun o -> BitConverter.ToUInt16(data,o+ttlInOffset))
    
//let timestamps = 
//    magicoffsets.[0..(Array.length magicoffsets)-1] |> Array.scan (fun s o -> getTimeStamp (s&&&0xffffu=0xc690u) (o+8)) 0u
//    |> fun x -> x.[1..]
//
//let ttlIns = 
//    magicoffsets.[1..] |> Array.map (fun o -> getTTLIn (o-1))

let fstart = 0
for framenum in fstart..1..fstart+50 do 
    data.[framenum*176..(framenum+1)*176-1] |> Seq.iter (printf "%02x")
    printfn ""

//let frameEqual f1 f2 = 
//    let o1 = magicoffsets.[f1]
//    let o2 = magicoffsets.[f2]
//    data.[o1..o1+175] = data.[o2..o2+175]
//for i in 0..2999 do
//    printfn "%d\t%A" i (frameEqual (120582+i) (123582+i))    


let timestamps = Array.init numPerFrame (fun i -> BitConverter.ToUInt32(data,i*bytespersample+timestampOffset))

PlotHelper.plottimeline "" 1.0 (timestamps |> Array.map float)

let tsdiffs = timestamps|>Seq.map float|>Seq.pairwise|>Seq.map (fun (a,b) -> b-a)|>Seq.toArray

tsdiffs |> Array.mapi (fun i x -> (i,x))|>Array.choose (fun (i,x) -> if x <> 1. then Some i else None)|>Array.iter (printfn "%d")

PlotHelper.plottimeline "TimeStamps" 1.0 tsdiffs

let ttlIns = Array.init numPerFrame (fun i -> BitConverter.ToUInt16(data,i*bytespersample+ttlInOffset))

PlotHelper.plottimeline "TTLIns" (1.0/30000.) (ttlIns|>Array.map (fun x -> (x&&&256us)|>float))

let ttlChanges = Seq.zip timestamps ttlIns |> Seq.pairwise
                 |> Seq.choose (fun ((_,a),(x,b)) -> 
                    if ((a &&& 256us) <> (b &&& 256us)) then Some x else None
                 ) 
                 |> Seq.toArray
ttlChanges |> Seq.pairwise |> Seq.map (fun (a,b) -> (b-a|>float)/1.0) |> Seq.toArray |> PlotHelper.plottimeline "" 1.0

let ampOffset = 8+4+12
let amps = Array2D.init numPerFrame 64 (fun i j -> BitConverter.ToUInt16(data,i*bytespersample+ampOffset+j*2))
           |> Array2D.map (fun x -> ((x|>float)-32768.0)*0.195)
