module Helper

open MathHelper
open System.Xml.Linq
open System.Net

let calcMAD endPadding chnum b = 
    let MAD d =
        let dn = d |> Array.map (fun x -> abs x) |> Array.sort
        dn.[dn.Length/2]
    let totalsamples = (Array.length b)/bytespersample
    let d = scaleData 32768.0 b chnum |> filtfilt spikes_bandpass
    MAD (d.[endPadding..totalsamples-endPadding-1])

let calcThresholds blocksize b chnum =
    let mad = calcMAD blocksize b chnum
    let sd = mad*1.4826
    (sd*5.0,sd*2.0)

open System.IO

let getThresholds fname blocksize curBlock =
    let b = Array.zeroCreate<byte> (blocksize*2*64)
    use fs = new FileStream(fname,FileMode.Open,FileAccess.Read)
    let offset = (curBlock|>int64)*(blocksize|>int64)*2L*64L
    fs.Seek(offset,SeekOrigin.Begin) |> ignore
    fs.Read(b,0,b.Length) |> ignore
    List.init 64 (fun chnum -> async { return calcThresholds blocksize chnum b})
    |> Async.Parallel |> Async.RunSynchronously
type DataSource =
    | Local
    | Remote of string

let rec getSize datasource fname = async{
    try
        match datasource with
        | Remote hostname ->
            let req = WebRequest.Create(sprintf "http://%s/size?FileName=%s" hostname fname)
            use! resp = req.AsyncGetResponse()
            use s = resp.GetResponseStream()
            use sr = new StreamReader(s)
            return sr.ReadToEnd()|>int64
        | Local ->
            return FileInfo(fname).Length
    with
        | :? System.Net.WebException ->
            printfn "Error Reading Size. Trying again."
            return! getSize datasource fname
}

let rec getBytes datasource fname offset nb = async{
    try
        match datasource with
        | Remote hostname ->
            let req = WebRequest.Create(sprintf "http://%s/read?FileName=%s&Offset=%d&NumBytes=%d" hostname fname offset nb)
            use! resp = req.AsyncGetResponse()
            use s = resp.GetResponseStream()
            if nb > 0 then
                let b = Array.zeroCreate<byte> nb 
                use ms = new MemoryStream(b)
                s.CopyTo(ms)
                return b
            else
                use ms = new MemoryStream()
                s.CopyTo(ms)
                return ms.ToArray()
        | Local ->
            use fs = new FileStream(fname,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
            fs.Seek(offset|>int64,SeekOrigin.Begin)|>ignore
            let b = Array.zeroCreate<byte> (if nb < 0 then fs.Length|>int else nb)
            fs.Read(b,0,Array.length b)|>ignore
            return b

    with
        | :? System.Net.WebException ->
            printfn "Error Getting Bytes. Trying again."
            return! getBytes datasource fname offset nb
}

let rec setBytes datasource fname offset bytes = async{
    try
        let nb = Array.length bytes
        match datasource with
        | Remote hostname ->
            let req = WebRequest.Create(sprintf "http://%s/write?FileName=%s&Offset=%d&NumBytes=%d" hostname fname offset nb)
            req.Method <- "POST"
            req.ContentLength <- nb|>int64
            req.ContentType <- "application/x-www-form-urlencoded"
            use ds = req.GetRequestStream()
            ds.Write(bytes,0,nb)
            ds.Close()
            use! resp = req.AsyncGetResponse()
            ()
        | Local ->            
            use fs = new FileStream(fname,FileMode.OpenOrCreate,FileAccess.Write,FileShare.ReadWrite)
            fs.Seek(offset|>int64,SeekOrigin.Begin)|>ignore
            fs.Write(bytes,0,nb)
        return ()
    with
        | :? System.Net.WebException ->
            printfn "Error Setting Bytes. Trying again."
            return! setBytes datasource fname offset bytes
}

open System

let inline offsetbytes offset array = 
    let nb = Buffer.ByteLength(array)  
    let dims = Array.init array.Rank (fun i -> array.GetLength(i))
    (offset*((nb/array.Length*(dims.[1..]|>Array.fold (*) 1))|>uint64))

let writeArray hostname fname offset array =
    let nb = Buffer.ByteLength(array)
    if nb > 0 then
        let bytes = Array.zeroCreate<byte> nb
        Buffer.BlockCopy(array,0,bytes,0,nb)
        setBytes hostname fname (offsetbytes offset array) bytes
    else async{return ()}

let getRHDBytes hostname fname byteoffset blocksize endPadding curBlock =
    let offset = byteoffset + (curBlock|>uint64)*(blocksize|>uint64)*(bytespersample|>uint64)
    let nb = (blocksize+endPadding*2)*(bytespersample)
    getBytes hostname fname offset nb

let getAMPBytes hostname fname byteoffset blocksize endPadding curBlock =    
    let offset = byteoffset + (curBlock|>uint64)*(blocksize|>uint64)*(bytespersampleOld|>uint64)
    let nb = (blocksize+endPadding*2)*(bytespersampleOld)
    getBytes hostname fname offset nb

let getArrayAsync<'a> hostname fname offset count = async {
        let nperframe = offsetbytes 1UL (Array.zeroCreate<'a> 1)|>int
        let! bytes = getBytes hostname fname (offset*(nperframe|>uint64)) (if count < 0 then -1 else (count*nperframe))
        let array = Array.zeroCreate<'a> (Array.length bytes/nperframe)
        Buffer.BlockCopy(bytes,0,array,0,Array.length bytes)
        return array
    }

let getArray<'a> hostname fname offset count = getArrayAsync<'a> hostname fname offset count |> Async.RunSynchronously

let getArray2DAsync<'a> dim hostname fname offset count = async {
        let nperframe = offsetbytes 1UL (Array2D.zeroCreate<'a> 1 dim)|>int
        let! bytes = getBytes hostname fname (offset*(nperframe|>uint64)) (if count < 0 then -1 else (count*nperframe))
        let array = Array2D.zeroCreate<'a> (Array.length bytes/nperframe) dim
        Buffer.BlockCopy(bytes,0,array,0,Array.length bytes)
        return array
    }

let getArray2D<'a> dim hostname fname offset count = getArray2DAsync<'a> dim hostname fname offset count |> Async.RunSynchronously

let getArray3DAsync<'a> dim2 dim3 hostname fname offset count = async {
        let nperframe = offsetbytes 1UL (Array3D.zeroCreate<'a> 1 dim2 dim3)|>int
        let! bytes = getBytes hostname fname (offset*(nperframe|>uint64)) (if count < 0 then -1 else (count*nperframe))
        let array = Array3D.zeroCreate<'a> (Array.length bytes/nperframe) dim2 dim3
        Buffer.BlockCopy(bytes,0,array,0,Array.length bytes)
        return array
    }

let getArray3D<'a> dim2 dim3 hostname fname offset count = getArray3DAsync<'a> dim2 dim3 hostname fname offset count |> Async.RunSynchronously

let getVDD = getArray2D<uint16> 2
let getAXL = getArray2D<uint16> 3
let getSpikes spikelen nchans = getArray3D<int16> spikelen nchans
let getSpikeTimes = getArray<uint64>
let getTemp = getArray2D<float> 2
let getTTLIns = getArray<uint16>
let getTTLChanges = getArray<uint64>
let getLFP = getArray<int16>

let xn n = XName.op_Implicit(n)

let serializeThresholds ts = 
    XElement(xn "Thresholds",
        ts 
        |> Seq.mapi (fun i (event_thr,return_thr) -> 
            XElement(xn "Channel", 
                XAttribute(xn "Number",i),                                                            
                XAttribute(xn "EventThreshold",event_thr),
                XAttribute(xn "ReturnThreshold",return_thr)))).ToString()

let loadThresholds (x:XElement) =
    x.Elements(xn "Channel")
    |> Seq.map (fun x ->
        (x.Attribute(xn "Number")|>int),
         (x.Attribute(xn "EventThreshold")|>float,x.Attribute(xn "ReturnThreshold")|>float))
    |> Map.ofSeq

let loadnTrodes (x:XElement) =
    x.Elements(xn "ElectrodeSet")
    |> Seq.map (fun x -> (x.Attribute(xn "Name").Value,x.Elements(xn "Channel")
                                                       |> Seq.map (int) |> Seq.toArray))
    |> Seq.toArray

type binSearchResult = Exact of int64 | LargerThan of int64 | TooSmall
type ComparisonResult = TargetIsSmaller|TargetIsLarger|Match

let inline compare target x = 
    match x with 
    | _ when x < target -> TargetIsLarger
    | _ when x > target -> TargetIsSmaller
    | _ -> Match 

let binSearch targetfunc min max =
    let rec binSearch' min max =  
        if (max < min) then
            if (min = 0L) then TooSmall else LargerThan max
        else
            let middle = (min + max) / 2L
            match targetfunc middle with
            | TargetIsSmaller ->
                binSearch' min (middle-1L)
            | TargetIsLarger ->
                binSearch' (middle+1L) max
            | Match ->
                Exact middle
    binSearch' min max

let getOffset hostname fname t = 
    let numspikes = (getSize hostname fname |> Async.RunSynchronously)/8L
    let getspiketime spikenum =
        getSpikeTimes hostname fname (spikenum|>uint64) 1 |> Seq.head
    match binSearch (fun i -> compare t (getspiketime i)) 0L (numspikes-1L) with
    | Exact x
    | LargerThan x -> x+1L
    | TooSmall -> 0L
    |> uint64

let getSubSetBinSearch numspikes getspiketime (t0,t1) =
    let firstindx =
        match binSearch (fun i -> compare t0 (getspiketime i)) 0L (numspikes-1L) with
        | Exact x -> x
        | LargerThan x -> x+1L
        | TooSmall -> 0L
        |> uint64
    let lastindx =
        match binSearch (fun i -> compare t1 (getspiketime i)) 0L (numspikes-1L) with
        | Exact x -> x
        | LargerThan x -> x
        | TooSmall -> 0L
        |> uint64
    firstindx,lastindx

let getSubSetindx hostname fname (t0,t1) = 
    let numspikes = (getSize hostname fname |> Async.RunSynchronously)/8L
    let inline getspiketime spikenum =
        getSpikeTimes hostname fname (spikenum|>uint64) 1 |> Seq.head
    getSubSetBinSearch numspikes getspiketime (t0,t1)