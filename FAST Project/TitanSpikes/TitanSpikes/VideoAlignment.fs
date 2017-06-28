module VideoAlignment

open Helper
open System.IO
open System.Diagnostics

let inline diff xs = xs |> Seq.pairwise |> Seq.map (fun (a,b) -> b-a) |> Seq.toArray
let inline selecteven xs = Array.init (Array.length xs/2) (fun i -> xs.[2*i])
let inline last xs = Array.get xs (Array.length xs-1)
let inline cumsum xs = Seq.scan (+) (LanguagePrimitives.GenericZero) xs

let getTTLChangesInRange fname t0 dt =
    let inline evenize x = if (x%2UL=0UL) then x else x-1UL
    let first = getOffset fname t0 |> evenize
    let last = getOffset fname (t0+dt)
    first,getTTLChanges Local fname first (last-first+1UL|>int)

let getStart (videofnum:int64) (ephysfnum:int64) syncch videopath ephyspath = 
    let ephysfname = sprintf @"%s\%d\TTLChanges\Ch_%d" ephyspath ephysfnum syncch
    let metafname = sprintf @"%s\%d\%d.meta" videopath videofnum videofnum
    let firstframe = (getArray<uint32> Local metafname 0UL 1).[0]
    let offset = (videofnum-ephysfnum|>float)/10000000.
    let inline t2s t = t*30000.|>uint64
    let first,ttlchanges = getTTLChangesInRange ephysfname ((offset-30.0)|>t2s) (60.0|>t2s)
    let o = diff ttlchanges |> Array.mapi (fun i x -> (i,x)) |> Array.filter (fun (i,x) -> x > 10000UL)
    if (Array.length o <> 1) then failwith "Too many pauses in sync!"
    firstframe,(first+(o.[0]|>fst|>uint64))

let numFrames path syncch o1 o2 =
    let ephysfname = sprintf @"%s\TTLChanges\Ch_%d" path syncch
    let x = getTTLChanges Local ephysfname o1 (o2-o1+1UL|>int) |> diff
    printfn "%A" (x|>Seq.countBy id|>Seq.toArray |> Array.sortBy fst)
    let goodx,badx = x|>Array.partition (fun x -> x > 120UL && x < 130UL)
    let a = Array.length goodx
    let b = (Array.sum badx |> float)/(goodx|>Array.map float|>Array.average)
    let r = a+(round b|>int)
    printfn "%A" (a,b,r)
    r

let getAlignedFrameTimes (videofnum:int64) (ephysfnum:int64) syncch videopath ephyspath initoffset frameinterval = 
    let ephysfname = sprintf @"%s\%d\TTLChanges\Ch_%d" ephyspath ephysfnum syncch
    let metafname = sprintf @"%s\%d\%d.meta" videopath videofnum videofnum

    let framenums,firstframe =
        let tmp = getArray<uint32> Local metafname 0UL -1
        Array.init ((Array.length tmp)/2) (fun i -> tmp.[2*i] - tmp.[0] + initoffset |> int),tmp.[0]

    let offset = (videofnum-ephysfnum|>float)/10000000.
    let inline t2s t = t*30000.|>uint64
    let first,ttlchanges = getTTLChangesInRange ephysfname ((offset-30.0)|>t2s) (60.0|>t2s)
    let posedges = ttlchanges |> selecteven
    let o = diff posedges |> Array.tryFindIndex (fun x -> x > 10000UL)
    let minint = frameinterval*30000.*0.98|>uint64
    let maxint = frameinterval*30000.*1.02|>uint64
    match o with
    | Some o ->
        let allnegedges = 
            getTTLChanges Local ephysfname (first+((o+1)*2+1|>uint64)) ((framenums.[Array.length framenums-1]+1000)*2)
            |> selecteven
            |> Array.toList 
        let rec removenoise xs ys = 
            match xs with
            | a::b::t ->
                if (b-a > minint) then removenoise (b::t) (a::ys) else removenoise (a::t) ys
            | _ -> ys
        let goodnegedges =
            removenoise allnegedges [] |> List.rev |> List.toArray
            |> fun x -> x.[0..framenums.[Array.length framenums-1]]
        if (goodnegedges |> diff |> Array.exists (fun a -> a < minint || a > maxint)) then 
            None
        else
            let framestarts = 
                Directory.GetFiles(sprintf "%s\%d" videopath videofnum,"*.mkv")
                |>Array.map(fun x -> FileInfo(x).Name.Split('.').[0]|>int)|>Array.sort
            let num2starts = framenums |> Array.map (fun _ -> framestarts.[Array.length framestarts-1])
            framestarts |> Seq.pairwise |> Seq.iter (fun (a,b) -> 
                for i in a..b-1 do
                    num2starts.[i] <- a
            )
            let tmp = goodnegedges |> Array.map (fun t -> t, None)
            framenums |> Array.iteri (fun i framenum ->
                let t,n = tmp.[framenum] 
                tmp.[framenum] <- t,Some (num2starts.[i],i-num2starts.[i])
            )
            Some tmp
    | None -> None

let split splitfun xs =
    xs |> Seq.pairwise |> Seq.mapi (fun i x -> (i,x)) |> Seq.choose (fun (i,(a,b)) -> if (splitfun a b) then Some (i+1) else None)
    |> fun x -> seq {yield 0; yield! x; yield (Array.length xs)}
    |> Seq.pairwise |> Seq.map (fun (i,j) -> xs.[i..j-1]) |> Seq.toArray

let inline getVideoFiles frames (t0:uint64) (dt:uint64) =
    let inline closest t = frames |> Array.map fst |> Array.mapi (fun i x -> (i,(x|>int64)-(t|>int64)|>abs)) |> Array.minBy snd |> fst
    let first,last = closest t0, closest (t0+dt)
    frames.[first..last] |> Array.map snd |> split (fun n1 n2 -> 
        match n1,n2 with
        | Some (s1,_),Some(s2,_) when s1<>s2 -> true
        | Some _, None -> true
        | _ -> false
    )
    |> Array.map (fun xs ->
        xs |> Array.choose id
        |> fun x -> 
            let file,first = x.[0]
            let _,last = x.[Array.length x-1]
            file,first,last,(xs|>Array.filter Option.isNone|>Array.length)
    )

let ffmpegargs height datafile datawidth videofiles nframes outfile frate = 
    let sinputs = 
        videofiles 
        |>Array.map (snd>>(Array.map (fun (f,_,_,_) -> sprintf "-r %.2f -i %s" frate f))) |> Array.concat
        |>String.concat(" ")
    let sselect = 
        Seq.zip videofiles (videofiles |> Seq.map (snd>>Array.length) |> cumsum) |> Seq.toArray |> Array.mapi (fun j ((_,xs),n) -> 
            xs |> Array.mapi (fun i (_,first,last,offset) -> 
                sprintf "[%d:0]select=gte(n\,%d)*lte(n\,%d),setpts=(N+%d)/(%.2f*TB)[x%d_%d]" (i+1+n) first last offset (frate) j i
            )
        ) |> Array.concat |> String.concat(";")
    let sconcat = 
        videofiles |> Array.mapi (fun j ((_,trans),xs) ->
            let nfiles = Array.length xs
            let inps = Array.init nfiles (fun i -> sprintf "[x%d_%d]" j i) |> String.concat("")
            if (nfiles > 1) then sprintf "%sconcat=n=%d," inps nfiles else inps
            |> fun x -> sprintf "%s%s[x%d]" x trans j
        )|>String.concat(";")
    let cumw = videofiles |> Array.map (fst>>fst) |> cumsum |> Seq.toArray
    let soverlay =
        Seq.zip cumw videofiles |> Seq.toArray |> Array.mapi (fun j (w,_) ->
            sprintf "[y%d][x%d]overlay=x=%d[y%d]" j j (w+datawidth) (j+1)
        ) |> String.concat(";")
        |> fun x -> x.Substring(0,x.Length-4)
    let sfilter = 
        sprintf "color=black@0.5:2x%d[c];[0:0][c]overlay=format=yuv444:x=%.2f*t,pad=%d:%d:0:0:white[y0];%s;%s;%s"
            height ((datawidth|>float)*frate/(nframes|>float)) (datawidth+cumw.[Array.length cumw-1]) height
            sselect sconcat soverlay 
    sprintf ".\\ffmpeg -loop 1 -r %.2f -i %s %s -filter_complex \"%s\" -frames %d -y %s"
        frate datafile sinputs sfilter nframes outfile

let saveAvgTapFrame ephyspath ephysfnum videopath videofnum syncch initoffset tapch tmppath (outdown,outup) =
    if Directory.Exists(tmppath)=false then Directory.CreateDirectory(tmppath)|>ignore
    let frames = 
        getAlignedFrameTimes videofnum ephysfnum syncch videopath ephyspath initoffset (1./120.) |> Option.get
    let t0,t1 = frames.[0]|>fst,frames|>last|>fst
    let getFrame t =
        let numframes = Array.length frames
        match binSearch (fun i -> compare t (frames.[i|>int]|>fst)) 0L ((numframes|>int64)-1L) with
        | Exact x -> Some (int x)
        | LargerThan x ->
            let x = int x 
            if x=numframes-1 then None else
            if t-(frames.[x]|>fst) < (frames.[x+1]|>fst)-t then x else x+1
            |> Some
        | TooSmall -> None
        |> Option.bind (fun x -> frames.[x]|>snd)
  
    let x = getTTLChangesInRange (sprintf @"%s\%d\TTLChanges\Ch_%d" ephyspath ephysfnum tapch) t0 (t1-t0) |> snd
    let f outfile a = 
        a|>selecteven|>Array.choose getFrame
        |>fun x -> x.[0..(min 100 (Array.length x))-1]
        |>Array.Parallel.iteri (fun i (f,s) ->
            let args = sprintf @"-r 1 -i %s\%d\%d.mkv -ss %d -frames 1 -y %s\tmp%05d.png" videopath videofnum f s tmppath (i+1)
            let p = new Process(StartInfo=ProcessStartInfo(@"\\140.247.178.88\Rajesh\ffmpeg\bin\ffmpeg.exe",args,UseShellExecute=false))
            p.Start()|>ignore
            p.WaitForExit()
        )
        let p = 
            new Process(StartInfo=
                ProcessStartInfo(@"\\140.247.178.88\Rajesh\ImageMagick\convert.exe",
                                 sprintf @"tmp*.png -evaluate-sequence mean %s" outfile,
                                 WorkingDirectory=tmppath,
                                 UseShellExecute=false))
        p.Start()|>ignore
        p.WaitForExit()
        Directory.GetFiles(tmppath,"tmp*.png")|>Array.iter(fun x -> File.Delete(x))
    f outdown x
    f outup x.[1..]

let ephyspath = @"Y:\Data\Desh"
let ephysfnum = 635313447284390991L
let videopath = @"Y:\Data\Desh\PGCamL"
let videofnum = 635313456127185182L
let syncch = 5
let tapch = 1
let tmppath = @"C:\temp\Tap"
let initoffset = 0u
let outdown = @"C:\temp\avgdown.png"
let outup = @"C:\temp\avgup.png"
