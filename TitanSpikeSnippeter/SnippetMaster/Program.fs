#if COMPILED
#else

#load "MathHelper.fs"
#load "Helper.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open Helper
open System
open System.IO
open ZMQMapReduce

open MathHelper
let dfact = 100

type ChGroupData =
    {
        Spikes:int16[,,]
        SpikeTimes:uint64[]
        LFPs:int16[][]      
    }

type RHDData =
    {
        SpikesAndLFPs:ChGroupData[]
        AXL:uint16[,]
        Temp:float[,]
        VDD:uint16[,]
        TTLIns:uint16[]
        Refs:int16[][]
    }

let getAuxData endPadding blocksize b = 
    let axlOffset = 8+4+6
    let vddOffset = 8+4+4
    let ttlInOffset = 8+4+72*2+16

    let axl = 
        Array2D.init (blocksize/4) 3 (fun i j -> 
            BitConverter.ToUInt16(b,bytespersample*(endPadding+i*4+j+1)+axlOffset))

    let vdd =
        Array2D.init (blocksize/60) 2 (fun i j ->
            BitConverter.ToUInt16(b,(endPadding+i*60+28)*bytespersample+vddOffset+j*2))

    let temp =
        Array2D.init (blocksize/60) 2 (fun i j ->
            let inline temp x = BitConverter.ToUInt16(b,(endPadding + i*60+x)*bytespersample+vddOffset+j*2)
            (temp 20 - temp 12 |> float)/98.9-273.15
        )

    let ttlIns = 
        Array.init blocksize (fun i -> BitConverter.ToUInt16(b,(endPadding+i)*bytespersample+ttlInOffset))
    {SpikesAndLFPs=[||];AXL=axl;VDD=vdd;Temp=temp;TTLIns=ttlIns;Refs=[||]}

let getRefData scalefun channelgroups refchans endPadding blocksize b =
    let tmp_data =
        channelgroups 
        |> Array.map 
            (fun (_,chs) -> 
                    chs
                    |> Array.map 
                        (fun (chnum,(event_thr,return_thr)) ->
                                let d_s =
                                    scalefun (32768.0) b chnum
                                let downsampled_d = d_s |> downsample100
                                let d = d_s |> filtfilt spikes_bandpass
                                let lfp = downsampled_d.[endPadding/dfact..(endPadding+blocksize)/dfact-1]|>Array.map int16
                                chnum,(event_thr,return_thr,d),lfp))
    
    printfn "Computing Median"
    //Median reference
    let all_filtered = 
        let xs = tmp_data |> Array.map (Array.map (fun (chnum,(_,_,d),_) -> chnum,d)) |> Array.concat
        if Array.length xs = 0 then [||] else
        let tmp = Array.zeroCreate ((xs|>Array.maxBy fst|>fst)+1)
        xs |> Array.iter (fun (chnum,x) -> tmp.[chnum] <- x)
        tmp
    let ref = 
        refchans |> Array.map (fun refchan ->
            let nchans = Array.length refchan
            if nchans = 0 then [||] else
            Array.init (blocksize+endPadding*2) (fun i -> 
                refchan |> Array.map (fun j -> all_filtered.[j].[i]) |> Array.sort |> fun x -> x.[nchans/2])
        )
    tmp_data,ref

let extractSpikesAndLFPs (tmp_data,(ref:float[][])) w_pre w_post refchans blocksize endPadding curBlock = 
    tmp_data
    |> Array.map 
        (fun ds -> 
            let spikes = 
                ds |> Array.map (fun (chnum,(a,b,xs),_) -> 
                    let refnum = refchans |> Array.findIndex (Array.exists ((=)chnum))
                    (a,b,xs|>Array.mapi (fun i x -> x-ref.[refnum].[i])))
                |> getSpikeSnippets w_pre w_post endPadding |> List.toArray
            let spiketimes = spikes |> Array.map (fun (_,t) -> (uint64 t) + (uint64 curBlock)*(uint64 (blocksize)))
            let numspikes = Array.length spikes
            let s = Array3D.init numspikes (w_pre+w_post+1) (Array.length ds) 
                            (fun spikenum samplenum chindx -> 
                                let (s,_) = spikes.[spikenum]
                                int16 (s.[chindx,samplenum]))
            {Spikes=s;SpikeTimes=spiketimes;LFPs=(ds|>Array.map(fun (chnum,(a,b,xs),lfp) -> lfp))}
        )

let processAMPDataBlock hostname datafile byteoffset w_pre w_post channelgroups refchans blocksize endPadding curBlock =
    printfn "Starting Block %d" curBlock
    let b = getAMPBytes hostname datafile byteoffset blocksize endPadding curBlock |> Async.RunSynchronously
    printfn "Fetched Data!"
    printfn "Filtering Data"
    let tmp_data,ref = getRefData scaleDataOld channelgroups refchans endPadding blocksize b
    printfn "Snippeting"
    let data = extractSpikesAndLFPs (tmp_data,ref) w_pre w_post refchans blocksize endPadding curBlock
    printfn "Completed Block %d" curBlock
    {SpikesAndLFPs=data;Refs=ref|>Array.map (Array.map int16);AXL=null;VDD=null;Temp=null;TTLIns=null}

let processRHDDataBlock hostname datafile byteoffset w_pre w_post channelgroups refchans blocksize endPadding curBlock =
    printfn "Starting Block %d" curBlock
    let b = getRHDBytes hostname datafile byteoffset blocksize endPadding curBlock |> Async.RunSynchronously
    printfn "Fetched Data!"
    let aux = getAuxData endPadding blocksize b
    let vdd = aux.VDD
    printfn "Filtering Data"
    let tmp_data,ref = getRefData scaleData channelgroups refchans endPadding blocksize b
    printfn "Snippeting"
    let data = extractSpikesAndLFPs (tmp_data,ref) w_pre w_post refchans blocksize endPadding curBlock
    printfn "Completed Block %d" curBlock
    {aux with SpikesAndLFPs=data;Refs=ref|>Array.map (Array.map int16)}

let inline savefile hostname outpath data extract file offset = 
    let arr = data |> extract
    if arr <> null then writeArray hostname (sprintf @"%s/%s" outpath file) offset arr else async{return ()}

let SaveAux hostname outpath blocksize (curBlock,data) = 
    let baseoffset = (curBlock|>uint64)*(blocksize|>uint64)
    let savers = seq{
        yield savefile hostname outpath data (fun x -> x.VDD) "VDD" (baseoffset/60UL)
        yield savefile hostname outpath data (fun x -> x.Temp) "Temp" (baseoffset/60UL)
        yield savefile hostname outpath data (fun x -> x.AXL) "AXL" (baseoffset/4UL)
        yield savefile hostname outpath data (fun x -> x.TTLIns) "TTLIns" baseoffset
    }
    savers |> Seq.toArray |> Array.map (Async.StartAsTask)

let SaveRefs hostname outpath blocksize (curBlock,data) = 
    let baseoffset = (curBlock|>uint64)*(blocksize|>uint64)
    let savers = seq{
        for i in 0..data.Refs.Length-1 do
            yield savefile hostname outpath data (fun x -> x.Refs.[i]) (sprintf "Ref%d" i) baseoffset
    }
    savers |> Seq.toArray |> Array.map (Async.StartAsTask)

let SaveAll hostname outpath blocksize channelgroups curspikecounts (curBlock,data) =
    let waitaux = SaveAux hostname outpath blocksize (curBlock,data)
    let waitref = SaveRefs hostname outpath blocksize (curBlock,data)
    let baseoffset = (curBlock|>uint64)*(blocksize|>uint64)
    let savers = seq{
        for (i,(chname,chs),cnt) in Array.zip3 [|0..Array.length channelgroups-1|] channelgroups curspikecounts do
            let chstr = sprintf @"ChGroup_%s/%s" chname
            yield savefile hostname outpath data (fun x -> x.SpikesAndLFPs.[i].SpikeTimes) (chstr "SpikeTimes") cnt
            yield savefile hostname outpath data (fun x -> x.SpikesAndLFPs.[i].Spikes) (chstr "Spikes") cnt
            for j,(chnum,_) in Array.zip [|0..Array.length chs-1|] chs do
                yield savefile hostname outpath data (fun x -> x.SpikesAndLFPs.[i].LFPs.[j]) (sprintf "LFP/Ch_%d" chnum) (baseoffset/(dfact|>uint64))
    }
    let wait = savers |> Seq.toArray |> Array.map (Async.StartAsTask) |> Array.append waitaux |> Array.append waitref
    let r = 
        Array.zip channelgroups curspikecounts
        |> Array.mapi (fun i (_,cnt) -> 
            let numspikes = data.SpikesAndLFPs.[i].SpikeTimes|>Array.length
            cnt+(uint64 numspikes)
        )
    r,wait

let getTTLChanges hostname fname outpath endPadding (offset,count) =
    printfn "Starting"
    let ttlIns = getTTLIns hostname fname offset count
    printfn "Received Data"
    let changes = 
        ttlIns |> Seq.pairwise |> Seq.fold (fun (i,curchanges) (a,b) ->
            i+1UL,curchanges |> Array.mapi (fun ch chngs -> 
                if (a&&&(1us<<<ch)) <> (b&&&(1us<<<ch)) then i::chngs else chngs
            )
         ) (offset+(endPadding|>uint64), Array.init 16 (fun _ -> [])) |> snd |> Array.map List.rev

    printfn "Computed Changes"
    let save (state,curcounts) =
        let changes = 
            changes|> Array.mapi (fun ch chngs ->
                let i = offset+(endPadding|>uint64)-1UL
                let a = state
                let b = ttlIns.[0]
                if (a&&&(1us<<<ch)) <> (b&&&(1us<<<ch)) then i::chngs else chngs
            ) |> Array.map List.toArray
        curcounts |> Array.mapi (fun ch offset ->
            offset+(Array.length changes.[ch]|>uint64),savefile hostname outpath changes.[ch] id (sprintf "TTLChanges/Ch_%d" ch) offset
        )|>fun x -> (ttlIns.[count-1],x|>Array.map fst),x|>Array.map(snd>>Async.StartAsTask)
    save

open System.Xml.Linq

let snippet hostname settingsfile =
    let settings = 
        let bytes = getBytes hostname settingsfile 0UL -1 |> Async.RunSynchronously
        use ms = new MemoryStream(bytes)
        XElement.Load(ms)
    let byteoffset = 
        if (settings.Elements(xn "ByteOffset") |> Seq.length > 0) then
            settings.Element(xn "ByteOffset")|>uint64
        else 0UL
    let firstblock = settings.Element(xn "FirstBlock")|>int
    let lastblock = settings.Element(xn "LastBlock")|>int
    let blocksize = settings.Element(xn "BlockSize")|>int
    let endPadding = settings.Element(xn "EndPadding")|>int
            
    let w_pre = settings.Element(xn "WPre")|>int
    let w_post = settings.Element(xn "WPost")|>int
    let channelgroups = 
        let esets = loadnTrodes (settings.Element(xn "EIBElectrodePlacement"))
        let ts = loadThresholds (settings.Element(xn "Thresholds"))
        esets 
        |> Array.map (fun (s,channels) -> 
            (s,channels
                |>Array.map(fun channel -> channel,Map.find channel ts)))
                    
    let refchans =
        if (settings.Elements(xn "MedianReferenceGroups") |> Seq.length > 0) then
            settings.Element(xn "MedianReferenceGroups").Elements() |> Seq.map (fun x -> x.Elements() |> Seq.map int |> Seq.toArray) |> Seq.toArray
        else [|[|0..63|]|]
    let initstate = 
        settings.Element(xn "EIBElectrodePlacement").Elements()
        |> Seq.map (fun x -> 
            let a = x.Attributes(xn "Offset")|>Seq.toList
            match a with 
            | h::_ -> h|>uint64
            | _ -> 0UL
        ) |> Seq.toArray
    let blocks = [firstblock..lastblock] 
    let map processor datafile outpath blocknum = 
        let r = processor hostname datafile byteoffset w_pre w_post channelgroups refchans blocksize endPadding blocknum
        fun curspikecounts -> SaveAll hostname outpath blocksize channelgroups curspikecounts (blocknum,r)
    let worker,server = GetWorkerAndServer ()
    (fun endpoint -> 
        printfn "NumBlocks %d" (List.length blocks)
        printfn "%A" initstate
        server 30 initstate blocks endpoint |> printfn "%A"),
    (fun  processor datafile outpath endpoint workername -> worker (map processor datafile outpath) endpoint workername)

let aux hostname settingsfile =
    let settings = 
        let bytes = getBytes hostname settingsfile 0UL -1 |> Async.RunSynchronously
        use ms = new MemoryStream(bytes)
        XElement.Load(ms)
    let byteoffset = 
        if (settings.Elements(xn "ByteOffset") |> Seq.length > 0) then
            settings.Element(xn "ByteOffset")|>uint64
        else 0UL
    let firstblock = settings.Element(xn "FirstBlock")|>int
    let lastblock = settings.Element(xn "LastBlock")|>int
    let blocksize = settings.Element(xn "BlockSize")|>int
    let endPadding = settings.Element(xn "EndPadding")|>int
    let initstate = 0
    let blocks = [firstblock..lastblock]
    let map datafile outpath blocknum = 
        printfn "Starting Block %d" blocknum
        let b = getRHDBytes hostname datafile byteoffset blocksize endPadding blocknum |> Async.RunSynchronously
        printfn "Fetched Data!"
        let aux = getAuxData endPadding blocksize b
        fun i -> i+1,SaveAux hostname outpath blocksize (blocknum,aux)
    let worker,server = GetWorkerAndServer ()
    (fun endpoint -> 
        printfn "NumBlocks %d" (List.length blocks)
        server 30 initstate blocks endpoint |> printfn "%A"),
    (fun  datafile outpath endpoint workername -> worker (map datafile outpath) endpoint workername)
    
let ref hostname settingsfile =
    let settings = 
        let bytes = getBytes hostname settingsfile 0UL -1 |> Async.RunSynchronously
        use ms = new MemoryStream(bytes)
        XElement.Load(ms)
    let byteoffset = 
        if (settings.Elements(xn "ByteOffset") |> Seq.length > 0) then
            settings.Element(xn "ByteOffset")|>uint64
        else 0UL
    let firstblock = settings.Element(xn "FirstBlock")|>int
    let lastblock = settings.Element(xn "LastBlock")|>int
    let blocksize = settings.Element(xn "BlockSize")|>int
    let endPadding = settings.Element(xn "EndPadding")|>int
    let channelgroups = 
        let esets = loadnTrodes (settings.Element(xn "EIBElectrodePlacement"))
        let ts = loadThresholds (settings.Element(xn "Thresholds"))
        esets 
        |> Array.map (fun (s,channels) -> 
            (s,channels
                |>Array.map(fun channel -> channel,Map.find channel ts)))
                    
    let refchans =
        if (settings.Elements(xn "MedianReferenceGroups") |> Seq.length > 0) then
            settings.Element(xn "MedianReferenceGroups").Elements() |> Seq.map (fun x -> x.Elements() |> Seq.map int |> Seq.toArray) |> Seq.toArray
        else [|[|0..63|]|]
    let initstate = 0
    let blocks = [firstblock..lastblock]
    let map datafile outpath blocknum = 
        printfn "Starting Block %d" blocknum
        let b = getRHDBytes hostname datafile byteoffset blocksize endPadding blocknum |> Async.RunSynchronously
        printfn "Fetched Data!"
        let _,ref = getRefData scaleData channelgroups refchans endPadding blocksize b
        let x () = Array2D.zeroCreate 0 0
        fun i -> 
            i+1,SaveRefs hostname outpath blocksize (blocknum,
                {SpikesAndLFPs=[||];AXL=x ();VDD=x ();Temp=x ();TTLIns=[||];Refs=ref|>Array.map (Array.map int16)})
    let worker,server = GetWorkerAndServer ()
    (fun endpoint -> 
        printfn "NumBlocks %d" (List.length blocks)
        server 30 initstate blocks endpoint |> printfn "%A"),
    (fun  datafile outpath endpoint workername -> worker (map datafile outpath) endpoint workername)

let ttl hostname  =
    let endpadding = 3000
    let worker,server = GetWorkerAndServer ()
    (fun numsamples endpoint ->
        let numperread = 100000000UL //100 million
        let blocks = [0UL..numperread..numsamples-1UL]|>List.map (fun off -> off,(min numperread (numsamples-off))|>int) 
        printfn "NumBlocks %d" (List.length blocks)
        server 30 (0us,Array.init 16 (fun _ -> 0UL)) blocks endpoint |> printfn "%A"
    ),
    (fun datafile outpath endpoint workername ->
        worker (getTTLChanges hostname datafile outpath endpadding) endpoint workername
    )

let getamp hostname =
    let spikelen = 64
    let spikeampindx = 31
    let extractamp nchans (o,n) =
        getSpikes spikelen nchans 
    let worker,server = GetWorkerAndServer ()
    (fun numspikes endpoint ->
        let numperread = 1000000UL //1 million
        let blocks = [0UL..numperread..numspikes-1UL]|>List.map (fun off -> off,(min numperread (numspikes-off))|>int) 
        printfn "NumBlocks %d" (List.length blocks)
        server 30 0 blocks endpoint |> printfn "%A"
    ),
    (fun datafile nchans outfile endpoint workername ->
        worker 
            (fun (o,n) -> 
                let spikes = getSpikes spikelen nchans hostname datafile o n
                let samps = Array2D.init (Array3D.length1 spikes) nchans (fun i j -> spikes.[i,spikeampindx,j])
                writeArray hostname outfile o samps
                |> Async.RunSynchronously
                fun v -> v,[||]
            ) 
            endpoint workername
    )
    

let iotp () =
    let worker,server = GetWorkerAndServer()
    let endPadding = 3000
    let blockSize = 450000
    let getData hostname datafile blocknum = 
        let b = getRHDBytes hostname datafile 0UL blockSize endPadding blocknum |> Async.RunSynchronously
        fun i -> i+1,[||]
    (fun endpoint ->
        server 30 0 [0..999] endpoint |> printfn "%A"
    ),
    (fun hostname datafile endpoint workername ->
        worker (getData hostname datafile) endpoint workername
    )

[<EntryPoint>]
let main argv =
    match argv with  
    | [|"SnippetServer";hostname;settingsfile;endpoint;|] ->      
        let server,_ = snippet (Remote hostname) settingsfile
        server endpoint
        0
    | [|"SnippetWorker";hostname;settingsfile;endpoint;datafile;outpath;workername|] ->
        let _,worker = snippet (Remote hostname) settingsfile
        worker processRHDDataBlock datafile outpath endpoint workername
        0
    | [|"SnippetWorkerOld";hostname;settingsfile;endpoint;datafile;outpath;workername|] ->
        let _,worker = snippet (Remote hostname) settingsfile
        worker processAMPDataBlock datafile outpath endpoint workername
        0
    | [|"AmpServer";hostname;numspikes;endpoint;|] ->      
        let server,_ = getamp (Remote hostname)
        server (numspikes|>uint64) endpoint
        0
    | [|"AmpWorker";hostname;datafile;nchans;outfile;endpoint;workername|] ->
        let _,worker = getamp (Remote hostname)
        worker datafile (nchans|>int) outfile endpoint workername
        0
    | [|"AuxServer";hostname;settingsfile;endpoint;|] ->      
        let server,_ = aux (Remote hostname) settingsfile
        server endpoint
        0
    | [|"AuxWorker";hostname;settingsfile;endpoint;datafile;outpath;workername|] ->
        let _,worker = aux (Remote hostname) settingsfile
        worker datafile outpath endpoint workername
        0
    | [|"RefServer";hostname;settingsfile;endpoint;|] ->      
        let server,_ = ref (Remote hostname) settingsfile
        server endpoint
        0
    | [|"RefWorker";hostname;settingsfile;endpoint;datafile;outpath;workername|] ->
        let _,worker = ref (Remote hostname) settingsfile
        worker datafile outpath endpoint workername
        0
    | [|"TTLServer";hostname;endpoint;numsamples|] ->      
        let server,_ = ttl (Remote hostname)
        server (numsamples|>uint64) endpoint
        0
    | [|"TTLWorker";hostname;endpoint;datafile;outpath;workername|] ->
        let _,worker = ttl (Remote hostname)
        worker datafile outpath endpoint workername
        0
    | [|"IOTPServer";endpoint;|]->
        let server,_ = iotp ()
        server endpoint
        0
    | [|"IOTPWorker";hostname;datafile;endpoint;workername|] ->
        let _,worker = iotp ()
        worker (Remote hostname) datafile endpoint workername
        0
    | _ -> 
        printfn "%A" argv
        failwith "Invalid arguments"
