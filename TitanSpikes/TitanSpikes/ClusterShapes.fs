#if COMPILED
module ClusterShapes
#else
#time "on"
fsi.ShowDeclarationValues <- false
#endif

open Helper
open Nessos.FsPickler
open System.IO
open System.Xml.Linq

let userpathlocal = @"Z:\"
let userpathremote = @"/root/data/rpoddar"
let rat = "badlands"
let fnum = "635276396558304920"
let hostname = Remote "140.247.178.16:8080"
let esets = loadnTrodes (XElement.Load(sprintf @"%s\%s\%s\SnippeterSettings.xml" userpathlocal rat fnum).Element(xn "EIBElectrodePlacement"))

for chgroup,chans in esets do
    let nchans = Array.length chans
    let spikelen = 64
    let ntemp = 15
    let numperblock = 1000
    let numperread = 100000UL

    let minclussize = 15
    let minclusamp = 50.

    let nf = nchans*spikelen

    let nspikes = FileInfo(sprintf @"%s\%s\%s\ChGroup_%s\SpikeTimes" userpathlocal rat fnum chgroup).Length/8L|>uint64
    Directory.CreateDirectory(sprintf @"%s\%s\%s\ChGroup_%s\Level2" userpathlocal rat fnum chgroup)|>ignore

    printfn "%s %d" chgroup nspikes

    let sfname,stfname,clusfname,l2sfname,l2stfname,l2cfname,l2clusfname = 
        let fname = sprintf @"%s/%s/%s/ChGroup_%s/%s" userpathremote rat fnum chgroup
        fname @"Spikes", fname @"SpikeTimes", (sprintf "%s%s.large" (fname "Clusters")), fname @"Level2/Spikes", 
        fname @"Level2/SpikeTimes", fname @"Level2/ClusterSpikeNums", fname @"Level2/Clusters"
    
    let blocklevels = [|clusfname "1",1;clusfname "2",1;clusfname "3",1;clusfname "4",1|]
    
    let inline updateClusShape maxindx curoffset (curspikes:int16[,]) n (curshape,spikenums) =
        let cur,next = spikenums |> Array.partition (fun a -> a < maxindx)
        cur |> Array.fold (fun curshape spikenum -> 
            Array.mapi (fun i x -> x+(curspikes.[spikenum-curoffset|>int,i]|>float)/n) curshape) curshape,
        next

    let inline processBlockLevel maxindx curoffset (curspikes:int16[,]) (prevdone,prevclus) =
        prevclus
        |> List.map (fun ((clu,n),(curshape,spikenums)) ->
            (clu,n),updateClusShape maxindx curoffset (curspikes:int16[,]) n (curshape,spikenums)            
        )
        |> List.fold (fun (curdone,curclus) (((clu,_),(shape,xs)) as cur) -> 
            if Array.length xs = 0 then (clu,(shape|>Array.map int16))::curdone,curclus
            else curdone,cur::curclus
        ) (prevdone,[])

    let rec processCurData (fname,numblocks) maxindx curoffset (curspikes:int16[,]) (blocknum,x) =
        let newblocknum,x =
            if blocknum < numblocks then 
                let largeclus = 
                    let bytes = getBytes hostname fname 0UL -1|>Async.RunSynchronously
                    use ms = new MemoryStream(bytes)
                    let fsp = new FsPickler()
                    Seq.unfold (fun _ -> 
                        if ms.Position >= ms.Length then None else
                        (fsp.Deserialize<uint64[]>(ms),())|>Some) ()
                    |> Seq.toList
                let prevdone,prevclus = x
                let allclus = 
                    List.append prevclus (largeclus |> List.map (fun xs ->
                        let n = Array.length xs
                        let subxs = 
                            if n > minclussize then 
                                Array.init minclussize (fun i -> xs.[i*n/minclussize])
                            else xs
                        ((xs,xs.[n/2]),Array.length subxs|>float),(Array.zeroCreate<float> nf,subxs)    
                    ))
                blocknum+1,(prevdone,allclus)
            else blocknum,x
        let (newdone,newclus) as y = processBlockLevel maxindx curoffset (curspikes:int16[,]) x
        if List.length newclus = 0 && newblocknum>blocknum then
            processCurData (fname,numblocks) maxindx curoffset (curspikes:int16[,]) (newblocknum,y)
        else (newblocknum,y)

    let numnewspikes,_,_ = 
        let fsp = new FsPickler()
        [|0UL..numperread..nspikes-1UL|]
        |> Array.map (fun x -> x,min numperread (nspikes-x+1UL))
        |> Array.fold (fun (n,byteoffset,levels) (curoffset,numtoread) ->
            printfn "%d" curoffset 
            let curspikes = getArray2D<int16> nf hostname sfname curoffset (numtoread|>int)
            let maxindx = curoffset + (Array2D.length1 curspikes|>uint64)
            let x = levels |> Array.mapi (fun i x -> processCurData blocklevels.[i] maxindx curoffset (curspikes:int16[,]) x)
            let minx = 
                x |> Array.map (snd>>snd>>(List.map (fst>>fst>>snd))>>List.toArray) |> Array.concat |> (fun xs -> 
                    match Array.length xs with
                    | 0 -> maxindx
                    | _ -> xs |> Array.min
                )
            x |> Array.map (fun (b,(xs,ys)) -> 
                let xs1,xs2 = 
                    xs |> List.partition (fun ((_,t),_) -> t < minx)
                xs1,(b,(xs2,ys))
            ) 
            |> fun x ->
                let towrite = 
                    x |> Array.map (fst>>List.toArray) |> Array.concat |> Array.sortBy (fst>>snd)
                    |> Array.filter (fun (_,shape) -> 
                        shape.[31*nchans..32*nchans-1]|>Array.map abs|>Array.max|>float|>fun x -> x*0.195>minclusamp
                    )
                writeArray hostname l2sfname n (Array2D.init (Array.length towrite) nf (fun i j -> (towrite.[i]|>snd).[j])) |> Async.RunSynchronously
                writeArray hostname l2stfname n (towrite |> Array.map (fst>>snd)) |> Async.RunSynchronously
                use ms = new MemoryStream()
                for (c,_),_ in towrite do
                    fsp.Serialize(ms,c)|>ignore
                let bytes = ms.ToArray()
                writeArray hostname l2cfname byteoffset bytes |> Async.RunSynchronously
                n+(Array.length towrite|>uint64),byteoffset+(Array.length bytes|>uint64),x |> Array.map snd
        ) (0UL,0UL,blocklevels |> Array.map (fun x -> 0,([],[])))

    let numnewblocks = numnewspikes/1000UL|>int
    writeArray hostname (sprintf "%s.indx" l2clusfname) 0UL [|0UL..(numnewblocks*numperblock-1|>uint64)|] |> Async.RunSynchronously
    printfn "TotaBlocks: %d" numnewblocks