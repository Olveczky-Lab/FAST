module SortingHelper

open System.IO
open RP.HDF5.H5TypeProviderRuntime

let getfnames numspikes startblocknum count =
    let ntotal = Array.get numspikes (Array.length numspikes - 1) |> snd
    let islast,numtoread =
            let r = ntotal - startblocknum
            if count <= r then false,count else true,r
    let findpos blocknum =
        numspikes |> Array.tryFindIndex (fun (_,offset) ->
            offset > blocknum
        )
    let f i o n =
        let (spikesfname,clusfname),_ = numspikes.[i]
        let co = 
            if i=0 then o else o-(numspikes.[i-1]|>snd)
        spikesfname,clusfname,(co,n)
    match findpos startblocknum, findpos (startblocknum+numtoread-1) with
    | Some a, Some b when a=b -> 
        [|f a startblocknum numtoread|],islast
    | Some a, Some b when b=a+1 -> 
        let o = numspikes.[a] |> snd
        let n = o-startblocknum
        [|f a startblocknum n;f b o (numtoread-n)|],islast        
    | _ -> failwith "Requesting Nonexistent Data"    

let loadnumspikes fnames chnum numperclustering = 
    fnames |> Array.scan (fun offset  (spikesfname,clusfname) ->
        let newoffset = 
            offset+((getDataSetDimensions clusfname 
                        (sprintf "/ChGroup_%d/ClusteredSpikes" chnum)).[0]/(numperclustering|>uint64)|>int) 
        newoffset
    ) 0
    |> fun x -> Array.zip fnames x.[1..]

open SegmentationFusion

let serialize fname cs =
    use fs = new FileStream(fname,FileMode.Create)
    use bw = new BinaryWriter(fs)
    bw.Write(List.length cs)
    for c in cs do
        bw.Write(List.length c)
        for (i:int,clu) in c do
        bw.Write(i)
        bw.Write(clu.Temp)
        bw.Write(clu.Cluster)
        bw.Write(clu.Count)
        bw.Write(clu.Stability)
        bw.Write(clu.Amplitude)
        bw.Write(Array.length clu.Shape)
        for s in clu.Shape do
            bw.Write(s)

let deserialize fname = 
    use fs = new FileStream(fname,FileMode.Open)
    use br = new BinaryReader(fs)
    List.init (br.ReadInt32()) (fun _ ->
        List.init (br.ReadInt32()) (fun _ ->
            (br.ReadInt32(),
                {
                    Temp=br.ReadInt32();
                    Cluster=br.ReadUInt16();
                    Count=br.ReadInt32();
                    Stability=br.ReadInt32();
                    Amplitude=br.ReadDouble();
                    Shape = Array.init (br.ReadInt32()) (fun _ -> br.ReadDouble())
                })
        )
    )

let dispchain c =
    c |> Seq.iter (fun (i,clu) -> printfn "%d\t%d\t%d\t%d\t%5.1f" i clu.Count clu.Temp clu.Stability clu.Amplitude)
