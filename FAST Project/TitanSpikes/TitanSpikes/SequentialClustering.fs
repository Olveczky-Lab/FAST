module SequentialClustering

open Cluster
open ClusterHelper

let seqclus mintemp maxtemp tempstep spikesraw spikestocluster =
    let minstab = 3
    let nchans = Array3D.length3 spikesraw
    let features curindx = 
        let waveletfeatures = getFeatures spikesraw curindx [|getwavelets (Array.init nchans id) 64|]
        let ampfeatures = getFeatures spikesraw curindx [|getspikeamp(Array.init nchans (fun i -> (31,i)))>>(Array.map (fun x -> x*2.0))|]
        Array.zip (waveletfeatures|> cullfeaturesks 40) ampfeatures |> Array.map (fun (x,y) -> Array.append x y)
    let rec clusloop curindx = seq {
        if (Array.length curindx < 12) then ()
        else
            let clus = wave_clus mintemp maxtemp tempstep 300 11 0 
                        @"C:\temp\temp" (features curindx)
                       |> Array.zip curindx
            let stableclusters = getStableClusters 8 0.95 clus |> Seq.filter (fun (_,xs) -> List.length xs > minstab) 
                                 |>  Seq.toList
            match stableclusters with
            | [head] ->
                yield curindx
            | head::tail ->
                let stabletemp,stableclus = 
                    tail |> Seq.maxBy (fun (tempnum,xs) -> (List.length xs))
                    |> (fun (tempnum,xs) -> tempnum,(List.head xs|>fst))
                let curindx1,curindx2 = 
                    clus |> Array.partition (fun (_,xs) -> xs.[stabletemp] = stableclus)
                printfn "%d\t%d" (Array.length curindx1) (Array.length curindx2)
                yield! clusloop (curindx1|>Array.map fst)
                yield! clusloop (curindx2|>Array.map fst)
            | _ -> failwith "Not possible"
    }
    clusloop spikestocluster

