module RemoveNoise

open RP.HDF5.H5TypeProviderRuntime 
open HDF5Helper
open System

let getCorrelationCount x ys =
    let getCorrelated s1 s2 =
        let rec r xs t =
            match xs with
            | head::tail -> 
                match head with
                | _ when abs (head - t) <= 1L -> true, tail
                | _ when head > t -> false, xs
                | _ -> r tail t
            | _ -> false, []
        s1 
        |> List.fold (fun (c,xs) t ->
                            let newc,newxs = r xs t
                            newc::c,newxs) ([],s2)
        |> fst            
    ys
    |> Array.Parallel.map (getCorrelated x)
    |> fun c -> 
        List.fold (fun (counts,cs) _ ->
                        (cs |> Array.map (List.head >> (fun x -> if x then 1 else 0)) |> Seq.sum) :: counts,
                        cs |> Array.map (List.tail)) ([],c) x
    |> fst

let getCorrelationCountAll xs =
    let n = Array.length xs
    let inline removeAt i =
        Array.init (n-1) (fun j -> xs.[if (j < i) then j else j+1]) 
    xs |> Array.Parallel.mapi (fun i x -> 
            printfn "%d" i
            getCorrelationCount x (removeAt i) |> List.toArray)

let saveNonNoise fname channelgroups s0 s1 =
    let spikeindices =
        channelgroups |> Array.map (fun chgroup ->
                            chgroup, Option.get <| getSpikesIndex s0 s1 fname chgroup)
    let spiketimes =
        spikeindices |> Array.map (fun (chgroup,(spikeoffset,spikecount)) -> 
                            printfn "Reading %d spikes" spikecount 
                            readData fname ("./ChGroup_" + chgroup + "/SpikeTimes") [|spikeoffset|] null [|spikecount|] :?> int64[]
                            |> Array.toList)
    getCorrelationCountAll spiketimes
    |> Array.zip channelgroups
    |> Array.iter (fun (chgroup,ccount) -> 
                        printfn "%s %A" chgroup (Seq.countBy id ccount)
                        savehdf5 fname (sprintf "./ChGroup_%s/CCount%d" chgroup s0) ccount)

let inline loadNonNoise fname chgroup s0 =
    readData fname (sprintf "./ChGroup_%s/CCount%d" chgroup s0) null null null :?> int[]