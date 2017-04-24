module DynProgAlign

open System.Collections.Generic

type ContinuationBuilder() = 
    member this.Return(x) = (fun k -> k x) 
    member this.ReturnFrom(x) = x 
    member this.Bind(m,f) = (fun k -> m (fun a -> f a k)) 
    member this.Delay(f) = (fun k -> f () k) 
let K = new ContinuationBuilder()

let inline align stop cost gapcostx gapcosty nx ny =
    let cache = Dictionary<_,_>()
    let rec aligncost ((ix,iy) as i) = K {
        match cache.TryGetValue(i) with
        | true, r -> return r
        | _ ->
            let! c = K {
                match (ix,iy) with
                | _ when ix = nx -> 
                    return Some ([iy..ny-1]|>Seq.choose gapcosty|>Seq.sum,
                        ([iy..ny-1]|>List.map (fun indx -> (None,Some indx))))
                | _ when iy = ny -> 
                    return Some ([ix..nx-1]|>Seq.choose gapcostx|>Seq.sum,
                        ([ix..nx-1]|>List.map (fun indx -> (Some indx,None))))
                | _ ->
                    let sx,sy = stop ix iy
                    let! c1 = K {         
                        if sx || sy then return None else               
                        match  cost ix iy with
                        | None -> return None
                        | Some c ->
                            let! c1tmp = aligncost (ix+1,iy+1)                    
                            return c1tmp |> Option.map (fun (cold,gold) -> c + cold,(Some ix,Some iy)::gold)                            
                    }
                    let! c2 = K {
                        if sy then return None else
                        match gapcosty iy with
                        | None -> return None
                        | Some c ->
                            let! c2tmp = aligncost (ix,iy+1)
                            return c2tmp |> Option.map (fun (cold,gold) -> c + cold,((None,Some iy)::gold))
                    }
                    let! c3 = K {
                        if sx then return None else
                        match gapcostx ix with
                        | None -> return None
                        | Some c ->
                            let! c3tmp = aligncost (ix+1,iy)
                            return c3tmp |> Option.map (fun (cold,gold) -> c + cold,((Some ix,None)::gold))
                    }
                    let c = [c1;c2;c3] |> List.choose id
                    if List.isEmpty c then return None else
                    let best = (List.minBy fst c)
                    return Some best
            }
            cache.Add(i,c)
            return c
    }
    aligncost (0,0) id
