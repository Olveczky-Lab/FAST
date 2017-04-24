module Helper

open System
open System.Xml.Linq

let rnd = new Random()
let clamp min max x = if (x < min) then min else if (x > max) then max else x

let xn n = XName.op_Implicit(n)
let loadnTrodes (fname:string) =
    XElement.Load(fname).Elements(xn "ElectrodeSet")
    |> Seq.map (fun x -> (x.Attribute(xn "Name").Value,x.Elements(xn "Channel")
                                                       |> Seq.map (int) |> Seq.toList))
    |> Seq.toList
    
let getOrderOfMag x =
    if (x = 0.0) then (x,"") else
    let order = clamp -5 3 (int (floor ((log10 (abs x))/3.0)))
    let prefix =  
        match order with
        | x when x = 3 -> "T"
        | x when x = 2 -> "G"
        | x when x = 1 -> "k"
        | x when x = 0 -> ""
        | x when x = -1 -> "m"
        | x when x = -2 -> Char.ConvertFromUtf32(956)
        | x when x = -3 -> "n"
        | x when x = -4 -> "p"
        | x when x = -5 -> "f"
        | _ -> failwith "Not Possible"
    1.0/(10.0**(float (3*order))), prefix


let getOrderOfMagTime t = //t is in seconds
    let at = abs t
    if (abs t < 24.0*60.0*60.0) then
        if (abs t < 60.0*60.0) then 
            if (abs t < 60.0) then
                if (abs t  < 1.0) then
                    let multiplier,s = getOrderOfMag t
                    multiplier,s+"s"
                else 1.0,"s"
            else 1.0/60.0,"m"
        else 1.0/60.0/60.0,"h"
    else 1.0/24.0/60.0/60.0,"d"


let (|Zero|Pos|Neg|) x = if (x = 0) then Zero else (if (x < 0) then Neg else Pos)

let intersect comparef l1 l2 =
    let rec rintersect xs ys xs_com xs_diff ys_com ys_diff = 
        match xs with
        | [] -> (xs_com,xs_diff,ys_com,ys@ys_diff)
        | hx::tx -> match ys with
                      | [] -> (xs_com,xs@xs_diff,ys_com,ys_diff)
                      | hy::ty -> match comparef hx hy with
                                    | Zero -> rintersect tx ty (hx::xs_com) xs_diff (hy::ys_com) ys_diff
                                    | Neg -> rintersect tx ys xs_com (hx::xs_diff) ys_com ys_diff
                                    | Pos -> rintersect xs ty xs_com xs_diff ys_com (hy::ys_diff)
    let (a,b,c,d) = rintersect l1 l2 [] [] [] []
    (a |> List.rev, b|> List.rev, c|> List.rev, d |> List.rev)

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

let scalespike (st:int64,s:int16[]) = (float st)/30000.0, s |> Array.map (fun s -> (float s)*0.195e-6)

let convertToUInt16 blocksize b chnum =
    Array.init blocksize (fun i -> BitConverter.ToUInt16(b,chnum*2+i*64*2))

let scaleData offset blocksize b chnum =
    convertToUInt16 blocksize b chnum
    |> Array.map (fun x -> (float x) - offset)

