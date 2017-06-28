module ClusterHelper

open System.IO
open Helper

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
