open System.IO
open System.Text.RegularExpressions
open System

let fpath = @"Y:\Data\Jaunpuri"
let fnums = 
    Directory.GetDirectories(fpath)
    |>Array.filter (fun x -> Regex.IsMatch(x,@"\\[0-9]+$"))
    |>Array.map (fun x -> DirectoryInfo(x).Name|>int64) 
    |> Array.sort
let duration = 
    let first,last = fnums.[0],fnums.[Array.length fnums-1]
    DateTime(last) - DateTime(first) + TimeSpan.FromSeconds((FileInfo(sprintf "%s\%d\TTLIns" fpath last).Length/2L|>float)/30000.)
    |> fun x -> x.TotalDays

let nblocks = 
    fnums
    |> Array.map (fun fnum ->
        Array.init 16 (fun i -> FileInfo(sprintf @"%s\%d\ChGroup_%d\SpikeTimes" fpath fnum i).Length/8L/1000L)
    )
