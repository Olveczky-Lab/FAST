open System.IO
let rat = "Dhanashri"
for fnum in [|"635400060049541397";"635423311368602679"|] do
    for i in 0..15 do
        let files = Directory.GetFiles(sprintf @"Y:\Data\%s\%s\ChGroup_%d\MergedChains" rat fnum i)
        Directory.CreateDirectory(sprintf @"I:\%s\%s\ChGroup_%d\MergedChains" rat fnum i) |> ignore
        for file in files do
            File.Copy(file,sprintf @"I:\%s\%s\ChGroup_%d\MergedChains\%s" rat fnum i (FileInfo(file).Name))