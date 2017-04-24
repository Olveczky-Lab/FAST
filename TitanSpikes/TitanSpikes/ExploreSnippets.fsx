#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\HDF5IO\bin\Debug\HDF5IO.dll"

open RP.HDF5

[<Literal>]
let fname = @"Z:\arches\test.h5"

type h5file = HDF5File<fname>
h5file.ChGroup_13.SpikeTimes.GetDimensions()
