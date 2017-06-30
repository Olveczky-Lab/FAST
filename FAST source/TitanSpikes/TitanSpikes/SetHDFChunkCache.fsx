#r @"D:\Rajesh\Ephys\Data Analysis\FSharp\HDF5IO\bin\Release\HDF5IO.dll"
#time "on"

open RP.HDF5

let data =
    use file = new H5File(@"Z:\badlands\635276396558304920_0.h5",FileOpen(true),None)
    use dapl = new H5DataSetAccessPropList()
    dapl.SetChunkCache(521,200*1024*1024,1.0) |> ignore
    use dset = new H5DataSet(file,"./ChGroup_5/Spikes",DataSetOpen,Some dapl)
    dset.ReadData(null,null,[|1000000UL;64UL;4UL|]) :?> int16[,,]
