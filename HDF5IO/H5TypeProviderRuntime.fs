namespace RP.HDF5

module H5TypeProviderRuntime =
    let getDataSetDimensions filename path = 
        use file = new H5File(filename,FileOpen(false),None)
        use ds = file.OpenDataSet(path)
        use space = ds.SpaceOpen()
        space.GetDims() |> fst        

    let readData filename path (starts:uint64[]) (strides:uint64[]) (counts:uint64[]) =
        use file = new H5File(filename,FileOpen(false),None)
        use ds = file.OpenDataSet(path)
        ds.ReadData(starts,strides,counts)

    let writeData filename path (starts:uint64[]) (strides:uint64[])  (data:System.Array) =     
        use file = new H5File(filename,FileOpen(true),None)
        use ds = file.OpenDataSet(path)
        ds.WriteData(starts,strides,data)
    

