module HDF5MatlabHelper

open System
open System.IO
open RP.HDF5

let savehdf5 fname varname (data:Array) =
    use file =
        if (File.Exists(fname)) then
            new H5File(fname,FileOpen(true),None)
        else
            new H5File(fname,FileCreate(true,None),None)
    use ds =
        try
            file.DeleteDataSet(varname) |> ignore
        with
            | _ -> ()
        let dims = 
            let ndims = data.GetType().GetArrayRank()
            Array.init ndims (fun i -> 
                                let n = data.GetLongLength(i) |> uint64
                                (n,Some n,n))
        file.CreateDataSet(varname,DataTypeClass.FromDotNetType(data),dims)
    ds.WriteData(null,null,data)
