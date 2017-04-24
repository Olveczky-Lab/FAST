namespace RP.HDF5

open H5Externs
open System
open System.Runtime.InteropServices

type DataTypeClass =
| H5_UInt8
| H5_UInt16
| H5_UInt32
| H5_UInt64
| H5_Int8
| H5_Int16
| H5_Int32
| H5_Int64
| H5_Float32
| H5_Float64
    with 
    member x.GetID() =
        match x with
        | H5_UInt8 -> const_08u_g.Value
        | H5_UInt16 -> const_16u_g.Value
        | H5_UInt32 -> const_32u_g.Value
        | H5_UInt64 -> const_64u_g.Value
        | H5_Int8 -> const_8s_g.Value
        | H5_Int16 -> const_16s_g.Value
        | H5_Int32 -> const_32s_g.Value
        | H5_Int64 -> const_64s_g.Value
        | H5_Float32 -> const_IEEE_F32LE_g.Value
        | H5_Float64 -> const_IEEE_F64LE_g.Value
    static member FromDotNetType(x:Array) =
        match (x.GetType().GetElementType()) with
        | t when t=typeof<byte> -> H5_UInt8
        | t when t=typeof<uint16> -> H5_UInt16
        | t when t=typeof<uint32> -> H5_UInt32
        | t when t=typeof<uint64> -> H5_UInt64
        | t when t=typeof<sbyte> -> H5_Int8
        | t when t=typeof<int16> -> H5_Int16
        | t when t=typeof<int32> -> H5_Int32
        | t when t=typeof<int64> -> H5_Int64
        | t when t=typeof<float32> -> H5_Float32
        | t when t=typeof<float> -> H5_Float64
        | _ -> failwith "Invalid Type"

type FileInitParams =
| FileCreate of bool*(H5FileCreatePropList option) // empty pre-existing file?
| FileOpen of bool // enable writes?

type DataSetInitParams =
| DataSetCreate of H5Type*H5Space*(H5LinkCreatePropList option)*(H5DataSetCreatePropList option)
| DataSetOpen

and H5DataSet(file:H5File,dsname,initparams,dapl:(H5DataSetAccessPropList option)) =
    let daplid = 
        match dapl with
        | Some x -> x.GetID()
        | None -> H5P_DEFAULT
    let datasetid =        
        match initparams with
        | DataSetCreate(dtype,space,lcpl,dcpl) -> 
            let dcplid = 
                match dcpl with
                | Some x -> x.GetID()
                | None -> H5P_DEFAULT
            let lcplid = 
                match lcpl with
                | Some x -> x.GetID()
                | None -> H5P_DEFAULT
            H5DCreate2(file.GetID(),dsname,
                dtype.GetID(),space.GetID(),lcplid,
                dcplid,daplid)
            |> checkpositive "H5DataSet:Create"
        | DataSetOpen -> 
            let daplid = 
                match dapl with
                | Some x -> x.GetID()
                | None -> H5P_DEFAULT
            H5DOpen2(file.GetID(),dsname,daplid)
            |> checkpositive "H5DataSet:Open"
    member x.GetID() = datasetid
    member x.SpaceOpen() =
        new H5Space(SpaceOpen(x))
    member x.TypeOpen() =
        new H5Type(TypeOpen(x))
    member x.Extend(newdims) =
        H5DSetExtent (datasetid,newdims)
        |> checknonneg "DataSetExtend" |> ignore
    member x.ReadData((starts:uint64[]),(strides:uint64[]),(counts:uint64[])) = 
        use space = x.SpaceOpen()
        let (dims,_) = space.GetDims()
        use dtype = x.TypeOpen()
        match (dtype.GetDotNetType()) with
        | Some typ ->
            let corrected_starts = if (starts = null) then Array.zeroCreate (Array.length dims) else starts
            let corrected_counts = if (counts = null) then dims else counts
            space.SelectHyperSlab(H5S_SelOper.H5S_SELECT_SET,corrected_starts,strides,corrected_counts,null)
            let data = System.Array.CreateInstance(typ,corrected_counts |> Array.map int64)        
            use memspace = new H5Space(SpaceCreate(corrected_counts |> Array.map (fun x -> (x,Some x))))
            let hnd = GCHandle.Alloc(data,GCHandleType.Pinned)
            let pntr = hnd.AddrOfPinnedObject()
            H5DRead (datasetid,dtype.GetID(),memspace.GetID(),space.GetID(),H5P_DEFAULT,pntr)
            |> checknonneg "ReadArray"
            |> ignore
            hnd.Free()
            data
        | _ -> failwith "Wrong type"
    member x.WriteData((starts:uint64[]),(strides:uint64[]),(data:System.Array)) =
        let (dims,_) = 
            use space = x.SpaceOpen()
            space.GetDims()
        use dtype = x.TypeOpen()
        match (dtype.GetDotNetType()) with
        | Some typ ->
            let corrected_starts = if (starts = null) then Array.zeroCreate (Array.length dims) else starts
            let counts = Array.init (data.Rank) (fun i -> uint64 (data.GetLongLength(i)))
            let newdims = Array.init (data.Rank) (fun i -> max (counts.[i]+corrected_starts.[i]) dims.[i])
            x.Extend(newdims)
            use space = x.SpaceOpen()
            space.SelectHyperSlab(H5S_SelOper.H5S_SELECT_SET,corrected_starts,strides,counts,null)
            use memspace = new H5Space(SpaceCreate(counts |> Array.map (fun x -> (x,Some x))))
            let hnd = GCHandle.Alloc(data,GCHandleType.Pinned)
            let pntr = hnd.AddrOfPinnedObject()
            H5DWrite (datasetid,dtype.GetID(),memspace.GetID(),space.GetID(),H5P_DEFAULT,pntr)
            |> checknonneg "WriteArray"
            |> ignore
            hnd.Free()
        | _ -> failwith "Wrong type"
    interface IDisposable with        
        member x.Dispose() = 
            H5DClose(datasetid) |> checknonneg "H5DataSet:Close" |> ignore
            GC.SuppressFinalize(x)
    override x.Finalize() = (x:>IDisposable).Dispose()

and SpaceInitParams =
| SpaceCreate of (uint64*uint64 option)[]
| SpaceOpen of H5DataSet

and H5Space(initparams) = 
    let spaceid =
        match initparams with
        | SpaceCreate (dims) ->             
            H5SCreateSimple(Array.length dims, dims |> Array.map fst, 
                dims |> Array.map (fun (n,nmax) -> match nmax with 
                                                   | Some x -> max x n
                                                   | None -> -1|>uint64))
            |> checkpositive "H5Space:Create"
        | SpaceOpen (dset) -> H5DGetSpace(dset.GetID()) |> checkpositive "H5Space:Open"
    member x.GetID() = spaceid
    member x.GetNDims() =
        H5SGetSimpleNDims (spaceid)
        |> checknonneg "SpaceGetNDims"
    member x.GetDims() =
        let numberofdims = x.GetNDims()
        let currentcontent = Array.zeroCreate<uint64> numberofdims
        let maximalcapacity = Array.zeroCreate<uint64> numberofdims
        H5SGetSimpleDims (spaceid, currentcontent, maximalcapacity)
        |> checknonneg "SpaceGetDims"
        |> ignore
        (currentcontent, maximalcapacity)
    member x.SelectHyperSlab(seloper,starts,strides,counts,blocks) =
        H5SSelect_hyperslab (spaceid, seloper, starts, strides, counts, blocks)
        |> checknonneg "SelectHyperSlab"
        |> ignore
    interface IDisposable with
        member x.Dispose() = 
            H5SClose(spaceid) |> checknonneg "H5Space:Close" |> ignore
            GC.SuppressFinalize(x)
    override x.Finalize() = (x:>IDisposable).Dispose()
    
and TypeInitParams =
| TypeCreate of DataTypeClass
| TypeOpen of H5DataSet

and H5Type(initparams) =
    let typeid =
        match initparams with
        | TypeCreate (typeclass) -> H5TCopy(typeclass.GetID())
                                    |> checkpositive "H5Type:Create"
        | TypeOpen (dset) -> H5DGetType(dset.GetID()) |> checkpositive "H5Type:Open"
    member x.GetID() = typeid
    member x.GetClass() = 
        let classtype =
            H5TGetClass (typeid)
            |> checknonneg "DataTypeClass"
            |> enum<H5T_Class>
        if Enum.IsDefined(typeof<H5T_Class>, classtype)
        then classtype
        else failwithf "Failure; Routine name = 'DataTypeClass'; Data type class %d is unknown." (int classtype)
    member x.GetSize() =
        H5TGetSize (typeid)
        |> checkpositive "DataTypeSize"
    member x.GetSign() =
        let signtype =
            H5TGetSign (typeid)
            |> checknonneg "DataTypeSign"
            |> enum<H5T_Sign>
        if Enum.IsDefined(typeof<H5T_Sign>, signtype)
        then signtype
        else failwithf "Failure; Routine name = 'DataTypeSign'; Data type sign %d is unknown." (int signtype)
    member x.GetDotNetType() =
        match x.GetClass() with
        | H5T_Class.H5T_INTEGER ->
            match (x.GetSize(), x.GetSign()) with
            | (1,H5T_Sign.H5T_SGN_2) -> Some typeof<sbyte>
            | (1,H5T_Sign.H5T_SGN_NONE) -> Some typeof<byte>
            | (2,H5T_Sign.H5T_SGN_2) -> Some typeof<int16>
            | (2,H5T_Sign.H5T_SGN_NONE) -> Some typeof<uint16>
            | (4,H5T_Sign.H5T_SGN_2) -> Some typeof<int>
            | (4,H5T_Sign.H5T_SGN_NONE) -> Some typeof<uint32>
            | (8,H5T_Sign.H5T_SGN_2) -> Some typeof<int64>
            | (8,H5T_Sign.H5T_SGN_NONE) -> Some typeof<uint64>
            | _ -> None
        | H5T_Class.H5T_FLOAT ->
            match x.GetSize() with
            | 4 -> Some typeof<float32>
            | 8 -> Some typeof<float>
            | _ -> None
        | _ -> None
    interface IDisposable with
        member x.Dispose() = 
            H5TClose(typeid) |> checknonneg "H5Type:Close" |> ignore
            GC.SuppressFinalize(x)
    override x.Finalize() = (x:>IDisposable).Dispose()

and H5File(filename,initparams,fapl:H5FileAccessPropList option) =
    let fileid =
        let faplid = 
            match fapl with
            | Some x -> x.GetID()
            | None -> H5P_DEFAULT
        match initparams with
        | FileCreate(trunc,x) -> 
            let fcplid = 
                match fapl with
                | Some x -> x.GetID()
                | None -> H5P_DEFAULT
            H5FCreate(filename,(if trunc then FileAccess.ACC_TRUNC else FileAccess.ACC_EXCL) |> uint32,fcplid,faplid)
            |> checkpositive "H5File:Create"
        | FileOpen(x) -> 
            H5FOpen(filename,(if x then FileAccess.ACC_RDWR else FileAccess.ACC_RDONLY) |> uint32,faplid)
            |> checkpositive "H5File:Create"
    
    member x.GetID() = fileid
    member x.LinDelete(path) = H5LDelete(fileid,path,0)
    member x.CreateDataSet(path,datatype,dims) =
        use dtype = new H5Type(TypeCreate(datatype))
        use space = new H5Space(SpaceCreate(dims|>Array.map (fun (curdim,maxdim,_) -> (curdim,maxdim))))
        use lcpl = new H5LinkCreatePropList()
        lcpl.SetCreateIntermediateGroups()
        use dcpl = new H5DataSetCreatePropList()
        dcpl.SetChunk(dims|> Array.map (fun (_,_,chunkdim) -> chunkdim))
        new H5DataSet(x,path,DataSetCreate(dtype,space,Some lcpl,Some dcpl),None)
    member x.OpenDataSet(path) =
        new H5DataSet(x,path,DataSetOpen,None)
    member x.DeleteDataSet(path) =
        x.LinDelete(path)
        
    interface IDisposable with
        member x.Dispose() = 
            H5FClose(fileid) |> checknonneg "H5File:Close" |> ignore
            GC.SuppressFinalize(x)
    override x.Finalize() = (x:>IDisposable).Dispose()

