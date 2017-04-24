namespace RP.HDF5

open System
open H5Externs

type PropListClass =
| FileAccess
| FileCreate
| DataSetCreate
| DataSetAccess
| LinkCreate
    with 
    member x.GetID() =
        match x with
        | FileAccess -> const_H5P_FILE_ACCESS.Value
        | FileCreate -> const_H5P_FILE_CREATE.Value
        | DataSetCreate -> const_H5P_DATASET_CREATE.Value
        | DataSetAccess -> const_H5P_DATASET_ACCESS.Value
        | LinkCreate -> const_H5P_LINK_CREATE.Value

module PlistHelper = 
    let createPlist (plistclass:PropListClass) =
        H5PCreate(plistclass.GetID())
        |> checkpositive (sprintf "H5PropertyList:%A:Create" plistclass)
    let disposePlist plistid (plistclass:PropListClass) =
        H5PClose(plistid) 
        |> checknonneg (sprintf "H5PropertyList:%A:Close" plistclass) |> ignore

open PlistHelper

type H5FileAccessPropList() =
    let plistid = createPlist PropListClass.FileAccess
    member x.GetID() = plistid
    interface IDisposable with
        member this.Dispose() = disposePlist plistid PropListClass.FileAccess

type H5FileCreatePropList() =
    let plistid = createPlist PropListClass.FileCreate
    member x.GetID() = plistid
    interface IDisposable with
        member this.Dispose() = disposePlist plistid PropListClass.FileCreate

type H5DataSetCreatePropList() =
    let plistid = createPlist PropListClass.DataSetCreate
    member x.GetID() = plistid
    member x.SetChunk(dims) = 
        H5PSetChunk(plistid,Array.length dims,dims)
        |> checknonneg "H5DataSetCreatePropList:SetChunk" |> ignore
    interface IDisposable with
        member this.Dispose() = disposePlist plistid PropListClass.DataSetCreate

type H5DataSetAccessPropList() =
    let plistid = createPlist PropListClass.DataSetAccess
    member x.GetID() = plistid
    member x.SetChunkCache(numslots,numbytes,w0) = H5PSetChunkCache(plistid,numslots,numbytes,w0)
    interface IDisposable with
        member this.Dispose() = disposePlist plistid PropListClass.DataSetAccess

type H5LinkCreatePropList() =
    let plistid = createPlist PropListClass.LinkCreate
    member x.GetID() = plistid
    member x.SetCreateIntermediateGroups() =
        H5PSetCreateIntermediateGroup(plistid,1) 
        |> checknonneg "H5LinkCreatePropList::CreateIntermediateGroup" |> ignore
    interface IDisposable with
        member this.Dispose() = disposePlist plistid PropListClass.LinkCreate
