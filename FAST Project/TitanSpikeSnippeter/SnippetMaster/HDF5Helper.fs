module HDF5Helper

open RP.HDF5
open System.Xml.Linq
open System

let createEmptyH5File filename chgroups spikelen lfpchunksize spikeschunksize =
    use file = new H5File(filename,FileCreate(true,None),None)
    chgroups
    |> Seq.iter (fun (chname,chnums) -> 
                    let chstr = "./ChGroup_" + chname
                    let numchs = List.length chnums |> uint64
                    use dset = file.CreateDataSet(sprintf "%s/Spikes" chstr,DataTypeClass.H5_Int16,[|0UL,None,spikeschunksize;
                                                                                                   0UL,Some spikelen,spikelen;
                                                                                                   0UL,Some numchs,numchs|])
                    use dset = file.CreateDataSet(sprintf "%s/SpikeTimes" chstr,DataTypeClass.H5_UInt64,[|0UL,None,spikeschunksize|])
                    chnums |> List.iter (fun chnum -> 
                                           use dset = file.CreateDataSet(sprintf "./LFP/Ch_%d" chnum,DataTypeClass.H5_Int16,[|0UL,None,lfpchunksize|])
                                           ()))

let createEmptyMADFile filename =
    use file = new H5File(filename,FileCreate(true,None),None)
    use dset = file.CreateDataSet("./MAD",DataTypeClass.H5_Float64,[|0UL,None,1000UL;0UL,Some 64UL,64UL|])
    ()