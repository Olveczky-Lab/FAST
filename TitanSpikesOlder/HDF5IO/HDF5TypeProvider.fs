namespace RP.HDF5

open System.Reflection
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Text.RegularExpressions
open H5Externs
open H5TypeProviderRuntime

#nowarn "25"
    
[<TypeProvider>]
type public HD5TypeProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    //Helper Functions
    let erasedType assemblyName rootNamespace typeName = 
        ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some(typeof<obj>))

    let runtimeType typeName = ProvidedTypeDefinition(typeName, Some typeof<obj>)

    // Get the assembly and namespace used to house the provided types
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "RP.HDF5"

    // Create the main provided type
    let h5Ty = erasedType asm ns "HDF5File"

    // Parameterize the type by the file to use as a template
    let filename = ProvidedStaticParameter("filename", typeof<string>)
    do h5Ty.DefineStaticParameters([filename], fun tyName [| :? string as filename |] ->

        //Resolve the filename relative to the resolution folder
        let resolvedFilename = Path.Combine(cfg.ResolutionFolder, filename)        
        //Recurse through the file to get all datasets
        use file = new H5File(filename,FileOpen(false),None)
        let fileid = file.GetID()
        let rec ProcessGroup path (curtype:ProvidedTypeDefinition) =
            curtype.AddXmlDoc(sprintf "A strongly typed interface to '%s'" path)
            curtype.HideObjectMethods <- true
            
            let groupInfo = GroupGetInfoByName fileid path
            // Loop through the group's links
            for n = 0 to (int groupInfo.nlinks - 1) do
                let linkname, linkinfo = helperGetLinkNameAndInfo fileid path false n
                let newpath = path + "/" + linkname
                if linkinfo.link_type = H5L_TYPE.HARD then
                    let objinfo = helperGetObjectInfo fileid path false n
                    match objinfo.object_type with
                    | H5O_TYPE.GROUP -> 
                        let newpath = (sprintf "%s/%s" path linkname)
                        let nestedtype = runtimeType linkname
                        curtype.AddMember(ProcessGroup newpath nestedtype)
                    | H5O_TYPE.DATASET ->
                        use ds = file.OpenDataSet(newpath)
                        let nestedtype = runtimeType linkname
                        let SizeMethod = ProvidedMethod("GetDimensions",
                                                                [],
                                                                typeof<uint64[]>,
                                                                IsStaticMethod = true,
                                                                InvokeCode = (fun args -> <@@ getDataSetDimensions resolvedFilename newpath @@>)) 
                        nestedtype.AddMember(SizeMethod)
                        use dtype = ds.TypeOpen()
                        match (dtype.GetDotNetType()) with
                        | Some typ ->
                            use space = ds.SpaceOpen()
                            let (dims,_) = space.GetDims()
                            let numdims = dims.Length
                            let array_type = TypeInfo.GetType(typ.FullName + "[" + String.init (numdims-1) (fun i -> ",") + "]")
                            let fetchMethod = ProvidedMethod("ReadData",
                                                                [ProvidedParameter("starts",typeof<uint64[]>,false,null);
                                                                    ProvidedParameter("stride",typeof<uint64[]>,false,null);
                                                                    ProvidedParameter("counts",typeof<uint64[]>,false,null)],
                                                                array_type,
                                                                IsStaticMethod = true,
                                                                InvokeCode = (fun args -> <@@ readData resolvedFilename newpath %%args.[0] %%args.[1] %%args.[2] @@>))
                            nestedtype.AddMember(fetchMethod)
                            let writeMethod = ProvidedMethod("WriteData",
                                                               [ProvidedParameter("starts",typeof<uint64[]>,false,null);
                                                                ProvidedParameter("stride",typeof<uint64[]>,false,null);
                                                                ProvidedParameter("data",array_type)],
                                                                typeof<unit>,
                                                                IsStaticMethod = true,
                                                                InvokeCode = (fun args ->   let expr = Quotations.Expr.Coerce(args.[2],typeof<System.Array>)
                                                                                            <@@ writeData resolvedFilename newpath %%args.[0] %%args.[1] %%expr @@>))
                            nestedtype.AddMember(writeMethod)
                        | _ -> ()
                        curtype.AddMember(nestedtype)
                    | _ -> ()
                else
                    ()
            curtype
        ProcessGroup "." (erasedType asm ns tyName)
        )
    // add the type to the namespace
    do this.AddNamespace(ns, [h5Ty])
[<TypeProviderAssembly>]
do()
