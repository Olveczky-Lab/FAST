module H5Externs

open System.Runtime.InteropServices
open System
open System.Text
open System.IO

#nowarn "51" // no warnings for native pointer use (they are used as stand ins for byref)
let [<Literal>] HDF5DLL = @"hdf5.dll"
let [<Literal>] HDF5DLLUNIX = @"libhdf5.so"
let curdir =  Path.GetDirectoryName((new System.Uri(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath)
let HDF5DLLPATH = curdir + @"\HDF5\1.8.9" //Change to load from some configuration file later

/// Wrapper to "kernel32.dll" function "LoadLibraryExW".
[< DllImport("kernel32.dll", EntryPoint="LoadLibraryEx")>]
extern IntPtr LoadLibrary_Win([<In>] String libname, int, int flags)

[< DllImport("libdl.so", EntryPoint="dlopen")>]
extern IntPtr LoadLibrary_Unix([<In>] String libname, int flags)

/// Wrapper to "kernel32.dll" function "FreeLibrary".
[< DllImport("kernel32.dll", EntryPoint="FreeLibrary")>]
extern int FreeLibrary_Win([<In>] IntPtr hModule)

[< DllImport("libdl.so", EntryPoint="dlclose") >]
extern int FreeLibrary_Unix([<In>] IntPtr hModule)

/// Wrapper to "kernel32.dll" function "GetProcAddress".
[< DllImport("kernel32.dll", EntryPoint="GetProcAddress")>]
extern IntPtr GetProcAddress_Win([<In>] IntPtr hModule, [<In>] String procname)  

[< DllImport("libdl.so", EntryPoint="dlsym")>]
extern IntPtr GetProcAddress_Unix([<In>] IntPtr hModule, [<In>] String procname)  

/// Wrapper to "kernel32.dll" function "GetLastError".
[< DllImport("kernel32.dll", EntryPoint="GetLastError") >]
extern int GetLastError_Win()

[< DllImport("libdl.so", EntryPoint="dlerror") >]
extern int GetLastError_Unix()

let platformAppropriate (f_Win,f_Unix) =
    match Environment.OSVersion.Platform with
    | PlatformID.Win32NT -> f_Win
    | PlatformID.Unix -> f_Unix
    | _ -> failwith "OS Not Supported"

let LoadLibrary libname =
    match Environment.OSVersion.Platform with
    | PlatformID.Win32NT -> LoadLibrary_Win(libname,0,0x00001100)
    | PlatformID.Unix -> LoadLibrary_Unix(libname,2)
    | _ -> failwith "OS Not Supported"

let FreeLibrary = platformAppropriate (FreeLibrary_Win,FreeLibrary_Unix)
let GetProcAddress hModule procName = platformAppropriate (GetProcAddress_Win,GetProcAddress_Unix)(hModule,procName)
let GetLastError = platformAppropriate (GetLastError_Win,GetLastError_Unix)

// Some general purpose methods

/// Check that an integer argument is not negative.
/// If the argument is negative an exception is thrown.
let checknonneg (routinename: string) (i: int) =
    if i < 0
    then failwithf "Failure; Routine name = '%s'; Error code = %d." routinename i
    else i

/// Check that an integer argument is (strictly) positive.
/// If the argument is negative or zero an exception is thrown.
let checkpositive (routinename: string) (i: int) =
    if i = 0
    then failwithf "Failure; Routine name = '%s'; Returned code zero." routinename
    else checknonneg routinename i

/// Returns true when the 32-bit version of the HDF5-library is used,
/// and returns false when the 64-bit version of the library is used.
let use32bit =
    not Environment.Is64BitProcess

let libraryname = 
    if (Environment.OSVersion.Platform = PlatformID.Win32NT) then
        sprintf @"%s\%s\%s" HDF5DLLPATH (if use32bit then "x86" else "x64") HDF5DLL
    else
        HDF5DLLUNIX

let LibOpen =
    if (Environment.OSVersion.Platform = PlatformID.Win32NT) then
        Some <| LoadLibrary_Win(libraryname,0,0x00001100)
    else
        None

let LibClose handle =
    if (Environment.OSVersion.Platform = PlatformID.Win32NT) then
        match handle with
        | None -> ()
        | Some h -> FreeLibrary_Win(h) |> ignore
    

/// Type definition used in loadInt32VariableFromLibrary
type FuncVoidToInt = delegate of unit -> int

let loadInt32VariableFromLibrary (libraryname: string) (variablename: string) =
    let loadflags = 0x00001100
    let libhandle = LoadLibrary libraryname
    if libhandle = IntPtr(0) then
        let errorcode = GetLastError()
        failwithf "Unable to load library '%s'; error code %d." libraryname errorcode
    try
        // Get pointers to H5open and H5close and get the address of the named variable in question
        let H5openProc = GetProcAddress libhandle "H5open"
        if H5openProc = IntPtr(0) then failwith "Unable to locate function 'H5open' in library."
        let openProc = (Marshal.GetDelegateForFunctionPointer(H5openProc, typeof<FuncVoidToInt>)) :?> FuncVoidToInt
        let H5closeProc = GetProcAddress libhandle "H5close"
        if H5closeProc = IntPtr(0) then failwith "Unable to locate function 'H5close' in library."
        let closeProc = (Marshal.GetDelegateForFunctionPointer(H5openProc, typeof<FuncVoidToInt>)) :?> FuncVoidToInt
        let intptr = GetProcAddress libhandle variablename
        if intptr = IntPtr(0) then
            let errorcode = GetLastError()
            failwithf "Unable to locate variable named '%s' in library '%s'; error code %d." variablename libraryname errorcode
        // Initialize HDF5 library (required for the variables to be filled in with actual values)
        do openProc.Invoke () |> checknonneg "loadInt32VariableFromLibrary:H5open" |> ignore
        // Read the contents of the address using type Marshal class
        let result = Marshal.ReadInt32(intptr)
        // Deinitialize HDF5 library prior to unloading the present library instance
        do closeProc.Invoke () |> checknonneg "loadInt32VariableFromLibrary:H5close" |> ignore
        result
    finally
        FreeLibrary libhandle |> ignore

/// Used to load data type constans from HDF5 library.
/// Input is the (exact) name of the data type constant.
/// Output is a "lazy" value. This means that the constant
/// is loaded when it is first needed. A run-time error
/// occurs if the constant cannot be loaded at that time.
let getLazyHDFInt32Variable (variablename: string) =
    lazy loadInt32VariableFromLibrary libraryname variablename

// additional constants (data types) loaded when needed

// The datatype (for instance signed 16 bit integer or unsigned 16 bit integer) will determine how the value is shown in e.g. HDFView,
// even if it is unimportant from a pure save and load perspective .
// Add more constants if needed .
let const_8s_g =         getLazyHDFInt32Variable "H5T_STD_I8LE_g"
let const_16s_g =         getLazyHDFInt32Variable "H5T_STD_I16LE_g"
let const_32s_g =         getLazyHDFInt32Variable "H5T_STD_I32LE_g"
let const_64s_g =         getLazyHDFInt32Variable "H5T_STD_I64LE_g"
let const_08u_g =         getLazyHDFInt32Variable "H5T_STD_U8LE_g" // particularly useful for serialization
let const_16u_g =         getLazyHDFInt32Variable "H5T_STD_U16LE_g"
let const_32u_g =         getLazyHDFInt32Variable "H5T_STD_U32LE_g"
let const_64u_g =         getLazyHDFInt32Variable "H5T_STD_U64LE_g"
let const_IEEE_F32LE_g = getLazyHDFInt32Variable "H5T_IEEE_F32LE_g"
let const_IEEE_F64LE_g = getLazyHDFInt32Variable "H5T_IEEE_F64LE_g" 
let const_CS1_g =        getLazyHDFInt32Variable "H5T_C_S1_g" // exotic type indicating zero-terminated C-type string (ASCII). In essence a byte read out as an ASCII-char.

let const_H5P_ROOT              = getLazyHDFInt32Variable "H5P_CLS_ROOT_g"
let const_H5P_OBJECT_CREATE     = getLazyHDFInt32Variable "H5P_CLS_OBJECT_CREATE_g"
let const_H5P_FILE_CREATE       = getLazyHDFInt32Variable "H5P_CLS_FILE_CREATE_g"
let const_H5P_FILE_ACCESS       = getLazyHDFInt32Variable "H5P_CLS_FILE_ACCESS_g"
let const_H5P_DATASET_CREATE    = getLazyHDFInt32Variable "H5P_CLS_DATASET_CREATE_g"
let const_H5P_DATASET_ACCESS    = getLazyHDFInt32Variable "H5P_CLS_DATASET_ACCESS_g"
let const_H5P_DATASET_XFER      = getLazyHDFInt32Variable "H5P_CLS_DATASET_XFER_g"
let const_H5P_FILE_MOUNT        = getLazyHDFInt32Variable "H5P_CLS_FILE_MOUNT_g"
let const_H5P_GROUP_CREATE      = getLazyHDFInt32Variable "H5P_CLS_GROUP_CREATE_g"
let const_H5P_GROUP_ACCESS      = getLazyHDFInt32Variable "H5P_CLS_GROUP_ACCESS_g"
let const_H5P_DATATYPE_CREATE   = getLazyHDFInt32Variable "H5P_CLS_DATATYPE_CREATE_g"
let const_H5P_DATATYPE_ACCESS   = getLazyHDFInt32Variable "H5P_CLS_DATATYPE_ACCESS_g"
let const_H5P_STRING_CREATE     = getLazyHDFInt32Variable "H5P_CLS_STRING_CREATE_g"
let const_H5P_ATTRIBUTE_CREATE  = getLazyHDFInt32Variable "H5P_CLS_ATTRIBUTE_CREATE_g"
let const_H5P_OBJECT_COPY       = getLazyHDFInt32Variable "H5P_CLS_OBJECT_COPY_g"
let const_H5P_LINK_CREATE       = getLazyHDFInt32Variable "H5P_CLS_LINK_CREATE_g"
let const_H5P_LINK_ACCESS       = getLazyHDFInt32Variable "H5P_CLS_LINK_ACCESS_g"

let [<Literal>] H5P_DEFAULT = 0 // default value for all property list classes .

// Structure(s) typically used in the newer versions of HDF5 (ver 1.8).

/// Storage type used in H5G_info_t
type H5G_STORAGE_TYPE =
| UNKNOWN = -1 // Unknown link storage type
| SYMBOL_TABLE = 0 // Links in group are stored with a "symbol table" ("old-style" groups)
| COMPACT = 1 // Links are stored in object header
| DENSE = 2 // Links are stored in fractal heap and indexed with binary tree

/// Link type used in H5L_info_t
type H5L_TYPE =
| ERROR = -1 // Invalid link type id
| HARD = 0 // Hard link
| SOFT = 1 // Soft link
| EXTERNAL = 64 // External link
| MAXIMUM = 255 // Maximum link type id
 
/// Object type used in H5O_info_t
type H5O_TYPE =
| UNKNOWN = -1 // Unknown object type
| GROUP = 0 // Group
| DATASET = 1 // Dataset
| NAMED_DATATYPE = 2 // Named data type
| NTYPES = 3 // Number of different object types (must be last)

type H5S_SelOper =
| H5S_SELECT_SET = 0
| H5S_SELECT_OR = 1
| H5S_SELECT_AND = 2
| H5S_SELECT_XOR = 3
| H5S_SELECT_NOTB = 4
| H5S_SELECT_NOTA = 5

type H5T_Class =
| H5T_NO_CLASS  = -1 // error (not used)
| H5T_INTEGER   =  0 // integer types
| H5T_FLOAT     =  1 // floating-point types
| H5T_TIME      =  2 // date and time types (exotic)
| H5T_STRING    =  3 // character string types (text)
| H5T_BITFIELD  =  4 // bit field types
| H5T_OPAQUE    =  5 // opaque types (exotic)
| H5T_COMPOUND  =  6 // compound types (exotic)
| H5T_REFERENCE =  7 // reference types (exotic)
| H5T_ENUM      =  8 // enumeration types (exotic)
| H5T_VLEN      =  9 // variable-Length types (exotic)
| H5T_ARRAY     = 10 // array types (exotic)

type H5T_Sign =
| H5T_SGN_NONE = 0
| H5T_SGN_2 = 1

type FileAccess =
| ACC_RDONLY = 0u // open in read only mode .
| ACC_RDWR =   1u // open in read-and-write mode .
| ACC_TRUNC = 2u // truncate (i.e. wipe) file on creation if it already exists .
| ACC_EXCL =  4u // fail if file already exists .

let [<Literal>] H5T_CSET_ASCII = 0 // character encoding ASCII (default) .
let [<Literal>] H5T_CSET_UTF8 = 1 // character encoding UTF-8 (introduced in version 1.8.0) .
  
/// Enumeration type indicating wheter string encoding is ASCII or UTF-8.
type CSET =
| ASCII
| UTF8
    with
    static member ofH5TCSET (code: int) =
        match code with
        | H5T_CSET_ASCII -> ASCII
        | H5T_CSET_UTF8 -> UTF8
        | _ -> failwithf "Unknown character encoding set code %d." code
    member self.toH5TCSET () =
        match self with
        | ASCII -> H5T_CSET_ASCII
        | UTF8 -> H5T_CSET_UTF8
    member self.Encoding =
        match self with
        | ASCII -> new ASCIIEncoding() :> Encoding
        | UTF8 -> new UTF8Encoding() :> Encoding

[< Struct >]
type H5G_info_t =
    val storage_type: H5G_STORAGE_TYPE
    val private unused_private_field_1: int32 // padding to maintain field alignment
    val nlinks: uint64
    val max_corder: int64 // creation order fields have different data types depending on the context
    val mounted: uint32
    val private unused_private_field_2: uint32

[< Struct >]
type H5L_info_t =
    val link_type: H5L_TYPE
    val corder_valid: uint32
    val corder: int64
    val cset: int32 // Character set of link name 
    val private unused_private_field_1: int32
    val address_or_size: uint64 // Address hard link points to or size of a soft link (or UD link value)
    with
        member self.CSET = CSET.ofH5TCSET self.cset

[< Struct >]
type H5A_info_t =
    val corder_valid: uint32
    val corder: int32 // creation order field here is 32 bit signed integer
    val cset: int32 // Character set of link name 
    val private unused_private_field_1: int32
    val datasize: uint64
    with
        member self.CSET = CSET.ofH5TCSET self.cset

[< Struct >]
type H5O_hdr_info_t =
    val version: uint32
    val nmesgs: uint32
    val nchunks: uint32
    val flags: uint32
    val space_total: uint64
    val space_meta: uint64
    val space_mesg: uint64
    val space_free: uint64
    val mesg_flags_present: uint64
    val mesg_flags_shared: uint64

[< Struct >]
type H5O_info_t =
    val fileno: uint64
    val address: uint64
    val object_type: H5O_TYPE
    val reference_count: uint32
    val atime: int64
    val mtime: int64
    val ctime: int64
    val btime: int64
    val num_attrs: uint64
    val headerinformation: H5O_hdr_info_t
    val meta_obj_index_size: uint64
    val meta_obj_heap_size: uint64
    val meta_attr_index_size: uint64
    val meta_attr_heap_size: uint64
  
// DLL-imports for core methods

[< DllImport(HDF5DLL, EntryPoint="H5open", CallingConvention=CallingConvention.Cdecl) >]
extern int H5LibOpen()

[< DllImport(HDF5DLL, EntryPoint="H5get_libversion", CallingConvention=CallingConvention.Cdecl) >]
extern int H5LibGetVersion([<Out>] int* majnum, [<Out>] int* minnum, [<Out>] int* relnum )

[< DllImport(HDF5DLL, EntryPoint="H5close", CallingConvention=CallingConvention.Cdecl) >]
extern int H5LibClose()

[< DllImport(HDF5DLL, EntryPoint="H5Fcreate", CallingConvention=CallingConvention.Cdecl) >]
extern int H5FCreate([< MarshalAs(UnmanagedType.LPStr) >] String filename, uint32 flags, int fcpl_id, int fapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Fopen", CallingConvention=CallingConvention.Cdecl) >]
extern int H5FOpen([< MarshalAs(UnmanagedType.LPStr) >] String filename, uint32 flags, int fapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Fclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5FClose(int fileid)

[< DllImport(HDF5DLL, EntryPoint="H5Tcopy", CallingConvention=CallingConvention.Cdecl) >]
extern int H5TCopy(int id)

[< DllImport(HDF5DLL, EntryPoint="H5Tget_class", CallingConvention=CallingConvention.Cdecl) >]
extern int H5TGetClass(int datatypeid)

[< DllImport(HDF5DLL, EntryPoint="H5Tget_size", CallingConvention=CallingConvention.Cdecl) >]
extern int H5TGetSize(int datatypeid)

[< DllImport(HDF5DLL, EntryPoint="H5Tset_size", CallingConvention=CallingConvention.Cdecl) >]
extern int H5TSetSize(int datatypeid, int size)

[< DllImport(HDF5DLL, EntryPoint="H5Tget_sign", CallingConvention=CallingConvention.Cdecl) >]
extern int H5TGetSign(int datatypeid)

[< DllImport(HDF5DLL, EntryPoint="H5Tget_cset", CallingConvention=CallingConvention.Cdecl) >]
extern int H5Tget_cset(int datatypeid)

[< DllImport(HDF5DLL, EntryPoint="H5Tset_cset", CallingConvention=CallingConvention.Cdecl) >]
extern int H5TSet_cset(int datatypeid, int cset)

[< DllImport(HDF5DLL, EntryPoint="H5Tclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5TClose(int datatypeid)

[< DllImport(HDF5DLL, EntryPoint="H5Screate_simple", CallingConvention=CallingConvention.Cdecl) >]
extern int H5SCreateSimple(int rank, [< MarshalAs(UnmanagedType.LPArray) >] uint64[] dims, [<MarshalAs(UnmanagedType.LPArray) >] uint64[] maxdims);

[< DllImport(HDF5DLL, EntryPoint="H5Sget_simple_extent_ndims", CallingConvention=CallingConvention.Cdecl) >]
extern int H5SGetSimpleNDims(int spaceid)

[< DllImport(HDF5DLL, EntryPoint="H5Sget_simple_extent_dims", CallingConvention=CallingConvention.Cdecl) >]
extern int H5SGetSimpleDims(int spaceid, [< MarshalAs(UnmanagedType.LPArray) >][<In>][<Out>] uint64[] dims,  [<MarshalAs(UnmanagedType.LPArray) >][<In>][<Out>] uint64[] maxdims)

[< DllImport(HDF5DLL, EntryPoint="H5Sget_simple_extent_type", CallingConvention=CallingConvention.Cdecl) >]
extern int H5SGetSimpleType(int spaceid)

[< DllImport(HDF5DLL, EntryPoint="H5Sis_simple", CallingConvention=CallingConvention.Cdecl) >]
extern int H5SGetIsSimple(int spaceid)

[< DllImport(HDF5DLL, EntryPoint="H5Sclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5SClose(int spaceid)

[< DllImport(HDF5DLL, EntryPoint="H5Dcreate2", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DCreate2(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String dataSetName, int datatypeid, int spaceid, int lcpl_id, int dcpl_id, int dapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Dopen2", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DOpen2(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String dataSetName, int dapl_id )

[< DllImport(HDF5DLL, EntryPoint="H5Dget_type", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DGetType(int datasetid)

[< DllImport(HDF5DLL, EntryPoint="H5Dget_space", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DGetSpace(int datasetid)

[< DllImport(HDF5DLL, EntryPoint="H5Dset_extent", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DSetExtent(int datasetid, uint64[] dims)

[< DllImport(HDF5DLL, EntryPoint="H5Dclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DClose(int datasetid)

[< DllImport(HDF5DLL, EntryPoint="H5Acreate2", CallingConvention=CallingConvention.Cdecl) >]
extern int H5ACreate2(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String attributeName, int type_id, int space_id, int acpl_id, int aapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Aopen", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AOpen(int objid, [< MarshalAs(UnmanagedType.LPStr) >] String attributeName, int aapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Aclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AClose(int attrid)

[< DllImport(HDF5DLL, EntryPoint="H5Aexists", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AExists(int objid, [< MarshalAs(UnmanagedType.LPStr) >] String attributeName)

[< DllImport(HDF5DLL, EntryPoint="H5Aget_type", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AGetType(int attrid)

[< DllImport(HDF5DLL, EntryPoint="H5Aget_space", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AGetSpace(int attrid)

[< DllImport(HDF5DLL, EntryPoint="H5Aget_info", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AGetInfo(int attrid, [<Out>] H5A_info_t* attributeinfo)

[< DllImport(HDF5DLL, EntryPoint="H5Aget_info_by_name", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AGetInfoByName(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String objectName, [< MarshalAs(UnmanagedType.LPStr) >] String attributeName, [<Out>] H5A_info_t* attributeinfo, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Aget_info_by_idx", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AGetInfoByIdx(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String objectName, int index_field, int iter_order, uint64 n, [<Out>] H5A_info_t* attributeinfo, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Aget_name_by_idx", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AGetNameByIdx(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String objectName, int index_field, int iter_order, uint64 n, [<In>][<Out>] byte[] namebuffer, int namebufferlength, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Awrite", CallingConvention=CallingConvention.Cdecl) >]
extern int H5AWrite(int attrid, int memtypeid, [<In>] uint8[] data)

[< DllImport(HDF5DLL, EntryPoint="H5Aread", CallingConvention=CallingConvention.Cdecl) >]
extern int H5ARead(int attrid, int memtypeid, [<In>][<Out>] uint8[] data)

[< DllImport(HDF5DLL, EntryPoint="H5Adelete", CallingConvention=CallingConvention.Cdecl) >]
extern int H5ADelete(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String attributeName)

[< DllImport(HDF5DLL, EntryPoint="H5Gcreate2", CallingConvention=CallingConvention.Cdecl) >]
extern int H5GCreate2(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String groupName, int lcplid, int gcplid, int gaplid)

[< DllImport(HDF5DLL, EntryPoint="H5Gopen2", CallingConvention=CallingConvention.Cdecl) >]
extern int H5GOpen2(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String groupName, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Gclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5GClose(int groupid)

[< DllImport(HDF5DLL, EntryPoint="H5Gget_info", CallingConvention=CallingConvention.Cdecl) >]
extern int H5GGetInfo(int groupid, [<Out>] H5G_info_t* groupinfo)

[< DllImport(HDF5DLL, EntryPoint="H5Gget_info_by_name", CallingConvention=CallingConvention.Cdecl) >]
extern int H5GGetInfoByName(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String groupName, [<Out>] H5G_info_t* groupinfo, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Lget_info_by_idx", CallingConvention=CallingConvention.Cdecl) >]
extern int H5LGetInfoByIdx(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String groupName, int index_field, int iter_order, uint64 n, [<Out>] H5L_info_t* linkinfo, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Lget_name_by_idx", CallingConvention=CallingConvention.Cdecl) >]
extern int H5LGetNameByIdx(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String groupName, int index_field, int iter_order, uint64 n, [<In>][<Out>] byte[] namebuffer, int namebufferlength, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Ldelete", CallingConvention=CallingConvention.Cdecl) >]
extern int H5LDelete(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String linkName, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Oopen", CallingConvention=CallingConvention.Cdecl) >]
extern int H5OOpen(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String objectName, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Oclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5OClose(int objectid)

[< DllImport(HDF5DLL, EntryPoint="H5Oget_info", CallingConvention=CallingConvention.Cdecl) >]
extern int H5OGetInfo(int objectid, [<Out>] H5O_info_t* objectinfo)

[< DllImport(HDF5DLL, EntryPoint="H5Oget_info_by_name", CallingConvention=CallingConvention.Cdecl) >]
extern int H5OGetInfoByName(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String objectName, [<Out>] H5O_info_t* objectinfo, int lapl_id)

[< DllImport(HDF5DLL, EntryPoint="H5Oget_info_by_idx", CallingConvention=CallingConvention.Cdecl) >]
extern int H5OGetInfoByIdx(int locid, [< MarshalAs(UnmanagedType.LPStr) >] String groupName, int index_field, int iter_order, uint64 n, [<Out>] H5O_info_t* objectinfo, int lapl_id)
  
// DLL-imports for write-read

[< DllImport(HDF5DLL, EntryPoint="H5Dread", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DRead(int datasetid, int mem_type_id, int mem_space_id, int file_space_id, int xfer_plist_id, [<In>][<Out>] nativeint data)

[< DllImport(HDF5DLL, EntryPoint="H5Dwrite", CallingConvention=CallingConvention.Cdecl) >]
extern int H5DWrite(int datasetid, int mem_type_id, int mem_space_id, int file_space_id, int xfer_plist_id, [<In>] nativeint data)


// DLLImport for Additional Methods

[< DllImport(HDF5DLL, EntryPoint="H5Pcreate", CallingConvention=CallingConvention.Cdecl) >]
extern int H5PCreate(int clsid)

[< DllImport(HDF5DLL, EntryPoint="H5Pclose", CallingConvention=CallingConvention.Cdecl) >]
extern int H5PClose(int plistid)

[< DllImport(HDF5DLL, EntryPoint="H5Pset_chunk", CallingConvention=CallingConvention.Cdecl) >]
extern int H5PSetChunk(int plistid, int ndims, [< MarshalAs(UnmanagedType.LPArray) >] uint64[] dims)

[< DllImport(HDF5DLL, EntryPoint="H5Pset_create_intermediate_group", CallingConvention=CallingConvention.Cdecl) >]
extern int H5PSetCreateIntermediateGroup(int lcplid, int create)

[< DllImport(HDF5DLL, EntryPoint="H5Pset_fclose_degree", CallingConvention=CallingConvention.Cdecl) >]
extern int H5PSetFCloseDegree(int faplid, int degree)

[< DllImport(HDF5DLL, EntryPoint="H5Sselect_hyperslab", CallingConvention=CallingConvention.Cdecl) >]
extern int H5SSelect_hyperslab(int spaceid, H5S_SelOper seloper, [< MarshalAs(UnmanagedType.LPArray) >] uint64[] start, [< MarshalAs(UnmanagedType.LPArray) >] uint64[] stride, [< MarshalAs(UnmanagedType.LPArray) >] uint64[] count, [< MarshalAs(UnmanagedType.LPArray) >] uint64[] block)

[< DllImport(HDF5DLL, EntryPoint="H5Pset_chunk_cache", CallingConvention=CallingConvention.Cdecl) >]
extern int H5PSetChunkCache(int dapl_id, int rdcc_nslots, int rdcc_nbytes, double rdcc_w0)

type H5_INDEX =
| UNKNOWN = -1 // Unknown index type
| NAME = 0 // Index on names
| CRT_ORDER = 1 // Index on creation order
| INDEX_N = 2 // Number of indices defined

type H5_ITER =
| UNKNOWN = -1 // Unknown order
| INC = 0 // Increasing order
| DEC = 1 // Decreasing order
| NATIVE = 2 // No particular order, whatever is fastest
| N = 3 // Number of iteration orders

/// Get group information by group name.
let GroupGetInfoByName (locid: int) (groupname: string): H5G_info_t =
    let mutable groupinfo = new H5G_info_t ()
    H5GGetInfoByName(locid, groupname, &&groupinfo, 0)
    |> checknonneg "GroupGetInfoByName"
    |> ignore
    groupinfo

/// Helper function to get iteration flags.
/// If it is known in advance that a creation order index exists use that,
/// otherwise iterate by name in ascending lexigraphic order.
let helperIterationTuple (indexedByCreationOrder: bool) =
    if indexedByCreationOrder
    then int H5_INDEX.CRT_ORDER, int H5_ITER.NATIVE
    else int H5_INDEX.NAME, int H5_ITER.INC

/// Helper function to retrieve link name and link information from group name and link index.
let helperGetLinkNameAndInfo (locid: int) (groupname: string) (indexedByCreationOrder: bool) (n: int) =
    let iter_field, iter_direction = helperIterationTuple indexedByCreationOrder
    let mutable linkinfo = new H5L_info_t ()
    H5LGetInfoByIdx(locid, groupname, iter_field, iter_direction, (uint64 n), &&linkinfo, 0)
    |> checknonneg "helperGetLinkNameAndInfo:H5LGetInfoByIdx"
    |> ignore
    let namelength =
        H5LGetNameByIdx(locid, groupname, iter_field, iter_direction, (uint64 n), null, 0, 0)
        |> checkpositive "helperGetLinkNameAndInfo:H5LGetNameByIdx:length"
    let bufferlength = namelength + 1
    let buffer = Array.zeroCreate<byte> bufferlength
    H5LGetNameByIdx(locid, groupname, iter_field, iter_direction, (uint64 n), buffer, bufferlength, 0)
    |> checkpositive "helperGetLinkNameAndInfo:H5LGetNameByIdx:buffer"
    |> ignore
    let name = linkinfo.CSET.Encoding.GetString(buffer, 0, namelength)
    name, linkinfo

/// Helper function to object info by group name and link index.
/// This function is only used when helperGetLinkNameAndInfo is also used,
/// otherwise it makes more sense to use e.g. ObjectGetInfoByName .
let helperGetObjectInfo (locid: int) (groupname: string) (indexedByCreationOrder: bool) (n: int) =
    let iter_field, iter_direction = helperIterationTuple indexedByCreationOrder
    let mutable objinfo = new H5O_info_t ()
    H5OGetInfoByIdx(locid, groupname, iter_field, iter_direction, (uint64 n), &&objinfo, 0)
    |> checknonneg "helperGetObjectInfo:H5OGetInfoByIdx"
    |> ignore
    objinfo
