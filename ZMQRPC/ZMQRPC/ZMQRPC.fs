open System.Diagnostics
open System.Reflection
open Nessos.FsPickler
open System.IO
open ZeroMQ
open System

#if INTERACTIVE
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let thisexe = __SOURCE_DIRECTORY__ + @"\bin\Release\ZMQRPC.exe"
#else
let thisexe = Uri.UnescapeDataString(UriBuilder(Assembly.GetExecutingAssembly().CodeBase).Path)
#endif

let serialize x =
    let fsp = new FsPickler()
    use ms = new MemoryStream()
    fsp.Serialize(x.GetType(),ms,x)
    let d = Array.zeroCreate<byte> (ms.Length|>int)
    ms.Seek(0L,SeekOrigin.Begin) |> ignore
    ms.Read(d,0,d.Length) |> ignore
    d

let getInt32 (socket:ZmqSocket) =
    let x = Array.zeroCreate<byte> 4
    let r = socket.Receive(x)
    BitConverter.ToInt32(x,0)

let sendInt32 (socket:ZmqSocket) (x:int) = 
    socket.Send(BitConverter.GetBytes(x))

let deserialize (x:byte[]) =     
    let fsp = new FsPickler()
    use ms = new MemoryStream(x)
    fsp.Deserialize(ms)

let startServer assemblyFile typeName methodName numbytes endpoint =
    let ass = Assembly.LoadFrom(assemblyFile)
    let typ = ass.GetType(typeName)
    let meth = typ.GetMethod(methodName)
    use context = ZmqContext.Create()
    use socket = context.CreateSocket(SocketType.REP)
    socket.Bind(endpoint)
    let data = Array.zeroCreate<byte> numbytes
    let n = socket.Receive(data)
    let ps = deserialize data
    let r = serialize (meth.Invoke(null,ps))
    sendInt32 socket r.Length |> ignore
    getInt32 socket |> ignore // ack
    socket.Send(r) |> ignore
    getInt32 socket |> ignore //ack

open System.Diagnostics

let CallMethodInNewProcess assemblyFile typeName methodName portnum parameters =
    use context = ZmqContext.Create()
    let b = serialize parameters
    let args = [|assemblyFile;typeName;methodName;sprintf "%d" b.Length;sprintf @"tcp://*:%d" portnum|]
    use p = new Process(StartInfo=ProcessStartInfo(thisexe,String.concat " " (args|>Array.map (sprintf @"""%s""")),                                                    
                                                    UseShellExecute=false,
                                                    CreateNoWindow=true,
                                                    WorkingDirectory=Path.GetDirectoryName(assemblyFile)))
    p.Start() |> ignore
    use socket = context.CreateSocket(SocketType.REQ)
    socket.Connect(sprintf @"tcp://localhost:%d" portnum)
    socket.Send b |> ignore
    let numbytes = getInt32 socket
    sendInt32 socket 0 |> ignore //ack
    let r = Array.zeroCreate<byte> numbytes
    socket.Receive(r) |> ignore
    sendInt32 socket 0 |> ignore
    deserialize r

[<EntryPoint>]
let main argv = 
    match argv with
    | [|assemblyFile;typeName;methodName;numbytes;endpoint|] ->
        startServer assemblyFile typeName methodName (numbytes|>int) endpoint
    | _ -> printfn "Invalid Arguments"
    0