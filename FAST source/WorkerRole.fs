namespace TitanSpikesClustererRole

open System
open System.Collections.Generic
open System.Diagnostics
open System.Linq
open System.Net
open System.Threading
open Microsoft.WindowsAzure
open Microsoft.WindowsAzure.Diagnostics
open Microsoft.WindowsAzure.ServiceRuntime
open Microsoft.ServiceBus
open Microsoft.ServiceBus.Messaging

type ClusterCommand = {HostName:string;SpikesFileName:string}
type WorkerRole() =
    inherit RoleEntryPoint() 
    let queuename = "TitanSpikesClustererQueue"
    let log message (kind : string) = Trace.TraceInformation(message, kind)
    let mutable (queue:QueueClient) = null
    let completed = new ManualResetEvent(false)
    override wr.Run() =
        log "TitanSpikesClusterer entry point called" "Information"
        queue.OnMessage(fun (msg:BrokeredMessage) ->
            let cmd = msg.GetBody<ClusterCommand>()
            ()
        )
        completed.WaitOne()|>ignore

    override wr.OnStart() = 
        ServicePointManager.DefaultConnectionLimit <- 100
        let cstr = CloudConfigurationManager.GetSetting("Microsoft.ServiceBus.ConnectionString")
        let ns = NamespaceManager.CreateFromConnectionString(cstr)
        if not (ns.QueueExists(queuename)) then (ns.CreateQueue(queuename)|>ignore)
        queue <- QueueClient.CreateFromConnectionString(cstr,queuename)
        base.OnStart()

    override wr.OnStop() =
        queue.Close()
        completed.Set()|>ignore
        base.OnStop()
