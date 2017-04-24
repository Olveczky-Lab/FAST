open Microsoft.Azure.Batch
open Microsoft.Azure.Batch.Auth
open Microsoft.Azure.Batch.Common
open Microsoft.WindowsAzure.Storage
open Microsoft.WindowsAzure.Storage.Auth
open Microsoft.WindowsAzure.Storage.Blob
open System
open System.IO
open System.Collections.Generic

let Url = "https://batch.core.windows.net"
let BatchAccount = "olveczkylab"
let BatchKey = "hNcsDMH6UQwns3pWXwHw9fiBSf3/Ikd5gnd51diPeUQo3a+ZKfn2jG/2ZYI+q6xzPKBPOYvYxtwdwnL8Kh542A=="
let StorageAccount = "olveczkylab"
let StorageKey = "kSIA7BfznKD/vGCL/QTnSOWj256xWc/N4sdFhq7s3s1oxEfZR4UBQVUJ0dEr3NbqEDDrA2/LGeHhTyAvwLfeew=="
let ContainerName = "titanbatch"

System.Net.ServicePointManager.DefaultConnectionLimit <- 100

let batchcred = BatchCredentials(BatchAccount,BatchKey)

let createClusterPool poolname vmsize numvms numtaskspervm =
    let resourceFiles =
        let storagecred = StorageCredentials(StorageAccount,StorageKey)
        let container = CloudStorageAccount(storagecred,false).CreateCloudBlobClient().GetContainerReference(ContainerName)
        let policy = 
            SharedAccessBlobPolicy(SharedAccessStartTime = Nullable(DateTimeOffset(DateTime.UtcNow)), 
                SharedAccessExpiryTime = Nullable(DateTimeOffset(DateTime.UtcNow.AddYears(1))),
                Permissions = (SharedAccessBlobPermissions.Read ||| SharedAccessBlobPermissions.List)
            )
        let uri,sas = container.Uri.ToString(),container.GetSharedAccessSignature(policy)
        container.ListBlobs(useFlatBlobListing=true) |> Seq.map (fun item ->
            let relativeuri = item.Uri.ToString().Remove(0,uri.Length)
            ResourceFile(sprintf "%s%s%s" uri relativeuri sas,relativeuri.Replace(@"/",@"\").Substring(1))
        ) |> Seq.cast |> fun x -> List(x)
    use client = BatchClient.Connect(Url,batchcred)
    use pm = client.OpenPoolManager()
    let pool = pm.CreatePool(poolname,"4",vmsize,Nullable(numvms))
    let st = StartTask(ResourceFiles=resourceFiles,WaitForSuccess = Nullable(true),CommandLine = "cmd /c CopyFiles.cmd")
    pool.StartTask <- st
    pool.MaxTasksPerVM <- Nullable(numtaskspervm)
    pool.Commit()

let createTasks (poolname:string) workitemname cmdfile =
    use client = BatchClient.Connect(Url,batchcred)
    let toolbox = client.OpenToolbox()
    use wm = client.OpenWorkItemManager()
    let tsh = toolbox.CreateTaskSubmissionHelper(wm,poolname)
    tsh.WorkItemName <- workitemname
    let exeName = @"%WATASK_TVM_ROOT_DIR%\shared\TitanSpikeClusterer.exe"
    let tempPath = @"%WATASK_TVM_ROOT_DIR%\workitems\%WATASK_WORKITEM_NAME%\%WATASK_JOB_NAME%\%WATASK_TASK_NAME%\temp"
    File.ReadAllLines(cmdfile) |> Array.iteri (fun i cmd ->
        let cmd = 
            sprintf @"cmd /c %s ClusterWorkerBlockRange %s %s"
                exeName tempPath cmd
        use task = new CloudTask(sprintf "Task%d" i,cmd)
        tsh.AddTask(task) |> ignore    
    )
    tsh.Commit() |> ignore
//createClusterPool "ClusterPool1" "large" 1 6
createTasks "ClusterPool1" "Jaunpuri" @"D:\Rajesh\testcmds"