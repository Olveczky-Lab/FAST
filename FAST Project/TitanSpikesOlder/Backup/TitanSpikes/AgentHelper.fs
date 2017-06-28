module AgentHelper

type Agent<'T> = MailboxProcessor<'T>
 
type agentProcessorMsg<'a> =
    | ProcessData of 'a
    | CompletedProcessing
    | GetNumInProgress of AsyncReplyChannel<int>

let agentProcessor () =
    Agent.Start(fun inbox ->
            let rec loop numinprogress = async {
                let! msg = inbox.Receive()
                match msg with
                | ProcessData dataProcessor -> 
                    Async.Start dataProcessor
                    return! loop (numinprogress+1)
                | CompletedProcessing -> 
                    return! loop (numinprogress-1)
                | GetNumInProgress reply -> 
                    reply.Reply numinprogress
                    return! loop numinprogress
            }
            loop 0 )

let agentSerial initState =
    Agent.Start(fun inbox ->
            let rec loop state = async {
                let! (func,reply:AsyncReplyChannel<_>) = inbox.Receive()
                let (newstate,r) = func state
                reply.Reply r
                do! loop newstate
            }
            loop initState )

let rec AsyncLoopUntil delay completed =
    async {
        if (not (completed ())) then
            do! Async.Sleep delay
            do! AsyncLoopUntil delay completed
    }

