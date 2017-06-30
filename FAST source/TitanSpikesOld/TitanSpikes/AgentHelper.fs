module AgentHelper

type Agent<'T> = MailboxProcessor<'T>
 
type SerialMsg<'T,'U,'S> =
| ProcessData of 'T
| ProcessDataWithReply of 'S
| WaitForCompletion of 'U

let agentSerial initState =
    Agent.Start(fun inbox ->
        let rec loop state internalstate = async {
            match internalstate with
                | Some (endfunc,reply:AsyncReplyChannel<_>) -> if (endfunc state) then reply.Reply()
                | None -> ()
            let! msg = inbox.Receive()
            match msg with
            | ProcessData func ->
                let newstate = func state
                return! loop newstate internalstate
            | ProcessDataWithReply (func,curreply:AsyncReplyChannel<_>) ->
                let (newstate,r) = func state
                curreply.Reply(r)
                return! loop newstate internalstate
            | WaitForCompletion r ->
                return! loop state (Some r)
        }
        loop initState None)