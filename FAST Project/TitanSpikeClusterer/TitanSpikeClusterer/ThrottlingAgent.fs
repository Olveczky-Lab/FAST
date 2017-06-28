module ThrottlingAgent

/// Message type used by the agent - contains queueing 
/// of work items and notification of completion 
type ThrottlingAgentMessage<'T,'S,'U> = 
  | Completed
  | WorkParallel of Async<unit>
  | WorkSerial of 'T
  | WorkSerialWithReply of 'S
  | WaitForCompletion of 'U
    
/// Represents an agent that runs operations in concurrently. When the number
/// of concurrent operations exceeds 'limit', they are queued and processed later
let ThrottlingAgent limit initstate = 
  MailboxProcessor.Start(fun agent -> 

    /// Represents a state when the agent is blocked
    let rec waiting state terminator = 
      // Use 'Scan' to wait for completion of some work
      agent.Scan(function
        | Completed -> Some(working (limit - 1) state terminator)
        | _ -> None)

    /// Represents a state when the agent is working        
    and working count state terminator = async { 
      match terminator with
      | None -> ()
      | Some (endfunc,reply:AsyncReplyChannel<_>) -> if (endfunc state) then reply.Reply()
      // Receive any message 
      let! msg = agent.Receive()
      match msg with 
      | Completed -> 
          // Decrement the counter of work items
          return! working (count - 1) state terminator
      | WorkParallel work ->
          // Start the work item & continue in blocked/working state
          async { try do! work 
                  finally agent.Post(Completed) }
          |> Async.Start
          if (count+1 < limit) then return! working (count + 1) state terminator
          else return! waiting state terminator
      | WorkSerial func ->
          let newstate = func state
          return! working count newstate terminator
      | WorkSerialWithReply (func,curreply:AsyncReplyChannel<_>) ->
          let newstate,r = func state
          curreply.Reply(r)
          return! working count newstate terminator
      | WaitForCompletion r ->
          return! working count state (Some r)
      }
      

    // Start in working state with zero running work items
    working 0 initstate None)