module Subject

open System
open System.Collections.Generic

type Subject<'T> () =
   let sync = obj()
   let mutable stopped = false
   let observers = List<IObserver<'T>>()
   let iter f = observers |> Seq.iter f
   let onCompleted () =
      if not stopped then
         stopped <- true
         iter (fun observer -> observer.OnCompleted())
   let onError ex () =
      if not stopped then
         stopped <- true
         iter (fun observer -> observer.OnError(ex))
   let next value () =
      if not stopped then
         iter (fun observer -> observer.OnNext(value))
   let remove observer () =
      observers.Remove observer |> ignore
   member x.Next value = lock sync <| next value
   member x.Error ex = lock sync <| onError ex
   member x.Completed () = lock sync <| onCompleted
   interface IObserver<'T> with
      member x.OnCompleted() = x.Completed()
      member x.OnError ex = x.Error ex
      member x.OnNext value = x.Next value
   interface IObservable<'T> with
      member this.Subscribe(observer:IObserver<'T>) =
         observers.Add observer
         { new IDisposable with
            member this.Dispose() =
               lock sync <| remove observer
         }
