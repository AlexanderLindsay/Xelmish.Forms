module Xelmish.Queue

open System.Collections.Generic

type private Msg<'T> =
    | Enqueue of 'T
    | Dequeue of AsyncReplyChannel<'T>
    | HasValues of AsyncReplyChannel<bool>
    | Clear

type queue<'T>() =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec loop (queue: Queue<'T>) = async {
            let! msg = inbox.Receive()

            match msg with
                | Enqueue item ->
                    queue.Enqueue (item)
                    return! loop queue
                | Dequeue reply ->
                    reply.Reply <| queue.Dequeue ()
                    return! loop queue
                | HasValues reply ->
                    reply.Reply <| (queue.Count > 0)
                    return! loop queue
                | Clear ->
                    queue.Clear ()
                    return! loop queue
        }
        loop <| Queue<'T>()
    )

    let enqueue item =
        agent.Post <| Enqueue item

    let dequeue () =
        agent.PostAndReply Dequeue
    
    let hasValues () =
        agent.PostAndReply HasValues
    
    let clear () =
        agent.Post Clear
    
    member __.Enqueue item = enqueue item
    member __.Dequeue () = dequeue ()
    member __.HasValues () = hasValues ()
    member __.Clear () = clear ()