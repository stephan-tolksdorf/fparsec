// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

[<AutoOpen>]
module FParsec.Primitives

open FParsec.Internals
open FParsec.Error

[<Literal>]
let Ok         = ReplyStatus.Ok
[<Literal>]
let Error      = ReplyStatus.Error
[<Literal>]
let FatalError = ReplyStatus.FatalError

type Parser<'a, 'u> = CharStream<'u> -> Reply<'a>

// The `PrimitiveTests.Reference` module contains simple (but inefficient)
// reference implementations of most of the functions below.

// =================================
// Parser primitives and combinators
// =================================

let preturn x : Parser<_,_> = fun stream -> Reply(x)
let pzero : Parser<_,_> = fun stream -> Reply()

// ---------------------------
// Chaining and piping parsers
// ---------------------------

let (>>=) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) =
    match box f with
    // optimization for uncurried functions
    | :? OptimizedClosures.FSharpFunc<'a, CharStream<'u>, Reply<'b>> as optF ->
        fun stream ->
            let reply1 = p stream
            if reply1.Status = Ok then
                if isNull reply1.Error then
                    // in separate branch because the JIT can produce better code for a tail call
                    optF.Invoke(reply1.Result, stream)
                else
                    let stateTag1 = stream.StateTag
                    let mutable reply2 = optF.Invoke(reply1.Result, stream)
                    if stateTag1 = stream.StateTag then
                        reply2.Error <- mergeErrors reply2.Error reply1.Error
                    reply2
            else
                Reply(reply1.Status, reply1.Error)
    | _ ->
        fun stream ->
            let reply1 = p stream
            if reply1.Status = Ok then
                let p2 = f reply1.Result
                if isNull reply1.Error then
                    // in separate branch because the JIT can produce better code for a tail call
                    p2 stream
                else
                    let stateTag1 = stream.StateTag
                    let mutable reply2 = p2 stream
                    if stateTag1 = stream.StateTag then
                        reply2.Error <- mergeErrors reply2.Error reply1.Error
                    reply2
            else
                Reply(reply1.Status, reply1.Error)

let (>>%) (p: Parser<'a,'u>) x =
    fun stream ->
        let reply = p stream
        Reply(reply.Status, x, reply.Error)

let (>>.) (p: Parser<'a,'u>) (q: Parser<'b,'u>) =
    fun stream ->
        let mutable reply1 = p stream
        if reply1.Status = Ok then
            if isNull reply1.Error then
                // in separate branch because the JIT can produce better code for a tail call
                q stream
            else
                let stateTag1 = stream.StateTag
                let mutable reply2 = q stream
                if stateTag1 = stream.StateTag then
                    reply2.Error <- mergeErrors reply2.Error reply1.Error
                reply2
        else
            Reply(reply1.Status, reply1.Error)

let (.>>) (p: Parser<'a,'u>) (q: Parser<'b,'u>) =
    fun stream ->
        let mutable reply1 = p stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = q stream
            let error = if isNull reply1.Error then reply2.Error
                        elif stateTag1 <> stream.StateTag then reply2.Error
                        else mergeErrors reply2.Error reply1.Error
            reply1.Error  <- error
            reply1.Status <- reply2.Status
        reply1


let (.>>.) (p: Parser<'a,'u>) (q: Parser<'b,'u>) =
    fun stream ->
        let reply1 = p stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = q stream
            let error = if stateTag1 <> stream.StateTag then reply2.Error
                        else mergeErrors reply1.Error reply2.Error
            let result = if reply2.Status = Ok then (reply1.Result, reply2.Result)
                         else Unchecked.defaultof<_>
            Reply(reply2.Status, result, error)
        else
            Reply(reply1.Status, reply1.Error)

let between (popen: Parser<_,'u>) (pclose: Parser<_,'u>) (p: Parser<_,'u>) =
    fun stream ->
        let reply1 = popen stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let mutable reply2 = p stream
            if reply2.Status = Ok then
                let stateTag2 = stream.StateTag
                let reply3 = pclose stream
                let error = if stateTag2 <> stream.StateTag then reply3.Error
                            else
                                let error2 = mergeErrors reply2.Error reply3.Error
                                if stateTag1 <> stateTag2 then error2
                                else mergeErrors reply1.Error error2
                reply2.Error  <- error
                reply2.Status <- reply3.Status
                reply2
            else
                let error = if stateTag1 <> stream.StateTag then reply2.Error
                            else mergeErrors reply1.Error reply2.Error
                reply2.Error <- error
                reply2
        else
            Reply(reply1.Status, reply1.Error)

let (|>>) (p: Parser<'a,'u>) f =
    fun stream ->
        let reply = p stream
        Reply(reply.Status,
              (if reply.Status = Ok then f reply.Result else Unchecked.defaultof<_>),
              reply.Error)

let pipe2 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
    fun stream ->
        let mutable reply = Reply()
        let reply1 = p1 stream
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = p2 stream
            error <- if stateTag1 <> stream.StateTag then reply2.Error
                     else mergeErrors error reply2.Error
            if reply2.Status = Ok then
                 reply.Result <- optF.Invoke(reply1.Result, reply2.Result)
                 reply.Status <- Ok
            else reply.Status <- reply2.Status
        else reply.Status <- reply1.Status
        reply.Error <- error
        reply

let pipe3 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) (p3: Parser<'c,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
    fun stream ->
        let mutable reply = Reply()
        let reply1 = p1 stream
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = p2 stream
            error <- if stateTag1 <> stream.StateTag then reply2.Error
                     else mergeErrors error reply2.Error
            if reply2.Status = Ok then
                let stateTag2 = stream.StateTag
                let reply3 = p3 stream
                error <- if stateTag2 <> stream.StateTag then reply3.Error
                         else mergeErrors error reply3.Error
                if reply3.Status = Ok then
                     reply.Result <- optF.Invoke(reply1.Result, reply2.Result, reply3.Result)
                     reply.Status <- Ok
                else reply.Status <- reply3.Status
            else reply.Status <- reply2.Status
        else reply.Status <- reply1.Status
        reply.Error <- error
        reply

let pipe4 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) (p3: Parser<'c,'u>) (p4: Parser<'d,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_,_,_>.Adapt(f)
    fun stream ->
        let mutable reply = Reply()
        let reply1 = p1 stream
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = p2 stream
            error <- if stateTag1 <> stream.StateTag then reply2.Error
                     else mergeErrors error reply2.Error
            if reply2.Status = Ok then
                let stateTag2 = stream.StateTag
                let reply3 = p3 stream
                error <- if stateTag2 <> stream.StateTag then reply3.Error
                         else mergeErrors error reply3.Error
                if reply3.Status = Ok then
                    let stateTag3 = stream.StateTag
                    let reply4 = p4 stream
                    error <- if stateTag3 <> stream.StateTag then reply4.Error
                             else mergeErrors error reply4.Error
                    if reply4.Status = Ok then
                         reply.Result <- optF.Invoke(reply1.Result, reply2.Result, reply3.Result, reply4.Result)
                         reply.Status <- Ok
                    else reply.Status <- reply4.Status
                else reply.Status <- reply3.Status
            else reply.Status <- reply2.Status
        else reply.Status <- reply1.Status
        reply.Error <- error
        reply

let pipe5 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) (p3: Parser<'c,'u>) (p4: Parser<'d,'u>) (p5: Parser<'e,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt(f)
    fun stream ->
        let mutable reply = Reply()
        let reply1 = p1 stream
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = p2 stream
            error <- if stateTag1 <> stream.StateTag then reply2.Error
                     else mergeErrors error reply2.Error
            if reply2.Status = Ok then
                let stateTag2 = stream.StateTag
                let reply3 = p3 stream
                error <- if stateTag2 <> stream.StateTag then reply3.Error
                         else mergeErrors error reply3.Error
                if reply3.Status = Ok then
                    let stateTag3 = stream.StateTag
                    let reply4 = p4 stream
                    error <- if stateTag3 <> stream.StateTag then reply4.Error
                             else mergeErrors error reply4.Error
                    if reply4.Status = Ok then
                        let stateTag4 = stream.StateTag
                        let reply5 = p5 stream
                        error <- if stateTag4 <> stream.StateTag then reply5.Error
                                 else mergeErrors error reply5.Error
                        if reply5.Status = Ok then
                             reply.Result <- optF.Invoke(reply1.Result, reply2.Result, reply3.Result, reply4.Result, reply5.Result)
                             reply.Status <- Ok
                        else reply.Status <- reply5.Status
                    else reply.Status <- reply4.Status
                else reply.Status <- reply3.Status
            else reply.Status <- reply2.Status
        else reply.Status <- reply1.Status
        reply.Error <- error
        reply


// -----------------------------------------------
// Parsing alternatives and recovering from errors
// -----------------------------------------------

let (<|>) (p1: Parser<'a,'u>) (p2: Parser<'a,'u>) : Parser<'a,'u> =
    fun stream ->
        let mutable stateTag = stream.StateTag
        let mutable reply = p1 stream
        if reply.Status = Error && stateTag = stream.StateTag then
            let error = reply.Error
            reply <- p2 stream
            if stateTag = stream.StateTag then
                reply.Error <- mergeErrors reply.Error error
        reply

let choice (ps: seq<Parser<'a,'u>>)  =
    match ps with
    | :? (Parser<'a,'u>[]) as ps ->
        if ps.Length = 0 then pzero
        else
            fun stream ->
                let stateTag = stream.StateTag
                let mutable error = NoErrorMessages
                let mutable reply = ps.[0] stream
                let mutable i = 1
                while reply.Status = Error && stateTag = stream.StateTag && i < ps.Length do
                    error <- mergeErrors error reply.Error
                    reply <- ps.[i] stream
                    i <- i + 1
                if stateTag = stream.StateTag then
                    error <- mergeErrors error reply.Error
                    reply.Error <- error
                reply
    | :? (Parser<'a,'u> list) as ps ->
        match ps with
        | [] -> pzero
        | hd::tl ->
            fun stream ->
                let stateTag = stream.StateTag
                let mutable error = NoErrorMessages
                let mutable hd, tl = hd, tl
                let mutable reply = hd stream
                while reply.Status = Error && stateTag = stream.StateTag
                      && (match tl with
                          | h::t -> hd <- h; tl <- t; true
                          | _ -> false)
                   do
                    error <- mergeErrors error reply.Error
                    reply <- hd stream
                if stateTag = stream.StateTag then
                    error <- mergeErrors error reply.Error
                    reply.Error <- error
                reply
    | _ -> fun stream ->
               use iter = ps.GetEnumerator()
               if iter.MoveNext() then
                   let stateTag = stream.StateTag
                   let mutable error = NoErrorMessages
                   let mutable reply = iter.Current stream
                   while reply.Status = Error && stateTag = stream.StateTag && iter.MoveNext() do
                       error <- mergeErrors error reply.Error
                       reply <- iter.Current stream
                   if stateTag = stream.StateTag then
                       error <- mergeErrors error reply.Error
                       reply.Error <- error
                   reply
               else
                   Reply()


let choiceL (ps: seq<Parser<'a,'u>>) label : Parser<_,_> =
    let error = expected label
    match ps with
    | :? (Parser<'a,'u>[]) as ps ->
        if ps.Length = 0 then
            fun stream -> Reply(Error, error)
        else
            fun stream ->
                let stateTag = stream.StateTag
                let mutable reply = ps.[0] stream
                let mutable i = 1
                while reply.Status = Error && stateTag = stream.StateTag && i < ps.Length do
                    reply <- ps.[i] stream
                    i <- i + 1
                if stateTag = stream.StateTag then
                    reply.Error <- error
                reply
    | :? (Parser<'a,'u> list) as ps ->
        match ps with
        | [] -> fun stream -> Reply(Error, error)
        | hd::tl ->
            fun stream ->
                let stateTag = stream.StateTag
                let mutable hd, tl = hd, tl
                let mutable reply = hd stream
                while reply.Status = Error && stateTag = stream.StateTag
                      && (match tl with
                          | h::t -> hd <- h; tl <- t; true
                          | _ -> false)
                   do
                    reply <- hd stream
                if stateTag = stream.StateTag then
                    reply.Error <- error
                reply
    | _ -> fun stream ->
               use iter = ps.GetEnumerator()
               if iter.MoveNext() then
                   let stateTag = stream.StateTag
                   let mutable reply = iter.Current stream
                   while reply.Status = Error && stateTag = stream.StateTag && iter.MoveNext() do
                       reply <- iter.Current stream
                   if stateTag = stream.StateTag then
                       reply.Error <- error
                   reply
               else
                   Reply(Error, error)

let (<|>%) (p: Parser<'a,'u>) x : Parser<'a,'u> =
    fun stream ->
        let stateTag = stream.StateTag
        let mutable reply = p stream
        if reply.Status = Error && stateTag = stream.StateTag then
            reply.Result <- x
            reply.Status <- Ok
        reply

let opt (p: Parser<'a,'u>) : Parser<'a option,'u> =
    fun stream ->
        let stateTag = stream.StateTag
        let reply = p stream
        if reply.Status = Ok then
            Reply(Ok, Some reply.Result, reply.Error)
        else
            // None is represented as null
            let status = if reply.Status = Error && stateTag = stream.StateTag then Ok else reply.Status
            Reply(status, reply.Error)

let optional (p: Parser<'a,'u>) : Parser<unit,'u> =
    fun stream ->
        let stateTag = stream.StateTag
        let reply = p stream
        let status = if reply.Status = Error && stateTag = stream.StateTag then Ok else reply.Status
        Reply(status, (), reply.Error)

let attempt (p: Parser<'a,'u>) : Parser<'a,'u> =
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let mutable reply = p stream
        if reply.Status <> Ok then
            if state.Tag <> stream.StateTag then
                reply.Error  <- nestedError stream reply.Error
                reply.Status <- Error // turns FatalErrors into Errors
                stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
            elif reply.Status = FatalError then
                reply.Status <- Error
        reply

let (>>=?) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) : Parser<'b,'u> =
    let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let reply1 = p stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let mutable reply2 = optF.Invoke(reply1.Result, stream)
            if stateTag1 = stream.StateTag then
                let error = mergeErrors reply1.Error reply2.Error
                if reply2.Status <> Error || stateTag1 = state.Tag then
                    reply2.Error <- error
                else
                    reply2.Error <- nestedError stream error
                    stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
            reply2
        else
            Reply(reply1.Status, reply1.Error)

let (>>?) (p: Parser<'a,'u>) (q: Parser<'b,'u>) : Parser<'b,'u> =
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let reply1 = p stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let mutable reply2 = q stream
            if stateTag1 = stream.StateTag then
                let error = mergeErrors reply1.Error reply2.Error
                if reply2.Status <> Error || stateTag1 = state.Tag then
                    reply2.Error <- error
                else
                    reply2.Error <- nestedError stream error
                    stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
            reply2
        else
            Reply(reply1.Status, reply1.Error)

let (.>>.?) (p: Parser<'a,'u>) (q: Parser<'b,'u>) : Parser<'a*'b,'u> =
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let reply1 = p stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let mutable reply2 = q stream
            if stateTag1 = stream.StateTag then
                let error = mergeErrors reply1.Error reply2.Error
                if reply2.Status <> Error || stateTag1 = state.Tag then
                    reply2.Error <- error
                else
                    reply2.Error <- nestedError stream error
                    stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
            let result = if reply2.Status = Ok then (reply1.Result, reply2.Result)
                         else Unchecked.defaultof<_>
            Reply(reply2.Status, result, reply2.Error)
        else
            Reply(reply1.Status, reply1.Error)

let (.>>?) (p: Parser<'a,'u>) (q: Parser<'b,'u>) : Parser<'a,'u> =
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let mutable reply1 = p stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = q stream
            if stateTag1 = stream.StateTag then
                let error = mergeErrors reply1.Error reply2.Error
                if reply2.Status <> Error || stateTag1 = state.Tag then
                    reply1.Error <- error
                    reply1.Status <- reply2.Status
                else
                    reply1.Error <- nestedError stream error
                    stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
                    reply1.Status <- Error
            else
                reply1.Error  <- reply2.Error
                reply1.Status <- reply2.Status
        reply1


// -------------------------------------
// Conditional parsing and looking ahead
// -------------------------------------

let notEmpty (p: Parser<'a,'u>) : Parser<'a,'u> =
    fun stream ->
        let stateTag = stream.StateTag
        let mutable reply = p stream
        if stateTag = stream.StateTag && reply.Status = Ok then
            reply.Status <- Error
        reply

// REVIEW: should `followedBy` use the error messages generated by `p`?

let internal followedByE (p: Parser<'a,'u>) error : Parser<unit,'u> =
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let reply = p stream
        if state.Tag <> stream.StateTag then
            stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
        if reply.Status = Ok then Reply(())
        else Reply(Error, error)

let followedBy  p       = followedByE p NoErrorMessages
let followedByL p label = followedByE p (expected label)

let internal notFollowedByE (p: Parser<'a,'u>) error : Parser<unit,'u> =
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let reply = p stream
        if state.Tag <> stream.StateTag then
            stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
        if reply.Status <> Ok then Reply(())
        else Reply(Error, error)

let notFollowedBy  p       = notFollowedByE p NoErrorMessages
let notFollowedByL p label = notFollowedByE p (unexpected label)

let lookAhead (p: Parser<'a,'u>) : Parser<'a,'u> =
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let mutable reply = p stream
        if reply.Status = Ok then
            reply.Error <- NoErrorMessages
            if state.Tag <> stream.StateTag then
                stream.BacktrackTo(&state) // passed by ref as a (slight) optimization
        else
            if state.Tag <> stream.StateTag then
                reply.Error  <- nestedError stream reply.Error
                stream.BacktrackTo(&state)
            reply.Status <- Error // turn FatalErrors into normal Errors
        reply


// --------------------------
// Customizing error messages
// --------------------------

let (<?>) (p: Parser<'a,'u>) label  : Parser<'a,'u> =
    let error = expected label
    fun stream ->
        let stateTag = stream.StateTag
        let mutable reply = p stream
        if stateTag = stream.StateTag then
            reply.Error <- error
        reply

let (<??>) (p: Parser<'a,'u>) label : Parser<'a,'u> =
    let expErr = expected label
    fun stream ->
        // state is only declared mutable so it can be passed by ref, it won't be mutated
        let mutable state = CharStreamState(stream) // = stream.State (manually inlined)
        let mutable reply = p stream
        if reply.Status = Ok then
            if state.Tag = stream.StateTag then
                reply.Error <- expErr
        else
            if state.Tag = stream.StateTag then
                (*
                // manually inlined:
                let error = match reply.Error with
                            | ErrorMessageList(NestedError(pos, userState, msgs), NoErrorMessages)
                                -> ErrorMessageList(CompoundError(label, pos, userState, msgs), NoErrorMessages)
                            | _ -> expErr
                *)
                let error = if reply.Error |> isSingleErrorMessageOfType ErrorMessageType.NestedError then
                                let ne = reply.Error.Head :?> NestedError
                                ErrorMessageList(CompoundError(label, ne.Position, ne.UserState, ne.Messages))
                            else expErr
                reply.Error <- error
            else
                reply.Error  <- compoundError label stream reply.Error
                stream.BacktrackTo(&state) // we backtrack ...
                reply.Status <- FatalError // ... so we need to make sure normal parsing doesn't continue
        reply

let fail msg : Parser<'a,'u> =
    let error = messageError msg
    fun stream -> Reply(Error, error)

let failFatally msg : Parser<'a,'u> =
    let error = messageError msg
    fun stream -> Reply(FatalError, error)

// -----------------
// Parsing sequences
// -----------------

let tuple2 p1 p2          = p1 .>>. p2
let tuple3 p1 p2 p3       = pipe3 p1 p2 p3       (fun a b c     -> (a, b, c))
let tuple4 p1 p2 p3 p4    = pipe4 p1 p2 p3 p4    (fun a b c d   -> (a, b, c, d))
let tuple5 p1 p2 p3 p4 p5 = pipe5 p1 p2 p3 p4 p5 (fun a b c d e -> (a, b, c, d, e))

let parray n (p: Parser<'a,'u>) =
    if n = 0 then preturn [||]
    else
        fun stream ->
            let mutable reply = p stream
            let mutable error = reply.Error
            let mutable newReply = Reply()
            if reply.Status = Ok then
                let mutable xs = Array.zeroCreate n
                xs.[0] <- reply.Result
                let mutable i = 1
                while i < n do
                    let mutable stateTag = stream.StateTag
                    reply <- p stream
                    error <- if stateTag <> stream.StateTag then reply.Error
                             else mergeErrors error reply.Error
                    if reply.Status = Ok then
                        xs.[i] <- reply.Result
                        i <- i + 1
                    else
                        i <- n // break
                newReply.Result <- xs // we set the result even if there was an error
            newReply.Error  <- error
            newReply.Status <- reply.Status
            newReply

let skipArray n (p: Parser<'a,'u>) =
    if n = 0 then preturn ()
    else
        fun stream ->
            let mutable reply = p stream
            let mutable error = reply.Error
            let mutable newReply = Reply()
            if reply.Status = Ok then
                 let mutable i = 1
                 while i < n do
                     let mutable stateTag = stream.StateTag
                     reply <- p stream
                     error <- if stateTag <> stream.StateTag then reply.Error
                              else mergeErrors error reply.Error
                     if reply.Status = Ok then
                         i <- i + 1
                     else
                         i <- n // break
                // () is represented as null
            newReply.Error  <- error
            newReply.Status <- reply.Status
            newReply

[<Sealed>]
type Inline =

#if NOINLINE
  static member
#else
  [<NoDynamicInvocation>]
  static member inline
#endif
                       Many(stateFromFirstElement,
                            foldState,
                            resultFromState,
                            elementParser: Parser<_,_>,
                            ?firstElementParser: Parser<_,_>,
                            ?resultForEmptySequence) : Parser<_,_> =
      fun stream ->
        let mutable stateTag = stream.StateTag
        let firstElementParser = match firstElementParser with Some p -> p | _ -> elementParser
        let mutable reply = firstElementParser stream
        if reply.Status = Ok then
            let mutable xs = stateFromFirstElement reply.Result
            let mutable error = reply.Error
            stateTag <- stream.StateTag
            reply <- elementParser stream
            while reply.Status = Ok do
                if stateTag = stream.StateTag then
                    raiseInfiniteLoopException "many" stream
                xs    <- foldState xs reply.Result
                error <- reply.Error
                stateTag <- stream.StateTag
                reply <- elementParser stream
            if reply.Status = Error && stateTag = stream.StateTag then
                error <- mergeErrors error reply.Error
                Reply(Ok, resultFromState xs, error)
            else
                error <- if stateTag <> stream.StateTag then reply.Error
                         else mergeErrors error reply.Error
                Reply(reply.Status, error)
        else
            match resultForEmptySequence with
            | Some _ (* if we bind f here, fsc won't be able to inline it *)
              when reply.Status = Error && stateTag = stream.StateTag ->
                Reply(Ok, (match resultForEmptySequence with Some f -> f() | _ -> Unchecked.defaultof<_>), reply.Error)
            | _ ->
                Reply(reply.Status, reply.Error)

#if NOINLINE
  static member
#else
  [<NoDynamicInvocation>]
  static member inline
#endif
                       SepBy(stateFromFirstElement,
                             foldState,
                             resultFromState,
                             elementParser: Parser<_,_>,
                             separatorParser: Parser<_,_>,
                             ?firstElementParser: Parser<_,'u>,
                             ?resultForEmptySequence,
                             ?separatorMayEndSequence) : Parser<_,'u> =
      fun stream ->
        let mutable stateTag = stream.StateTag
        let firstElementParser = match firstElementParser with Some p -> p | _ -> elementParser
        let mutable reply = firstElementParser stream
        if reply.Status = Ok then
            let mutable xs    = stateFromFirstElement reply.Result
            let mutable error = reply.Error
            stateTag <- stream.StateTag
            let mutable sepReply = separatorParser stream
            let mutable sepStateTag = stream.StateTag
            while sepReply.Status = Ok && (reply <- elementParser stream; reply.Status = Ok) do
                xs <- foldState xs sepReply.Result reply.Result
                if sepStateTag <> stream.StateTag then
                    error <- reply.Error
                elif stateTag <> sepStateTag then
                    error <- mergeErrors sepReply.Error reply.Error
                else
                    raiseInfiniteLoopException "sep(End)By" stream
                stateTag <- stream.StateTag
                sepReply <- separatorParser stream
                sepStateTag <- stream.StateTag
            if sepReply.Status = Error && stateTag = sepStateTag then
                Reply(Ok, resultFromState xs, mergeErrors error sepReply.Error)
            else
                match separatorMayEndSequence with
                | Some true when reply.Status = Error && sepStateTag = stream.StateTag ->
                    error <- mergeErrors (if stateTag <> sepStateTag then sepReply.Error
                                          else mergeErrors error sepReply.Error) reply.Error
                    Reply(Ok, resultFromState xs, error)
                | _ when reply.Status <> Ok ->
                    error <- if sepStateTag <> stream.StateTag then reply.Error
                             else
                                let error2 = mergeErrors sepReply.Error reply.Error
                                if stateTag <> sepStateTag then error2
                                else mergeErrors error error2
                    Reply(reply.Status, error)
                | _ ->
                    let error = if stateTag <> sepStateTag then sepReply.Error
                                else mergeErrors error sepReply.Error
                    Reply(sepReply.Status, error)
        else
            match resultForEmptySequence with
            | Some _ (* if we bind f here, fsc won't be able to inline it *)
              when reply.Status = Error && stateTag = stream.StateTag ->
                Reply(Ok, (match resultForEmptySequence with Some f -> f() | _ -> Unchecked.defaultof<_>), reply.Error)
            | _ ->
                Reply(reply.Status, reply.Error)

#if NOINLINE
  static member
#else
  [<NoDynamicInvocation>]
  static member inline
#endif
                       ManyTill(stateFromFirstElement,
                                foldState,
                                resultFromStateAndEndParserResult,
                                elementParser: Parser<_,_>,
                                endParser: Parser<_,_>,
                                ?firstElementParser: Parser<_,_>,
                                ?resultForEmptySequence) : Parser<_,_> =
      fun stream ->
        // This is really, really ugly, but it does the job,
        // and it does it about as efficient as it can be done here.
        let firstElementParser = match firstElementParser with Some p -> p | _ -> elementParser
        match resultForEmptySequence with
        | None -> // require at least one element
                let mutable reply = firstElementParser stream
                if reply.Status = Ok then
                    // ------------------------------------------------------------------
                    // the following code is duplicated in the match branch below
                    let mutable xs = stateFromFirstElement reply.Result
                    let mutable error = reply.Error
                    let mutable stateTag = stream.StateTag
                    let mutable endReply = endParser stream
                    while endReply.Status = Error && stateTag = stream.StateTag do
                        endReply.Status <- enum System.Int32.MinValue
                        reply <- elementParser stream
                        if reply.Status = Ok then
                            if stateTag = stream.StateTag then
                                raiseInfiniteLoopException "manyTill" stream
                            xs <- foldState xs reply.Result
                            error <- reply.Error
                            stateTag <- stream.StateTag
                            endReply <- endParser stream
                    if endReply.Status = Ok then
                        error <- if stateTag <> stream.StateTag then endReply.Error
                                 else mergeErrors error endReply.Error
                        Reply(Ok, resultFromStateAndEndParserResult xs endReply.Result, error)
                    elif endReply.Status = enum System.Int32.MinValue then
                        error <- if stateTag <> stream.StateTag then reply.Error
                                 else mergeErrors (mergeErrors error endReply.Error) reply.Error
                        Reply(reply.Status, error)
                    else
                        error <- if stateTag <> stream.StateTag then endReply.Error
                                 else mergeErrors error endReply.Error
                        Reply(endReply.Status, error)
                    // ------------------------------------------------------------------
                else
                    Reply(reply.Status, reply.Error)
        | Some _ ->
            let mutable stateTag = stream.StateTag
            let mutable endReply = endParser stream
            if endReply.Status = Error && stateTag = stream.StateTag then
                let mutable reply = firstElementParser stream
                if reply.Status = Ok then
                    // ------------------------------------------------------------------
                    // the following code is duplicated in the match branch above
                    let mutable xs = stateFromFirstElement reply.Result
                    let mutable error = reply.Error
                    stateTag <- stream.StateTag
                    endReply <- endParser stream
                    while endReply.Status = Error && stateTag = stream.StateTag do
                        endReply.Status <- enum System.Int32.MinValue
                        reply <- elementParser stream
                        if reply.Status = Ok then
                            if stateTag = stream.StateTag then
                                raiseInfiniteLoopException "manyTill" stream
                            xs <- foldState xs reply.Result
                            error <- reply.Error
                            stateTag <- stream.StateTag
                            endReply <- endParser stream
                    if endReply.Status = Ok then
                        error <- if stateTag <> stream.StateTag then endReply.Error
                                 else mergeErrors error endReply.Error
                        Reply(Ok, resultFromStateAndEndParserResult xs endReply.Result, error)
                    elif endReply.Status = enum System.Int32.MinValue then
                        error <- if stateTag <> stream.StateTag then reply.Error
                                 else mergeErrors (mergeErrors error endReply.Error) reply.Error
                        Reply(reply.Status, error)
                    else
                        error <- if stateTag <> stream.StateTag then endReply.Error
                                 else mergeErrors error endReply.Error
                        Reply(endReply.Status, error)
                    // ------------------------------------------------------------------
                else
                    let error = if stateTag <> stream.StateTag then reply.Error
                                 else mergeErrors endReply.Error reply.Error
                    Reply(reply.Status, error)
            elif endReply.Status = Ok then
                Reply(Ok, (match resultForEmptySequence with Some f -> f endReply.Result | _ -> Unchecked.defaultof<_>), endReply.Error)
            else
                Reply(endReply.Status, endReply.Error)

let many      p = Inline.Many((fun x -> [x]), (fun xs x -> x::xs), List.rev, p, resultForEmptySequence = fun () -> [])
let many1     p = Inline.Many((fun x -> [x]), (fun xs x -> x::xs), List.rev, p)

let skipMany  p = Inline.Many((fun _ -> ()), (fun _ _ -> ()), (fun xs -> xs), p, resultForEmptySequence = fun () -> ())
let skipMany1 p = Inline.Many((fun _ -> ()), (fun _ _ -> ()), (fun xs -> xs), p)

let sepBy         p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), List.rev,       p, sep, resultForEmptySequence = fun () -> [])
let sepBy1        p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), List.rev,       p, sep)

let skipSepBy     p sep = Inline.SepBy((fun _ -> ()),  (fun _ _ _ -> ()),     (fun xs -> xs), p, sep, resultForEmptySequence = fun () -> ())
let skipSepBy1    p sep = Inline.SepBy((fun _ -> ()),  (fun _ _ _ -> ()),     (fun xs -> xs), p, sep)

let sepEndBy      p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), List.rev,       p, sep, separatorMayEndSequence = true, resultForEmptySequence = fun () -> [])
let sepEndBy1     p sep = Inline.SepBy((fun x -> [x]), (fun xs _ x -> x::xs), List.rev,       p, sep, separatorMayEndSequence = true)

let skipSepEndBy  p sep = Inline.SepBy((fun _ -> ()),  (fun _ _ _ -> ()),     (fun xs -> xs), p, sep, separatorMayEndSequence = true, resultForEmptySequence = fun () -> ())
let skipSepEndBy1 p sep = Inline.SepBy((fun _ -> ()),  (fun _ _ _ -> ()),     (fun xs -> xs), p, sep, separatorMayEndSequence = true)

let manyTill       p endp = Inline.ManyTill((fun x -> [x]), (fun xs x -> x::xs), (fun xs _ -> List.rev xs), p, endp, resultForEmptySequence = fun _ -> [])
let many1Till      p endp = Inline.ManyTill((fun x -> [x]), (fun xs x -> x::xs), (fun xs _ -> List.rev xs), p, endp)

let skipManyTill   p endp = Inline.ManyTill((fun _ -> ()),  (fun _ _ -> ()),     (fun _ _ -> ()), p, endp, resultForEmptySequence = fun _ -> ())
let skipMany1Till  p endp = Inline.ManyTill((fun _ -> ()),  (fun _ _ -> ()),     (fun _ _ -> ()), p, endp)

let chainl1 p op =
    Inline.SepBy((fun x0 -> x0), (fun x f y -> f x y), (fun x -> x), p, op)

let chainl p op x = chainl1 p op <|>% x

let chainr1 p op =
    Inline.SepBy(elementParser = p, separatorParser = op,
                 stateFromFirstElement = (fun x0 -> [(Unchecked.defaultof<_>, x0)]),
                 foldState = (fun acc op x -> (op, x)::acc),
                 resultFromState = function // is called with (op, y) list in reverse order
                                   | ((op, y)::tl) ->
                                       let rec calc op y lst =
                                           match lst with
                                           | (op2, x)::tl -> calc op2 (op x y) tl
                                           | [] -> y // op is null
                                       calc op y tl
                                   | [] -> // shouldn't happen
                                           failwith "chainr1")


let chainr p op x = chainr1 p op <|>% x


// ------------------------------
// Computation expression syntax
// ------------------------------
[<Sealed>]
type ParserCombinator() =
    member t.Delay(f:(unit -> Parser<'a,'u>)) = fun stream -> (f()) stream
    member t.Return(x) = preturn x
    member t.Bind(p, f) = p >>= f
    member t.Zero() : Parser<'a,'u> = pzero
    member t.ReturnFrom(p: Parser<'a,'u>) = p
    // no Combine member by purpose
    member t.TryWith(p:Parser<'a,'u>, cf:(exn -> Parser<'a,'u>)) =
        fun stream ->
            (try p stream with e -> (cf e) stream)
    member t.TryFinally(p:Parser<'a,'u>, ff:(unit -> unit)) =
        fun stream ->
            try p stream finally ff ()

let parse = ParserCombinator()


// ----------------------
// Other helper functions
// ----------------------

let createParserForwardedToRef() =
    let dummyParser = fun stream -> failwith "a parser created with createParserForwardedToRef was not initialized"
    let r = ref dummyParser
    (fun stream -> !r stream), r : Parser<_,'u> * Parser<_,'u> ref
