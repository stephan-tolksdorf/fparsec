// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Primitives

open FParsec.Internals
open FParsec.Error

type ReplyStatus = Ok         =  1
                 | Error      =  0
                 | FatalError = -1

[<Literal>]
let Ok         = ReplyStatus.Ok
[<Literal>]
let Error      = ReplyStatus.Error
[<Literal>]
let FatalError = ReplyStatus.FatalError

[<System.Diagnostics.DebuggerDisplay("{GetDebuggerDisplay(),nq}")>]
[<CustomEquality; NoComparison>]
type Reply<'TResult,'TUserState> = struct
    new (result, state)        = {State = state; Error = NoErrorMessages; Result = result; Status = Ok}
    new (status, error, state) = {State = state; Error = error; Result = Unchecked.defaultof<_>; Status = status}
    new (status, result, error, state) = {State = state; Error = error; Result = result; Status = status}

    // The order of the following fields was chosen to optimize the object layout.
    // We don't use LayoutKind.Auto because it inhibits JIT optimizations:
    // http://blogs.msdn.com/clrcodegeneration/archive/2007/11/02/how-are-value-types-implemented-in-the-32-bit-clr-what-has-been-done-to-improve-their-performance.aspx

    val mutable State:  State<'TUserState>
    [<System.Diagnostics.DebuggerDisplay("{FParsec.Error.ErrorMessageList.GetDebuggerDisplay(Error),nq}")>]
    val mutable Error:  ErrorMessageList
    /// If Status <> Ok then the value of the Result field is undefined and may be equal to Unchecked.defaultof<'TResult>.
    val mutable Result: 'TResult
    val mutable Status: ReplyStatus

    override t.Equals(value: obj) =
        match value with
        | :? Reply<'TResult,'TUserState> as r ->
               t.Status = r.Status
            && (t.Status <> Ok || LanguagePrimitives.GenericEqualityERComparer.Equals(t.Result, r.Result))
            && t.State = r.State
            && t.Error = r.Error
        | _ -> false

    override t.GetHashCode() =
        // GetHashCode() is not required to return different hash codes for unequal instances
        int t.Status ^^^ t.State.GetHashCode()

    member private t.GetDebuggerDisplay() =
        let pos = if isNull t.State then "null" else t.State.Position.ToString()
        if t.Status = Ok && isNull t.Error then
            if typeof<'TResult> = typeof<unit> then "Reply((), " + pos + ")"
            else sprintf "Reply(%0.5A, %s)" t.Result pos
        else
            let e = FParsec.Error.ErrorMessageList.GetDebuggerDisplay(t.Error)
            if t.Status = Ok then
                if typeof<'TResult> = typeof<unit> then "Reply(Ok, (), " + e + ", " + pos + ")"
                else sprintf "Reply(Ok, %0.5A, %s, %s)" t.Result e pos
            else
               let status = match t.Status with
                            | Error -> "Error"
                            | FatalError -> "FatalError"
                            | _ -> "(ReplyStatus)" + (int t.Status).ToString()                           
               sprintf "Reply(%s, %s, %s)" status e pos

end

type Parser<'a, 'u> = State<'u> -> Reply<'a,'u>

// =================================
// Parser primitives and combinators
// =================================

// The `PrimitiveTests.Reference` module contains simple (but sometimes naive)
// reference implementations of most of the functions below.

let preturn x = fun state -> Reply(x, state)
let pzero : Parser<'a,'u> = fun state -> Reply(Error, NoErrorMessages, state)


// ---------------------------
// Chaining and piping parsers
// ---------------------------

let (>>=) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) =
    match box f with
    // optimization for uncurried functions
    | :? OptimizedClosures.FSharpFunc<'a, State<'u>, Reply<'b,'u>> as optF ->
        fun state ->
            let reply1 = p state
            if reply1.Status = Ok then
                let mutable reply2 = optF.Invoke(reply1.Result, reply1.State)
                if isNotNull reply1.Error && reply2.State == reply1.State then
                    reply2.Error <- concatErrorMessages reply1.Error reply2.Error
                reply2
            else
                Reply(reply1.Status, reply1.Error, reply1.State)
    | _ ->
        fun state ->
            let reply1 = p state
            if reply1.Status = Ok then
                let p2 = f reply1.Result
                let mutable reply2 = p2 reply1.State
                if isNotNull reply1.Error && reply2.State == reply1.State then
                    reply2.Error <- concatErrorMessages reply1.Error reply2.Error
                reply2
            else
                Reply(reply1.Status, reply1.Error, reply1.State)

let (>>%) (p: Parser<'a,'u>) x =
    fun state ->
        let reply = p state
        Reply(reply.Status, x, reply.Error, reply.State)

let (>>.) (p: Parser<'a,'u>) (q: Parser<'b,'u>) =
    fun state ->
        let reply1 = p state
        if reply1.Status = Ok then
            let mutable reply2 = q reply1.State
            if isNotNull reply1.Error && reply2.State == reply1.State then
                reply2.Error <- concatErrorMessages reply1.Error reply2.Error
            reply2
        else
            Reply(reply1.Status, reply1.Error, reply1.State)

let (.>>) (p: Parser<'a,'u>) (q: Parser<'b,'u>) =
    fun state ->
        let mutable reply1 = p state
        if reply1.Status = Ok then
            let reply2 = q reply1.State
            reply1.Error  <- mergeErrorsIfNeeded reply1.State reply1.Error reply2.State reply2.Error
            reply1.State  <- reply2.State
            reply1.Status <- reply2.Status
        reply1

let between (popen: Parser<_,'u>) (pclose: Parser<_,'u>) (p: Parser<_,'u>) =
    fun state ->
        let reply1 = popen state
        if reply1.Status = Ok then
            let mutable reply2 = p reply1.State
            let error = mergeErrorsIfNeeded reply1.State reply1.Error reply2.State reply2.Error
            if reply2.Status = Ok then
                let reply3 = pclose reply2.State
                reply2.Error  <- mergeErrorsIfNeeded reply2.State error reply3.State reply3.Error
                reply2.State  <- reply3.State
                reply2.Status <- reply3.Status
            else
                reply2.Error <- error
            reply2
        else
            Reply(reply1.Status, reply1.Error, reply1.State)

let (|>>) (p: Parser<'a,'u>) f =
    fun state ->
        let reply = p state
        Reply(reply.Status,
              (if reply.Status = Ok then f reply.Result else Unchecked.defaultof<_>),
              reply.Error,
              reply.State)

let pipe2 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
    fun state ->
        let reply1 = p1 state
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let reply2 = p2 reply1.State
            error <- mergeErrorsIfNeeded reply1.State error reply2.State reply2.Error
            if reply2.Status = Ok then
                 Reply(Ok, optF.Invoke(reply1.Result, reply2.Result), error, reply2.State)
            else Reply(reply2.Status, error, reply2.State)
        else Reply(reply1.Status, reply1.Error, reply1.State)

let pipe3 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) (p3: Parser<'c,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
    fun state ->
        let reply1 = p1 state
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let reply2 = p2 reply1.State
            error <- mergeErrorsIfNeeded reply1.State error reply2.State reply2.Error
            if reply2.Status = Ok then
                let reply3 = p3 reply2.State
                error <- mergeErrorsIfNeeded reply2.State error reply3.State reply3.Error
                if reply3.Status = Ok then
                     Reply(Ok, optF.Invoke(reply1.Result, reply2.Result, reply3.Result), error, reply3.State)
                else Reply(reply3.Status, error, reply3.State)
            else Reply(reply2.Status, error, reply2.State)
        else Reply(reply1.Status, error, reply1.State)

let pipe4 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) (p3: Parser<'c,'u>) (p4: Parser<'d,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_,_,_>.Adapt(f)
    fun state ->
        let reply1 = p1 state
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let reply2 = p2 reply1.State
            error <- mergeErrorsIfNeeded reply1.State error reply2.State reply2.Error
            if reply2.Status = Ok then
                let reply3 = p3 reply2.State
                error <- mergeErrorsIfNeeded reply2.State error reply3.State reply3.Error
                if reply3.Status = Ok then
                    let reply4 = p4 reply3.State
                    error <- mergeErrorsIfNeeded reply3.State error reply4.State reply4.Error
                    if reply4.Status = Ok then
                         Reply(Ok, optF.Invoke(reply1.Result, reply2.Result, reply3.Result, reply4.Result), error, reply4.State)
                    else Reply(reply4.Status, error, reply4.State)
                else Reply(reply3.Status, error, reply3.State)
            else Reply(reply2.Status, error, reply2.State)
        else Reply(reply1.Status, error, reply1.State)

let pipe5 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) (p3: Parser<'c,'u>) (p4: Parser<'d,'u>) (p5: Parser<'e,'u>) f =
    let optF = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt(f)
    fun state ->
        let reply1 = p1 state
        let mutable error = reply1.Error
        if reply1.Status = Ok then
            let reply2 = p2 reply1.State
            error <- mergeErrorsIfNeeded reply1.State error reply2.State reply2.Error
            if reply2.Status = Ok then
                let reply3 = p3 reply2.State
                error <- mergeErrorsIfNeeded reply2.State error reply3.State reply3.Error
                if reply3.Status = Ok then
                    let reply4 = p4 reply3.State
                    error <- mergeErrorsIfNeeded reply3.State error reply4.State reply4.Error
                    if reply4.Status = Ok then
                        let reply5 = p5 reply4.State
                        error <- mergeErrorsIfNeeded reply4.State error reply5.State reply5.Error
                        if reply5.Status = Ok then
                             Reply(Ok, optF.Invoke(reply1.Result, reply2.Result, reply3.Result, reply4.Result, reply5.Result), error, reply5.State)
                        else Reply(reply5.Status, error, reply5.State)
                    else Reply(reply4.Status, error, reply4.State)
                else Reply(reply3.Status, error, reply3.State)
            else Reply(reply2.Status, error, reply2.State)
        else Reply(reply1.Status, error, reply1.State)


// -----------------------------------------------
// Parsing alternatives and recovering from errors
// -----------------------------------------------

let (<|>) (p1: Parser<'a,'u>) (p2: Parser<'a,'u>) : Parser<'a,'u> =
    fun state ->
        let reply1 = p1 state
        if reply1.Status = Error && reply1.State == state then
            let mutable reply2 = p2 state
            if reply2.State == reply1.State then
                reply2.Error <- mergeErrors reply1.Error reply2.Error
            reply2
        else reply1

let choice (ps: seq<Parser<'a,'u>>)  =
    match ps with
    | :? (Parser<'a,'u>[]) as ps ->
        if ps.Length = 0 then pzero
        else
            fun state ->
                let mutable error = NoErrorMessages
                let mutable reply = ps.[0] state
                let mutable i = 1
                while reply.Status = Error && reply.State == state && i < ps.Length do
                    error <- mergeErrors error reply.Error
                    reply <- ps.[i] state
                    i <- i + 1
                if reply.State == state then
                    reply.Error <- mergeErrors error reply.Error
                reply
    | :? (Parser<'a,'u> list) as ps ->
        match ps with
        | [] -> pzero
        | hd::tl ->
            fun state ->
                let mutable error = NoErrorMessages
                let mutable hd, tl = hd, tl
                let mutable reply = hd state
                while reply.Status = Error && reply.State == state
                      && (match tl with
                          | h::t -> hd <- h; tl <- t; true
                          | _ -> false)
                   do
                    error <- mergeErrors error reply.Error
                    reply <- hd state
                if reply.State == state then
                    reply.Error <- mergeErrors error reply.Error
                reply
    | _ -> fun state ->
               use iter = ps.GetEnumerator()
               if iter.MoveNext() then
                   let mutable error = NoErrorMessages
                   let mutable reply = iter.Current state
                   while reply.Status = Error && reply.State == state && iter.MoveNext() do
                       error <- mergeErrors error reply.Error
                       reply <- iter.Current state
                   if reply.State == state then
                       reply.Error <- mergeErrors error reply.Error
                   reply
               else
                   Reply(Error, NoErrorMessages, state)


let choiceL (ps: seq<Parser<'a,'u>>) label =
    let error = expectedError label
    match ps with
    | :? (Parser<'a,'u>[]) as ps ->
        if ps.Length = 0 then pzero
        else
            fun state ->
                let mutable reply = ps.[0] state
                let mutable i = 1
                while reply.Status = Error && reply.State == state && i < ps.Length do
                    reply <- ps.[i] state
                    i <- i + 1
                if reply.State == state then
                    reply.Error <- error
                reply
    | :? (Parser<'a,'u> list) as ps ->
        match ps with
        | [] -> pzero
        | hd::tl ->
            fun state ->
                let mutable hd, tl = hd, tl
                let mutable reply = hd state
                while reply.Status = Error && reply.State == state
                      && (match tl with
                          | h::t -> hd <- h; tl <- t; true
                          | _ -> false)
                   do
                    reply <- hd state
                if reply.State == state then
                    reply.Error <- error
                reply
    | _ -> fun state ->
               use iter = ps.GetEnumerator()
               if iter.MoveNext() then
                   let mutable reply = iter.Current state
                   while reply.Status = Error && reply.State == state && iter.MoveNext() do
                       reply <- iter.Current state
                   if reply.State == state then
                       reply.Error <- error
                   reply
               else
                   Reply(Error, NoErrorMessages, state)

let (<|>%) (p: Parser<'a,'u>) x =
    fun state ->
        let mutable reply = p state
        if reply.Status = Error && reply.State == state then
            reply.Result <- x
            reply.Status <- Ok
        reply

let opt (p: Parser<'a,'u>) : Parser<'a option,'u> =
    fun state ->
        let reply = p state
        if reply.Status = Ok then
            Reply(Ok, Some reply.Result, reply.Error, reply.State)
        else
            // None is represented as null
            let status = if reply.Status = Error && reply.State == state then Ok else reply.Status
            Reply(status, reply.Error, reply.State)

let optional (p: Parser<'a,'u>) : Parser<unit,'u> =
    fun state ->
        let reply = p state
        let status = if reply.Status = Error && reply.State == state then Ok else reply.Status
        // () is represented as null
        Reply<unit,_>(status, reply.Error, reply.State)

let attempt (p: Parser<'a,'u>) =
    fun state ->
        let mutable reply = p state
        if reply.Status <> Ok then
            if reply.State != state then
                reply.Error  <- backtrackError reply.State reply.Error
                reply.State  <- state
                reply.Status <- Error // turns FatalErrors into Errors
            elif reply.Status = FatalError then
                reply.Status <- Error
        reply

let (>>=?) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) =
    let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
    fun state ->
        let reply1 = p state
        if reply1.Status = Ok then
            let mutable reply2 = optF.Invoke(reply1.Result, reply1.State)
            if reply2.State == reply1.State then
                let error = mergeErrors reply1.Error reply2.Error
                if reply2.Status <> Error || reply1.State == state then
                    reply2.Error <- error
                else
                    reply2.Error <- backtrackError reply2.State error
                    reply2.State <- state
            reply2
        else
            Reply(reply1.Status, reply1.Error, reply1.State)

let (>>?) (p: Parser<'a,'u>) (q: Parser<'b,'u>) =
    fun state ->
        let reply1 = p state
        if reply1.Status = Ok then
            let mutable reply2 = q reply1.State
            if reply2.State == reply1.State then
                let error = mergeErrors reply1.Error reply2.Error
                if reply2.Status <> Error || reply1.State == state then
                    reply2.Error <- error
                else
                    reply2.Error <- backtrackError reply2.State error
                    reply2.State <- state
            reply2
        else
            Reply(reply1.Status, reply1.Error, reply1.State)

let (.>>?) (p: Parser<'a,'u>) (q: Parser<'b,'u>) =
    fun state ->
        let mutable reply1 = p state
        if reply1.Status = Ok then
            let reply2 = q reply1.State
            if reply2.State != reply1.State then
                reply1.State  <- reply2.State
                reply1.Error  <- reply2.Error
                reply1.Status <- reply2.Status
            else
                let error = mergeErrors reply1.Error reply2.Error
                if reply2.Status <> Error || reply1.State == state then
                    reply1.Error  <- error
                    reply1.Status <- reply2.Status
                else
                    reply1.Error  <- backtrackError reply1.State error
                    reply1.State  <- state
                    reply1.Status <- Error
        reply1


// -------------------------------------
// Conditional parsing and looking ahead
// -------------------------------------

/// REVIEW: should `followedBy` use the error messages generated by `p`?

let internal followedByE (p: Parser<'a,'u>) error =
    fun state ->
        let reply = p state
        if reply.Status = Ok then Reply((), state)
        else Reply(Error, error, state)

let followedBy  p       = followedByE p NoErrorMessages
let followedByL p label = followedByE p (expectedError label)

let internal notFollowedByE (p: Parser<'a,'u>) error =
    fun state ->
        let reply = p state
        if reply.Status <> Ok then Reply((), state)
        else Reply(Error, error, state)

let notFollowedBy  p       = notFollowedByE p NoErrorMessages
let notFollowedByL p label = notFollowedByE p (unexpectedError label)

let lookAhead (p: Parser<'a,'u>) =
    fun state ->
        let mutable reply = p state
        if reply.Status = Ok then
            reply.State <- state
            reply.Error <- NoErrorMessages
        else
            if reply.State != state then
                reply.Error  <- backtrackError reply.State reply.Error
                reply.State  <- state
            reply.Status <- Error // turn FatalErrors into normal Errors
        reply


// --------------------------
// Customizing error messages
// --------------------------

let (<?>) (p: Parser<'a,'u>) label  =
    let error = expectedError label
    fun state ->
        let mutable reply = p state
        if reply.State == state then
            reply.Error <- error
        reply

let (<??>) (p: Parser<'a,'u>) label =
    let expErr = expectedError label
    fun state ->
        let mutable reply = p state
        if reply.Status = Ok then
            if reply.State == state then
                reply.Error <- expErr
        else
            if reply.State == state then
                reply.Error <- match reply.Error with
                               | AddErrorMessage(BacktrackPoint(pos, msgs), NoErrorMessages)
                                   -> AddErrorMessage(CompoundError(label, pos, msgs), NoErrorMessages)
                               | _ -> expErr
            else
                reply.Error  <- compoundError label reply.State reply.Error
                reply.State  <- state      // we backtrack ...
                reply.Status <- FatalError // ... so we need to make sure normal parsing doesn't continue
        reply

let fail msg : Parser<'a,'u> =
    let error = messageError msg
    fun state ->
        Reply(Error, error, state)

let failFatally msg : Parser<'a,'u> =
    let error = messageError msg
    fun state -> Reply(FatalError, error, state)

// -----------------
// Parsing sequences
// -----------------

let tuple2 p1 p2          = pipe2 p1 p2          (fun a b       -> (a, b))
let tuple3 p1 p2 p3       = pipe3 p1 p2 p3       (fun a b c     -> (a, b, c))
let tuple4 p1 p2 p3 p4    = pipe4 p1 p2 p3 p4    (fun a b c d   -> (a, b, c, d))
let tuple5 p1 p2 p3 p4 p5 = pipe5 p1 p2 p3 p4 p5 (fun a b c d e -> (a, b, c, d, e))

let parray n (p: Parser<'a,'u>) =
    if n = 0 then preturn [||]
    else
        fun state ->
            let mutable reply = p state
            let mutable error = reply.Error
            let mutable newReply = Unchecked.defaultof<Reply<_,_>>
            if reply.Status = Ok then
                let mutable xs = Array.zeroCreate n
                xs.[0] <- reply.Result
                let mutable i = 1
                while i < n do
                    let prevState = reply.State
                    reply <- p prevState
                    error <- mergeErrorsIfNeeded prevState error reply.State reply.Error
                    if reply.Status = Ok then
                        xs.[i] <- reply.Result
                        i <- i + 1
                    else
                        i <- n // break
                newReply.Result <- xs // we set the result even if there was an error
            newReply.State  <- reply.State
            newReply.Error  <- error
            newReply.Status <- reply.Status
            newReply

let skipArray n (p: Parser<'a,'u>) =
    if n = 0 then preturn ()
    else
        fun state ->
            let mutable reply = p state
            let mutable error = reply.Error
            let mutable newReply = Unchecked.defaultof<Reply<_,_>>
            if reply.Status = Ok then
                 let mutable i = 1
                 while i < n do
                     let prevState = reply.State
                     reply <- p prevState
                     error <- mergeErrorsIfNeeded prevState error reply.State reply.Error
                     if reply.Status = Ok then
                         i <- i + 1
                     else
                         i <- n // break
                // () is represented as null
            newReply.State  <- reply.State
            newReply.Error  <- error
            newReply.Status <- reply.Status
            newReply
let
#if NOINLINE
#else
    inline
#endif
           private manyFoldApplyImpl require1 fold1 fold applyF getEmpty (p: Parser<'a,'u>) =
    fun state ->
        let mutable reply = p state
        if reply.Status = Ok then
            let mutable xs    = fold1 reply.Result
            let mutable error = reply.Error
            let mutable state = reply.State
            reply <- p state
            while reply.Status = Ok do
                if referenceEquals reply.State state then
                    _raiseInfiniteLoopException "many" state
                xs    <- fold xs reply.Result
                error <- reply.Error
                state <- reply.State
                reply <- p state
            if reply.Status = Error && reply.State == state then
                Reply(Ok, applyF xs, mergeErrors error reply.Error, state)
            else
                let error = mergeErrorsIfNeeded state error reply.State reply.Error
                Reply(reply.Status, error, reply.State)
        elif not require1 && reply.Status = Error && reply.State == state then
            Reply(Ok, getEmpty(), reply.Error, state)
        else
            Reply(reply.Status, reply.Error, reply.State)

let
#if NOINLINE
#else
    inline
#endif
           private manyFoldApply2Impl require1 fold1 fold applyF getEmpty (p1: Parser<'a,'u>) (p: Parser<'b,'u>) =
    fun state ->
        let reply1 = p1 state
        if reply1.Status = Ok then
            let mutable xs    = fold1 reply1.Result
            let mutable error = reply1.Error
            let mutable state = reply1.State
            let mutable reply = p state
            while reply.Status = Ok do
                if referenceEquals reply.State state then
                    _raiseInfiniteLoopException "many" state
                xs    <- fold xs reply.Result
                error <- reply.Error
                state <- reply.State
                reply <- p state
            if reply.Status = Error && reply.State == state then
                Reply(Ok, applyF xs, mergeErrors error reply.Error, state)
            else
                let error = mergeErrorsIfNeeded state error reply.State reply.Error
                Reply(reply.Status, error, reply.State)
        elif not require1 && reply1.Status = Error && reply1.State == state then
            Reply(Ok, getEmpty(), reply1.Error, state)
        else
            Reply(reply1.Status, reply1.Error, reply1.State)

let
#if NOINLINE
#else
    inline
#endif
           manyFoldApply fold1 fold applyF getEmpty p =
    manyFoldApplyImpl false fold1 fold applyF getEmpty p

let
#if NOINLINE
#else
    inline
#endif
           many1FoldApply fold1 fold applyF p =
    manyFoldApplyImpl true fold1 fold applyF (fun () -> Unchecked.defaultof<_>) p

let
#if NOINLINE
#else
    inline
#endif
           manyFoldApply2 fold1 fold applyF getEmpty p1 p =
    manyFoldApply2Impl false fold1 fold applyF getEmpty p1 p

let
#if NOINLINE
#else
    inline
#endif
           many1FoldApply2 fold1 fold applyF p1 p =
    manyFoldApply2Impl true fold1 fold applyF (fun () -> Unchecked.defaultof<_>) p1 p


// it's the sepMayEnd case that's difficult to implement (efficiently) without a specialized parser
let
#if NOINLINE
#else
    inline
#endif
           sepEndByFoldApplyImpl require1 sepMayEnd fold1 fold applyF getEmpty (p: Parser<'a,'u>) (sep: Parser<'b,'u>) =
    fun state ->
        let mutable reply1 = p state
        if reply1.Status = Ok then
            let mutable xs     = fold1 reply1.Result
            let mutable error  = reply1.Error
            let mutable state  = reply1.State
            let mutable reply2 = sep state
            while reply2.Status = Ok && (reply1 <- p reply2.State; reply1.Status = Ok) do
                xs <- fold xs reply1.Result
                if not (referenceEquals reply1.State reply2.State) then
                    error <- reply1.Error
                elif not (referenceEquals reply1.State state) then
                    error <- mergeErrors reply2.Error reply1.Error
                else
                    _raiseInfiniteLoopException "sep(EndBy)" state
                state  <- reply1.State
                reply2 <- sep state
            if  reply2.Status = Error && reply2.State == state then
                Reply(Ok, applyF xs, mergeErrors error reply2.Error, state)
            elif sepMayEnd && reply1.Status = Error && reply1.State == reply2.State then
                let error = mergeErrors (mergeErrorsIfNeeded state error reply2.State reply2.Error) reply1.Error
                Reply(Ok, applyF xs, error, reply1.State)
            elif reply1.Status <> Ok then
                let error = mergeErrorsIfNeeded3 state error reply2.State reply2.Error reply1.State reply1.Error
                Reply(reply1.Status, error, reply1.State)
            else
                let error = mergeErrorsIfNeeded state error reply2.State reply2.Error
                Reply(reply2.Status, error, reply2.State)
        elif not require1 && reply1.Status = Error && reply1.State == state then
            Reply(Ok, getEmpty(), reply1.Error, state)
        else
            Reply(reply1.Status, reply1.Error, reply1.State)

let
#if NOINLINE
#else
    inline
#endif
           sepByFoldApply fold1 fold applyF getEmpty p sep =
    sepEndByFoldApplyImpl false false fold1 fold applyF getEmpty p sep


let
#if NOINLINE
#else
    inline
#endif
           sepBy1FoldApply fold1 fold applyF p sep =
    sepEndByFoldApplyImpl true false fold1 fold applyF (fun () -> Unchecked.defaultof<_>) p sep

let
#if NOINLINE
#else
    inline
#endif
           sepEndByFoldApply fold1 fold applyF getEmpty p sep =
    sepEndByFoldApplyImpl false true fold1 fold applyF getEmpty p sep

let
#if NOINLINE
#else
    inline
#endif
           sepEndBy1FoldApply fold1 fold applyF p sep =
    sepEndByFoldApplyImpl true true fold1 fold applyF (fun () -> Unchecked.defaultof<_>) p sep

let
#if NOINLINE
#else
    inline
#endif
           manyTillFoldApply fold1 fold applyF getEmpty (p: Parser<'a,'u>) (endp: Parser<'c,'u>) =
    fun state ->
        let mutable reply2 = endp state
        if reply2.Status <> Ok then
            let mutable reply1 = p state
            if reply1.Status = Ok then
                let mutable xs     = fold1 reply1.Result
                let mutable error  = reply1.Error
                let mutable state  = reply1.State
                reply2 <- endp state
                while reply2.Status <> Ok && (reply1 <- p state; reply1.Status = Ok) do
                    if referenceEquals reply1.State state then
                        _raiseInfiniteLoopException "manyTill" state
                    xs     <- fold xs reply1.Result
                    error  <- reply1.Error
                    state  <- reply1.State
                    reply2 <- endp state
                if reply2.Status = Ok then
                    let error = mergeErrorsIfNeeded state error reply2.State reply2.Error
                    Reply(Ok, applyF xs reply2.Result, error, reply2.State)
                elif reply1.Status = Error && reply1.State == state then
                    let error = if reply2.State != state then reply2.Error
                                else mergeErrors (mergeErrors error reply1.Error) reply2.Error
                    Reply(reply2.Status, error, reply2.State)
                else
                    let error = mergeErrorsIfNeeded state error reply1.State reply1.Error
                    Reply(reply1.Status, error, reply1.State)
            elif reply1.Status = Error && reply1.State == state then
                let error = if reply2.State != state then reply2.Error
                            else mergeErrors reply1.Error reply2.Error
                Reply(reply2.Status, error, reply2.State)
            else
                Reply(reply1.Status, reply1.Error, reply1.State)
        else
            Reply(Ok, getEmpty reply2.Result, reply2.Error, reply2.State)



let many               p = manyFoldApply (fun x -> [x]) (fun xs x -> x::xs) List.rev       (fun () -> []) p
let manyRev            p = manyFoldApply (fun x -> [x]) (fun xs x -> x::xs) (fun xs -> xs) (fun () -> []) p
let skipMany           p = manyFoldApply (fun _ -> ())  (fun _ _ -> ())     (fun xs -> xs) (fun () -> ()) p
let manyFold    acc0 f p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           manyFoldApply (fun x -> optF.Invoke(acc0, x)) (fun acc x -> optF.Invoke(acc, x)) (fun acc -> acc) (fun () -> acc0) p
let manyReduce  f altX p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           manyFoldApply (fun x0 -> x0) (fun x0 x -> optF.Invoke(x0, x)) (fun x0 -> x0) (fun () -> altX) p

let many1              p = many1FoldApply (fun x -> [x]) (fun xs x -> x::xs) List.rev       p
let many1Rev           p = many1FoldApply (fun x -> [x]) (fun xs x -> x::xs) (fun xs -> xs) p
let skipMany1          p = many1FoldApply (fun _ -> ())  (fun _ _ -> ())     (fun xs -> xs) p
let many1Fold   acc0 f p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           many1FoldApply (fun x -> optF.Invoke(acc0, x)) (fun acc x -> optF.Invoke(acc, x)) (fun x -> x) p
let many1Reduce f      p = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                           many1FoldApply (fun x0 -> x0) (fun x0 x -> optF.Invoke(x0, x)) (fun x0 -> x0) p


let sepBy              p sep = sepByFoldApply (fun x -> [x]) (fun xs x -> x::xs) List.rev       (fun () -> [])  p sep
let sepByRev           p sep = sepByFoldApply (fun x -> [x]) (fun xs x -> x::xs) (fun xs -> xs) (fun () -> [])  p sep
let skipSepBy          p sep = sepByFoldApply (fun _ -> ())  (fun _ _ -> ())     (fun xs -> xs) (fun _ -> ())   p sep
let sepByFold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                               sepByFoldApply (fun x -> optF.Invoke(acc0, x)) (fun acc x -> optF.Invoke(acc, x)) (fun acc -> acc) (fun () -> acc0) p sep
let sepByReduce f altX p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                               sepByFoldApply (fun x0 -> x0) (fun x0 x -> optF.Invoke(x0, x)) (fun x0 -> x0) (fun () -> altX) p sep

let sepBy1              p sep = sepBy1FoldApply (fun x -> [x]) (fun xs x -> x::xs) List.rev       p sep
let sepBy1Rev           p sep = sepBy1FoldApply (fun x -> [x]) (fun xs x -> x::xs) (fun xs -> xs) p sep
let skipSepBy1          p sep = sepBy1FoldApply (fun _ -> ())  (fun _ _ -> ())     (fun xs -> xs) p sep
let sepBy1Fold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                sepBy1FoldApply (fun x -> optF.Invoke(acc0, x)) (fun acc x -> optF.Invoke(acc, x)) (fun acc -> acc) p sep
let sepBy1Reduce f      p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                sepBy1FoldApply (fun x0 -> x0) (fun x0 x -> optF.Invoke(x0, x)) (fun x0 -> x0) p sep


let sepEndBy              p sep = sepEndByFoldApply(fun x -> [x]) (fun xs x -> x::xs) List.rev       (fun () -> [])  p sep
let sepEndByRev           p sep = sepEndByFoldApply(fun x -> [x]) (fun xs x -> x::xs) (fun xs -> xs) (fun () -> [])  p sep
let skipSepEndBy          p sep = sepEndByFoldApply(fun _ -> ())  (fun _ _ -> ())     (fun xs -> xs) (fun _ -> ())   p sep
let sepEndByFold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                  sepEndByFoldApply (fun x -> optF.Invoke(acc0, x)) (fun acc x -> optF.Invoke(acc, x)) (fun acc -> acc) (fun () -> acc0) p sep
let sepEndByReduce f altX p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                  sepEndByFoldApply (fun x0 -> x0) (fun x0 x -> optF.Invoke(x0, x)) (fun x0 -> x0) (fun () -> altX) p sep

let sepEndBy1              p sep = sepEndBy1FoldApply (fun x -> [x]) (fun xs x -> x::xs) List.rev       p sep
let sepEndBy1Rev           p sep = sepEndBy1FoldApply (fun x -> [x]) (fun xs x -> x::xs) (fun xs -> xs) p sep
let skipSepEndBy1          p sep = sepEndBy1FoldApply (fun _ -> ())  (fun _ _ -> ())     (fun xs -> xs) p sep
let sepEndBy1Fold   acc0 f p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   sepEndBy1FoldApply (fun x -> optF.Invoke(acc0, x)) (fun acc x -> optF.Invoke(acc, x)) (fun acc -> acc) p sep
let sepEndBy1Reduce f      p sep = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   sepEndBy1FoldApply (fun x0 -> x0) (fun x0 x -> optF.Invoke(x0, x)) (fun x0 -> x0) p sep

let manyTill              p endp = manyTillFoldApply (fun x -> [x]) (fun xs x -> x::xs) (fun xs _ -> List.rev xs) (fun _ -> [])       p endp
let manyTillRev           p endp = manyTillFoldApply (fun x -> [x]) (fun xs x -> x::xs) (fun xs _ -> xs) (fun _ -> []) p endp
let skipManyTill          p endp = manyTillFoldApply (fun _ -> ())  (fun _ _ -> ())     (fun _ _ -> ()) (fun _ -> ())  p endp
let manyTillFold   acc0 f p endp = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   manyTillFoldApply (fun x -> optF.Invoke(acc0, x)) (fun acc x -> optF.Invoke(acc, x)) (fun acc _ -> acc) (fun _ -> acc0) p endp
let manyTillReduce f altX p endp = let optF = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                                   manyTillFoldApply (fun x0 -> x0) (fun x0 x -> optF.Invoke(x0, x)) (fun x0 _ -> x0) (fun _ -> altX) p endp


let chainl1 p op =
    many1FoldApply2 (fun x0 -> x0) (fun x (f, y) -> f x y) (fun x -> x) p (tuple2 op p)

let chainl p op altX =
    manyFoldApply2 (fun x0 -> x0) (fun x (f, y) -> f x y) (fun x -> x) (fun () -> altX) p (tuple2 op p)

let chainr1 p op =
    pipe2 p (manyRev (tuple2 op p)) (fun x0 opYs -> match opYs with
                                                    | []          -> x0
                                                    | (op, y)::tl ->
                                                        let rec calc op1 y lst =
                                                            match lst with
                                                            | (op2, x)::tl -> calc op2 (op1 x y) tl
                                                            | [] -> op1 x0 y
                                                        calc op y tl)
let chainr p op x = chainr1 p op <|>% x


// ------------------------------
// Computation expression syntax
// ------------------------------
[<Sealed>]
type ParserCombinator() =
    member t.Delay(f:(unit -> Parser<'a,'u>)) = fun state -> (f ()) state
    member t.Return(x) = preturn x
    member t.Bind(p, f) = p >>= f
    member t.Zero() : Parser<'a,'u> = pzero
    // no Combine member by purpose
    member t.TryWith(p:Parser<'a,'u>, cf:(exn -> Parser<'a,'u>)) =
        fun state ->
            (try p state with e -> (cf e) state)
    member t.TryFinally(p:Parser<'a,'u>, ff:(unit -> unit)) =
        fun state ->
            try p state finally ff ()

let parse = ParserCombinator()


// ----------------------
// Other helper functions
// ----------------------

let createParserForwardedToRef() =
    let dummyParser = fun state -> failwith "a parser was not initialized"
    let r = ref dummyParser
    (fun state -> !r state), r : Parser<_,'u> * Parser<_,'u> ref
