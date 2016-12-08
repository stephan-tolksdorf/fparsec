// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.Test

open System.Runtime.CompilerServices

open FParsec
open FParsec.Error
open FParsec.Primitives

exception TestFailed of string

let Fail() = raise (TestFailed("Test failed."))

let True cond =
    if not cond then Fail ()

let False cond =
    if cond then Fail ()

let IsNull x =
    match box x with
    | null -> ()
    | _    -> Fail()

[<MethodImplAttribute(MethodImplOptions.NoInlining)>]
let EqualFail_ a b =
    Fail()

// inline to allow the F# compiler to optimize the equality comparison
let inline Equal a b =
    if not (a = b) then EqualFail_ a b

let NotEqual a b =
    if a = b then Fail ()

let ReferenceEqual (a: 't) (b: 't) =
    if not (System.Object.ReferenceEquals(a, b)) then Fail ()



let private ROkE_ withNewline (content: string) nSkippedChars result error (parser: Parser<_,_>) =
    use stream = new CharStream<unit>(content, 0, content.Length)
    let mutable reply = parser stream
    if     reply.Status <> Ok
        || reply.Result <> result
        || reply.Error <> error
        || stream.Index <> (int64 nSkippedChars)
        || stream.LineBegin = 0L <> (not withNewline)
    then
        System.Diagnostics.Debugger.Break()
        stream.Seek(0L)
        stream.SetLine_WithoutCheckAndWithoutIncrementingTheStateTag(1L)
        stream.SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag(0L)
        reply <- parser stream
        reply.Status |> Equal Ok
        reply.Result |> Equal result
        reply.Error  |> Equal error
        stream.Index |> Equal (int64 nSkippedChars)
        stream.LineBegin = 0L |> Equal (not withNewline)

let ROk   content nSkippedChars result parser = ROkE_ false content nSkippedChars result NoErrorMessages parser
let ROkE  content nSkippedChars result error parser = ROkE_ false content nSkippedChars result error parser
let ROkNL content nSkippedChars result parser = ROkE_ true  content nSkippedChars result NoErrorMessages parser

let private RError_ status (content: string) nSkippedChars error (parser: Parser<_,_>) =
    use stream = new CharStream<unit>(content, 0, content.Length)
    let mutable reply = parser stream
    if    reply.Status <> status
        || reply.Error <> error
        || stream.Index <> (int64 nSkippedChars)
    then
        System.Diagnostics.Debugger.Break()
        stream.Seek(0L)
        stream.SetLine_WithoutCheckAndWithoutIncrementingTheStateTag(1L)
        stream.SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag(0L)
        reply <- parser stream
        reply.Status |> Equal Error
        reply.Error  |> Equal error
        stream.Index |> Equal (int64 nSkippedChars)


let RError content nSkippedChars error parser = RError_ Error content nSkippedChars  error parser
let RFatalError content nSkippedChars error parser = RError_ FatalError content nSkippedChars error parser



//let EqualParser (parser1: Parser<'a,'u>) state (parser2: Parser<'a,'u>) =
//    let reply1 = parser1 state
//    let reply2 = parser2 state
//    Equal reply1 reply2


// we use the following flag to allow test parsers with mutable state
// to repeat the last action in order to simplify debugging
let mutable checkParserRepeat = false

let checkParser (parser1: Parser<'a,'u>) (parser2: Parser<'a,'u>) (stream: CharStream<'u>) =
    let state0 = stream.State

    let mutable reply1 = parser1 stream
    let mutable state1 = stream.State
    let mutable index1 = stream.Index

    stream.BacktrackTo(state0)

    let mutable reply2 = parser2 stream
    let mutable state2 = stream.State
    let mutable index2 = stream.Index

    if   reply1 <> reply2
      || index1 <> index2
      || state1.Line <> state2.Line
      || state1.LineBegin <> state2.LineBegin
      || state1.Name <> state2.Name
      || state1.UserState <> state2.UserState
      || (state1.Tag <> state0.Tag) <> (state2.Tag <> state0.Tag)
    then
        if System.Diagnostics.Debugger.IsAttached then
            System.Diagnostics.Debugger.Break()
            checkParserRepeat <- true
            // step into the following parser calls to see what goes wrong

        stream.BacktrackTo(state0)

        reply1 <- parser1 stream
        state1 <- stream.State
        index1 <- stream.Index

        stream.BacktrackTo(state0)

        reply2 <- parser2 stream
        state2 <- stream.State
        index2 <- stream.Index

        stream.BacktrackTo(state0)

        Equal reply1.Status reply2.Status
        Equal reply1.Error  reply2.Error
        if reply1.Status = Ok then
            Equal reply1.Result reply2.Result
        Equal index1 stream.Index
        Equal state1.Line stream.Line
        Equal state1.LineBegin stream.LineBegin
        Equal state1.Name stream.Name
        Equal state1.UserState stream.UserState
        Equal (state1.Tag <> state0.Tag) (stream.StateTag <> state0.Tag)

let checkParserStr parser1 parser2 (str: string) =
    use stream = new CharStream<unit>(str, 0, str.Length)
    checkParser parser1 parser2 stream

let constantTestParsers r e : Parser<'a, int>[] = [| // we rely on the order of these parsers
    fun s -> Reply(Ok, r, e);
    fun s -> s.UserState <- s.UserState + 1; Reply(Ok, r, e)
    fun s -> Reply(Error, e);
    fun s -> s.UserState <- s.UserState + 1; Reply(Error, e);
    fun s -> Reply(FatalError, e);
    fun s -> s.UserState <- s.UserState + 1; Reply(FatalError, e);
|]


/// Builds a parser from a list of constant test parsers. The first parser
/// will be used for the first invocation, the second for the next
/// invocation, and so on. The reset function can be used to reset the aggregate parser.
let seqParserAndReset ps =
    let psr        = ref ps
    let inRepeat   = ref false
    (fun stream ->
         if checkParserRepeat && not !inRepeat then
            inRepeat:= true
            psr:= ps
         match !psr with
         | hd::tl -> psr:= tl; hd stream
         | [] -> Reply(Error, NoErrorMessages)),
    (fun () -> psr:= ps)

let seqParserAndReset2 ps =
    let p1, p1r = seqParserAndReset ps
    let p2, p2r = seqParserAndReset ps
    p1, p2, (fun () -> p1r(); p2r())

let setStaticField (t: System.Type) name v =
    t.GetField(name, System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Static).SetValue(null, v)

let getStaticField (t: System.Type) name =
    unbox (t.GetField(name, System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Static).GetValue())


let shuffleArray (rand: System.Random) (xs: 'a[]) =
    let n = xs.Length
    for i = 0 to n - 2 do
       let r = rand.Next(n - i - 1);
       let t = xs.[i]
       xs.[i]     <- xs.[i + r]
       xs.[i + r] <- t

let inline _1< ^t when ^t : (static member One : ^t) > = LanguagePrimitives.GenericOne< ^t >
