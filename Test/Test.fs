// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.Test

open FParsec.Error
open FParsec.Primitives

exception TestFailed of string

let Fail() = raise (TestFailed("Test failed."))

let True cond =
    if not cond then Fail ()

let False cond =
    if cond then Fail ()

let Equal a b =
    if not (a = b) then Fail ()

let NotEqual a b =
    if a = b then Fail ()

let ReferenceEqual (a: 't) (b: 't) =
    if not (System.Object.ReferenceEquals(a, b)) then Fail ()


let private ROkE_ withNewline (content: string) nSkippedChars result error (parser: Parser<_,_>) =
    use stream = new FParsec.CharStream(content, 0, content.Length)
    let state = new FParsec.State<_>(stream, ())
    let reply = parser state
    reply.Status |> Equal Ok
    reply.Result |> Equal result
    reply.Error  |> Equal error
    int32 reply.State.Index |> Equal (int32 state.Index + nSkippedChars)
    (reply.State.LineBegin = state.LineBegin) |> Equal (not withNewline)

let ROk   content nSkippedChars result parser = ROkE_ false content nSkippedChars result NoErrorMessages parser
let ROkE  content nSkippedChars result error parser = ROkE_ false content nSkippedChars result error parser
let ROkNL content nSkippedChars result parser = ROkE_ true  content nSkippedChars result NoErrorMessages parser

let private RError_ status (content: string) nSkippedChars error (parser: Parser<_,_>) =
    use stream = new FParsec.CharStream(content, 0, content.Length)
    let state = new FParsec.State<_>(stream, ())
    let reply = parser state
    reply.Status |> Equal status
    reply.Error  |> Equal error
    int32 reply.State.Index |> Equal (int32 state.Index + nSkippedChars)

let RError content nSkippedChars error parser = RError_ Error content nSkippedChars  error parser
let RFatalError content nSkippedChars error parser = RError_ FatalError content nSkippedChars error parser




let EqualParser (parser1: Parser<'a,'u>) state (parser2: Parser<'a,'u>) =
    let reply1 = parser1 state
    let reply2 = parser2 state
    Equal reply1 reply2


// we use the following flag to allow test parsers with mutable state
// to repeat the last action in order to ease debugging
let mutable checkParserRepeat = false

let checkParser (parser1: Parser<_,_>) (parser2: Parser<_,_>) state =
    let reply1_ = parser1 state
    let reply2_ = parser2 state
    if reply1_ <> reply2_ then
        if System.Diagnostics.Debugger.IsAttached then
            System.Diagnostics.Debugger.Break()
        checkParserRepeat <- true
        // step into the following parser calls to see what goes wrong
        let reply1 = parser1 state
        let reply2 = parser2 state
        Equal reply1.Status reply2.Status
        Equal reply1.State  reply2.State
        Equal reply1.Error  reply2.Error
        if reply1.Status = Ok then
            Equal reply1.Result reply2.Result

let checkParserStr parser1 parser2 (str: string) =
    use cs = new FParsec.CharStream(str, 0, str.Length)
    let s0 = new FParsec.State<unit>(cs, ())
    checkParser parser1 parser2 s0

let constantTestParsers r e : Parser<'a, int>[] = [| // we rely on the order of these parsers
    fun s -> Reply(Ok, r, e, s);
    fun s -> Reply(Ok, r, e, s.WithUserState(s.UserState + 1))
    fun s -> Reply(Error, e, s);
    fun s -> Reply(Error, e, s.WithUserState(s.UserState + 1));
    fun s -> Reply(FatalError, e, s);
    fun s -> Reply(FatalError, e, s.WithUserState(s.UserState + 1));
|]


/// Builds a parser from a list of constant test parsers. The first parser
/// will be used for the first invocation, the second for the next
/// invocation, and so on. The reset function can be used to reset the aggregate parser.
let seqParserAndReset ps =
    let psr        = ref ps
    let inRepeat   = ref false
    (fun state ->
         if checkParserRepeat && not !inRepeat then
            inRepeat:= true
            psr:= ps
         match !psr with
         | hd::tl -> psr:= tl; hd state
         | [] -> Reply(Error, NoErrorMessages, state)),
    (fun () -> psr:= ps)

let seqParserAndReset2 ps =
    let p1, p1r = seqParserAndReset ps
    let p2, p2r = seqParserAndReset ps
    p1, p2, (fun () -> p1r(); p2r())

let private _shuffleArrayRandom = new System.Random(123)
let shuffleArray (xs: 'a[]) =
    let n = xs.Length
    for i = 0 to n - 2 do
       let r = _shuffleArrayRandom.Next(n - i - 1);
       let t = xs.[i]
       xs.[i]     <- xs.[i + r]
       xs.[i + r] <- t

let setStaticField (t: System.Type) name v =
    t.GetField(name, System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Static).SetValue(null, v)

let getStaticField (t: System.Type) name =
    unbox (t.GetField(name, System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Static).GetValue())
