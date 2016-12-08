// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.PrimitivesTests

open FParsec
open FParsec.Error

module Reference =

    [<Literal>]
    let Ok         = FParsec.Primitives.Ok
    [<Literal>]
    let Error      = FParsec.Primitives.Error
    [<Literal>]
    let FatalError = FParsec.Primitives.FatalError

    type Parser<'a,'u> = FParsec.Primitives.Parser<'a,'u>

    type FParsec.Reply<'a>
      with
        member t.WithError(error: ErrorMessageList) =
            Reply(t.Status, t.Result, error)

    let reconstructErrorReply (reply: Reply<_>) =
        Reply(reply.Status, reply.Error)

    let preturn x =
        fun stream -> Reply(x)

    let pzero : Parser<'a,'u> =
        fun stream -> Reply(Error, NoErrorMessages)

    let (>>=) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) =
        fun stream ->
            let reply1 = p stream
            if reply1.Status = Ok then
                let p2 = f reply1.Result
                let stateTag1 = stream.StateTag
                let reply2 = p2 stream
                if stateTag1 <> stream.StateTag then reply2
                else reply2.WithError(mergeErrors reply1.Error reply2.Error)
            else reconstructErrorReply reply1

    let (>>%) p x = p >>= fun _ -> preturn x

    let (>>.) p1 p2 = p1 >>= fun _ -> p2
    let (.>>) p1 p2 = p1 >>= fun x -> p2 >>% x

    let (.>>.) p1 p2 =
        p1 >>= fun x1 ->
        p2 >>= fun x2 -> preturn (x1, x2)

    let between popen pclose p = popen >>. (p .>> pclose)

    let (|>>) p f =
        p >>= fun a -> preturn (f a)

    let pipe2 p1 p2 f =
        p1 >>= fun x1 ->
        p2 >>= fun x2 -> preturn (f x1 x2)

    let pipe3 p1 p2 p3 f =
        p1 >>= fun x1 ->
        p2 >>= fun x2 ->
        p3 >>= fun x3 -> preturn (f x1 x2 x3)

    let pipe4 p1 p2 p3 p4 f =
        p1 >>= fun x1 ->
        p2 >>= fun x2 ->
        p3 >>= fun x3 ->
        p4 >>= fun x4 -> preturn (f x1 x2 x3 x4)

    let pipe5 p1 p2 p3 p4 p5 f =
        p1 >>= fun x1 ->
        p2 >>= fun x2 ->
        p3 >>= fun x3 ->
        p4 >>= fun x4 ->
        p5 >>= fun x5 -> preturn (f x1 x2 x3 x4 x5)

    let (<?>) (p: Parser<'a,'u>) label : Parser<'a,'u> =
        fun stream ->
            let stateTag = stream.StateTag
            let reply = p stream
            if stateTag <> stream.StateTag then reply
            else reply.WithError(expected label)

    let (<??>) (p: Parser<'a,'u>) label : Parser<'a,'u> =
        fun stream ->
            let mutable state = stream.State
            let reply = p stream
            if reply.Status = Ok then
                if state.Tag <> stream.StateTag then reply
                else reply.WithError(expected label)
            else
                if state.Tag <> stream.StateTag then
                    let error = compoundError label stream reply.Error
                    stream.BacktrackTo(&state)
                    Reply(FatalError, error)
                else
                    let error = match reply.Error with
                                | ErrorMessageList(NestedError(pos, ustate, msgs), NoErrorMessages)
                                    -> ErrorMessageList(CompoundError(label, pos, ustate, msgs), NoErrorMessages)
                                | _ -> expected label
                    reply.WithError(error)

    let fail msg : Parser<'a,'u> =
        fun stream -> Reply(Error, messageError msg)

    let failFatally msg : Parser<'a,'u> =
        fun stream -> Reply(FatalError, messageError msg)

    let (<|>) (p1: Parser<'a,'u>) (p2: Parser<'a,'u>) : Parser<'a,'u> =
        fun stream ->
            let stateTag = stream.StateTag
            let reply1 = p1 stream
            if reply1.Status = Error && stateTag = stream.StateTag then
                let reply2 = p2 stream
                if stateTag <> stream.StateTag then reply2
                else reply2.WithError(mergeErrors reply1.Error reply2.Error)
            else reply1

    let choice (ps: seq<Parser<'a,'u>>) =
        List.fold (fun p pc -> p <|> pc) pzero (List.ofSeq ps)

    let (<|>%) p x = p <|> preturn x

    let opt p = (p |>> Some) <|>% None

    let optional p = (p >>% ()) <|>% ()

    let notEmpty (p: Parser<'a,'u>) : Parser<'a,'u> =
        fun stream ->
            let stateTag = stream.StateTag
            let reply = p stream
            if reply.Status <> Ok || stateTag <> stream.StateTag then reply
            else Reply(Error, reply.Error)

    let attempt (p: Parser<'a,'u>) : Parser<'a,'u> =
        fun stream ->
            let mutable state = stream.State
            let reply = p stream
            if reply.Status = Ok then reply
            elif state.Tag = stream.StateTag then
                Reply(Error, reply.Error)
            else
                let error = nestedError stream reply.Error
                stream.BacktrackTo(&state)
                Reply(Error, error)

    let (>>=?) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) : Parser<'b,'u> =
        fun stream ->
            let mutable state = stream.State
            let reply1 = p stream
            if reply1.Status = Ok then
                let p2 = f reply1.Result
                let stateTag1 = stream.StateTag
                let reply2 = p2 stream
                if stateTag1 <> stream.StateTag then reply2
                else
                    let error = mergeErrors reply1.Error reply2.Error
                    if reply2.Status <> Error then reply2.WithError(error)
                    elif state.Tag = stateTag1 then Reply(Error, error)
                    else
                        let error = nestedError stream error
                        stream.BacktrackTo(&state)
                        Reply(Error, error)
            else reconstructErrorReply reply1

    let (>>?)  p1 p2 = p1 >>=? fun _ -> p2
    let (.>>?) p1 p2 = p1 >>=? fun x -> p2 >>% x

    let (.>>.?) p1 p2 =
        p1 >>=? fun x1 ->
        p2 >>=  fun x2 -> preturn (x1, x2)

    let lookAhead (p: Parser<'a,'u>) : Parser<'a,'u> =
        fun stream ->
            let mutable state = stream.State
            let reply = p stream
            if reply.Status = Ok then
                if state.Tag <> stream.StateTag then
                    stream.BacktrackTo(&state)
                Reply(reply.Result)
            else
                if state.Tag = stream.StateTag then
                    Reply(Error, reply.Error)
                else
                    let error = nestedError stream reply.Error
                    stream.BacktrackTo(&state)
                    Reply(Error, error)

    let followedByE (p: Parser<'a,'u>) error : Parser<unit,'u> =
        fun stream ->
            let mutable state = stream.State
            let reply = p stream
            if state.Tag <> stream.StateTag then
                stream.BacktrackTo(&state)
            if reply.Status = Ok then Reply(())
            else Reply(Error, error)

    let followedBy  p       = followedByE p NoErrorMessages
    let followedByL p label = followedByE p (expected label)

    let notFollowedByE (p: Parser<'a,'u>) error : Parser<unit,'u> =
        fun stream ->
            let mutable state = stream.State
            let reply = p stream
            if state.Tag <> stream.StateTag then
                stream.BacktrackTo(&state)
            if reply.Status <> Ok then Reply(())
            else Reply(Error, error)

    let notFollowedBy  p       = notFollowedByE p NoErrorMessages
    let notFollowedByL p label = notFollowedByE p (unexpected label)

    let tuple2  p1 p2          = pipe2 p1 p2          (fun a b       -> (a, b))
    let tuple3  p1 p2 p3       = pipe3 p1 p2 p3       (fun a b c     -> (a, b, c))
    let tuple4  p1 p2 p3 p4    = pipe4 p1 p2 p3 p4    (fun a b c d   -> (a, b, c, d))
    let tuple5  p1 p2 p3 p4 p5 = pipe5 p1 p2 p3 p4 p5 (fun a b c d e -> (a, b, c, d, e))

    let parray n (p : Parser<_,_>) =
        let rec loop i =
            if i = n then preturn []
            else
                p >>= fun hd -> (loop (i + 1) |>> fun tl -> hd::tl)
        loop 0 |>> Array.ofList

    let skipArray n p = parray n p |>> ignore

    // Note that the actual implemention of `many` tries to guard against
    // an infinite loop/recursion by throwing an exception if the given parser
    // argument succeeds without changing the stream.

    let rec many p = many1 p <|>% []
    and many1 p = p >>= fun hd -> many p |>> (fun tl -> hd::tl)

    // a version of many (p1 .>>. p2) that does not succeed if `p1` succeeds
    /// without changing the parser state and `p2` fails without changing the state
    let rec manyPair p1 p2 =
        p1 |>> (fun x1 ->
                    p2 >>= fun x2 ->
                        manyPair p1 p2 |>> fun tl -> (x1, x2)::tl)
        <|>% (preturn [])
        >>= fun p -> p

    let rec sepBy p sep = sepBy1 p sep <|>% []
    and sepBy1 p sep =
        p >>= fun hd -> manyPair sep p |>> fun sepPs -> hd::(List.map snd sepPs)

    let rec sepEndBy p sep = sepEndBy1 p sep <|>% []
    and sepEndBy1 p sep =
        p >>= fun hd -> sep >>. sepEndBy p sep <|>% [] |>> fun tl -> hd::tl

    let manyTill (p: Parser<'a,'u>) (endp: Parser<'b,'u>) =
        let rec parse (stream: CharStream<'u>) acc error  =
            let stateTag = stream.StateTag
            let replyE = endp stream
            if replyE.Status = Error && stateTag = stream.StateTag then
                let replyP = p stream
                if replyP.Status = Ok then
                    if stateTag = stream.StateTag then
                        failwith "infinite loop"
                    parse stream (replyP.Result::acc) replyP.Error
                else
                    let error = if stateTag <> stream.StateTag then replyP.Error
                                else mergeErrors (mergeErrors error replyE.Error) replyP.Error
                    Reply(replyP.Status, error)
            else
                let error = if stateTag <> stream.StateTag then replyE.Error
                            else mergeErrors error replyE.Error
                if replyE.Status = Ok then
                    Reply(Ok, List.rev acc, error)
                else
                    Reply(replyE.Status, error)

        fun stream -> parse stream [] NoErrorMessages

    let many1Till p endp = pipe2 p (manyTill p endp) (fun hd tl -> hd::tl)

    let chainl1 p op =
        p >>= fun x -> manyPair op p |>> fun opPs ->
            List.fold (fun x (f, y) -> f x y) x opPs

    let chainl p op x = chainl1 p op <|>% x

    let chainr1 p op =
        let rec calc x rhs =
            match rhs with
            | (f, y)::tl -> f x (calc y tl)
            | [] -> x

        pipe2 p (manyPair op p) calc

    let chainr p op x = chainr1 p op <|>% x


open FParsec.Primitives
open FParsec.Test.Test


let testPrimitives() =
    let content = "the content doesn't matter"
    use stream = new FParsec.CharStream<int>(content, 0, content.Length)


    let ps1  = Array.append (constantTestParsers 1    (expected "1")) [|(fun s -> s.UserState <- s.UserState + 1; Reply(1))|]
    let ps1b = constantTestParsers 11   (expected "1b")
    let ps1c = constantTestParsers 111  (expected "1c")
    let ps1d = constantTestParsers 1111 (expected "1d")

    let parserSeq4 =
        seq {for p1 in ps1 do
             for p2 in ps1b do
             for p3 in ps1c do
             for p4 in ps1d do
             yield [p1;p2;p3;p4]}

    let ps2  = constantTestParsers 2u   (expected "2")
    let ps2b = constantTestParsers 22u  (expected "2b")
    let ps2c = constantTestParsers 222u (expected "2c")
    let ps3  = constantTestParsers 3s (expected "3")
    let ps4  = constantTestParsers 4L (expected "4")
    let ps5  = constantTestParsers 5y (expected "5")

    let checkParser p1 p2 = checkParser p1 p2 stream

    let checkComb comb1 comb2 =
        for p in ps1 do
            checkParser (comb1 p) (comb2 p)

    let checkCombA comb1 comb2 arg =
        let adapt comb p = comb p arg
        checkComb (adapt comb1) (adapt comb2)

    let checkComb2 comb1 comb2 =
        for p1 in ps1 do
            for p2 in ps2 do
                checkParser (comb1 p1 p2) (comb2 p1 p2)

    let checkComb2A comb1 comb2 arg =
        let adapt comb p1 p2 = comb p1 p2 arg
        checkComb2 (adapt comb1) (adapt comb2)

    let checkBind bind1 bind2 =
        for p1 in ps1 do
            for p2 in ps2 do
                let f1 = fun r ->
                            r |> Equal 1
                            p2
                let f2 = fun r state ->
                            r |> Equal 1
                            p2 state
                checkParser (bind1 p1 f1) (bind2 p1 f1)
                checkParser (bind1 p1 f2) (bind2 p1 f2)

    let checkComb3 comb1 comb2 =
        for p1 in ps1 do
            for p2 in ps2 do
                for p3 in ps3 do
                    checkParser (comb1 p1 p2 p3) (comb2 p1 p2 p3)

    let checkComb3A comb1 comb2 arg =
        let adapt comb p1 p2 p3 = comb p1 p2 p3 arg
        checkComb3 (adapt comb1) (adapt comb2)

    let checkComb4 comb1 comb2 =
        for p1 in ps1 do
            for p2 in ps2 do
                for p3 in ps3 do
                    for p4 in ps4 do
                        checkParser (comb1 p1 p2 p3 p4) (comb2 p1 p2 p3 p4)
    let checkComb4A comb1 comb2 arg =
        let adapt comb p1 p2 p3 p4 = comb p1 p2 p3 p4 arg
        checkComb4 (adapt comb1) (adapt comb2)

    let checkComb5 comb1 comb2 =
        for p1 in ps1 do
            for p2 in ps2 do
                for p3 in ps3 do
                    for p4 in ps4 do
                        for p5 in ps5 do
                            checkParser (comb1 p1 p2 p3 p4 p5) (comb2 p1 p2 p3 p4 p5)
    let checkComb5A comb1 comb2 arg =
        let adapt comb p1 p2 p3 p4 p5 = comb p1 p2 p3 p4 p5 arg
        checkComb5 (adapt comb1) (adapt comb2)


    let testBasicPrimitives() =
        checkParser (preturn 42) (Reference.preturn 42)
        checkParser pzero   Reference.pzero
        checkCombA  (<?>)   Reference.(<?>) "test"
        checkBind   (>>=)   Reference.(>>=)
        checkCombA  (>>%)   Reference.(>>%) "test"
        checkComb2  (>>.)   Reference.(>>.)
        checkComb2  (.>>)   Reference.(.>>)
        checkComb2  (.>>.)  Reference.(.>>.)
        checkComb3  between Reference.between
        checkCombA  (|>>)   Reference.(|>>) (fun x -> x + 3)
        checkComb2A pipe2   Reference.pipe2 (fun a b       -> (a, b))
        checkComb3A pipe3   Reference.pipe3 (fun a b c     -> (a, b, c))
        checkComb4A pipe4   Reference.pipe4 (fun a b c d   -> (a, b, c, d))
        checkComb5A pipe5   Reference.pipe5 (fun a b c d e -> (a, b, c, d, e))
        checkComb2  tuple2  Reference.tuple2
        checkComb3  tuple3  Reference.tuple3
        checkComb4  tuple4  Reference.tuple4
        checkComb5  tuple5  Reference.tuple5

        checkCombA  (<?>)   Reference.(<?>)  "test"
        checkCombA  (<??>)  Reference.(<??>) "test"
        let btestp : Parser<_,_> =
            fun stream ->
                let mutable state0 = stream.State
                stream.Skip()
                stream.UserState <- stream.UserState + 1
                let error = nestedError stream (expected "test")
                stream.BacktrackTo(&state0)
                Reply(Error, error)

        checkParser ((<??>) btestp "btest") (Reference.(<??>) btestp "btest")
        checkParser (fail "test") (Reference.fail "test")
        checkParser (failFatally "test") (Reference.failFatally "test")

        for p1 in ps1 do
            for p2 in ps1b do
                checkParser ((<|>) p1 p2) (Reference.(<|>) p1 p2)

        for ps in Seq.append (Seq.singleton []) parserSeq4 do
            let refChoice  = Reference.choice ps
            let refChoiceL = refChoice <?> "test"

            // choice and choiceL use different implementations depending on whether
            // the type of the supplied sequence is a list, an array or a seq,
            // so we must test all 3 input types.
            let psa = Array.ofSeq ps
            let pss = match ps with
                      | [] -> Seq.empty
                      | [p1;p2;p3;p4] -> seq {yield p1
                                              yield p2
                                              yield p3
                                              yield p4}
                      | _ -> failwith "shouldn't happen"

            checkParser (choice  ps)         refChoice
            checkParser (choiceL ps  "test") refChoiceL
            checkParser (choice  psa)        refChoice
            checkParser (choiceL psa "test") refChoiceL
            checkParser (choice  pss)        refChoice
            checkParser (choiceL pss "test") refChoiceL

        checkCombA (<|>%)         Reference.(<|>%)          99
        checkComb  opt            Reference.opt
        checkComb  optional       Reference.optional
        checkComb  notEmpty       Reference.notEmpty
        checkComb  attempt        Reference.attempt
        checkBind  (>>=?)         Reference.(>>=?)
        checkComb2 (>>?)          Reference.(>>?)
        checkComb2 (.>>?)         Reference.(.>>?)
        checkComb2 (.>>.?)        Reference.(.>>.?)
        checkComb  followedBy     Reference.followedBy
        checkCombA followedByL    Reference.followedByL    "test"
        checkComb  notFollowedBy  Reference.notFollowedBy
        checkCombA notFollowedByL Reference.notFollowedByL "test"
        checkComb  lookAhead      Reference.lookAhead

    testBasicPrimitives()

    let testPArray() =
        // parray
        for ps in parserSeq4 do
            let p1, p2, pr = seqParserAndReset2 (List.ofSeq ps)
            checkParser (parray 0 p1)     (Reference.parray 0 p2);    pr()
            checkParser (parray 1 p1)     (Reference.parray 1 p2);    pr()
            checkParser (parray 2 p1)     (Reference.parray 2 p2);    pr()
            checkParser (parray 3 p1)     (Reference.parray 3 p2);    pr()
            checkParser (parray 4 p1)     (Reference.parray 4 p2);    pr()
            checkParser (skipArray 0 p1)  (Reference.skipArray 0 p2); pr()
            checkParser (skipArray 1 p1)  (Reference.skipArray 1 p2); pr()
            checkParser (skipArray 2 p1)  (Reference.skipArray 2 p2); pr()
            checkParser (skipArray 3 p1)  (Reference.skipArray 3 p2); pr()
            checkParser (skipArray 4 p1)  (Reference.skipArray 4 p2); pr()

    testPArray()

    let foldTestF acc x = (acc + 1)*(acc + 1) + x
    let reduceOrDefault f d lst = match lst with
                                  | [] -> d
                                  | _  -> List.reduce f lst

    let testMany() =
        let manySeq3 = // parserSeq4 without parsers that return Ok without changing the state
            seq {for p1 in ps1.[1..] do
                 for p2 in ps1b.[1..] do
                 for p3 in ps1c.[1..] do
                    yield [p1;p2;p3]}

        let f = foldTestF

        for ps in manySeq3 do
            let p1, p2, pr = seqParserAndReset2 ps

            let rMany = Reference.many p2
            checkParser (many       p1)    rMany;             pr()
            checkParser (skipMany   p1)   (rMany |>> ignore); pr()

            let rMany1 = Reference.many1 p2
            checkParser (many1       p1)   rMany1;             pr()
            checkParser (skipMany1   p1)  (rMany1 |>> ignore); pr()

        try many (preturn 0) stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()
        try many1 (preturn 0) stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()

    testMany()

    let sepByTestParsers r1 e1 r2 e2 =
        let ps1 = constantTestParsers r1 e1
        let ps2 = constantTestParsers r2 e2
        // all parser combinations except "ok without state change", "ok without state change"
        seq {
            for p2 in ps2.[1..] do
                yield ps1.[0], p2
            for p1 in ps1.[1..] do
                for p2 in ps2 do
                    yield p1, p2
        }

    let testSeqEndBy() =
        let sepEndSeq3 =
            seq {for p1       in (constantTestParsers 1 (expected "p1")).[1..] do
                 for sep1, p2 in sepByTestParsers 'a' (expected "sep1") 2 (expected "p2") do
                 for sep2, p3 in sepByTestParsers 'b' (expected "sep2") 3 (expected "p3") do
                 yield [p1; p2; p3;], [sep1; sep2;]}

        let f = foldTestF

        let mutable i = 0
        for ps, ss in sepEndSeq3 do
            i <- i + 1
            let p1, p2, pr = seqParserAndReset2 ps
            let s1, s2, sr = seqParserAndReset2 ss

            let rSepBy = Reference.sepBy p2 s2
            checkParser (sepBy       p1 s1)      rSepBy;                 pr(); sr()
            checkParser (skipSepBy   p1 s1)     (rSepBy |>> ignore);     pr(); sr()

            let rSepBy1 = Reference.sepBy1 p2 s2
            checkParser (sepBy1       p1 s1)     rSepBy1;                pr(); sr()
            checkParser (skipSepBy1   p1 s1)    (rSepBy1 |>> ignore);    pr(); sr()

            let rSepEndBy = Reference.sepEndBy p2 s2
            checkParser (sepEndBy       p1 s1)   rSepEndBy;              pr(); sr()
            checkParser (skipSepEndBy   p1 s1)  (rSepEndBy |>> ignore);  pr(); sr()

            let rSepEndBy1 = Reference.sepEndBy1 p2 s2
            checkParser (sepEndBy1       p1 s1)  rSepEndBy1;             pr(); sr()
            checkParser (skipSepEndBy1   p1 s1) (rSepEndBy1 |>> ignore); pr(); sr()

        try sepBy (preturn 0) (preturn 0) stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()
        try sepBy1 (preturn 0) (preturn 0) stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()
        try sepEndBy (preturn 0) (preturn 0) stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()
        try sepEndBy1 (preturn 0) (preturn 0) stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()

    testSeqEndBy()

    let testManyTill() =

        let manyTillSeq3 =
            seq {for endp1 in ps2 do
                 for p1    in ps1.[1..] do
                 for endp2 in ps2b do
                 for p2    in ps1b.[1..] do
                 for endp3 in ps2c do
                 for p3    in ps1c.[1..] do
                 yield [p1; p2; p3], [endp1; endp2; endp3; ps2c.[0]]}

        let f = foldTestF

        let mutable i = 0
        for ps, es in manyTillSeq3 do
            i <- i + 1
            let p1, p2, pr = seqParserAndReset2 ps
            let e1, e2, er = seqParserAndReset2 es

            let rManyTill = Reference.manyTill p2 e2
            checkParser (manyTill       p1 e1)  rManyTill;             pr(); er()
            checkParser (skipManyTill   p1 e1) (rManyTill |>> ignore); pr(); er()

            let rMany1Till = Reference.many1Till p2 e2
            checkParser (many1Till       p1 e1)  rMany1Till;             pr(); er()
            checkParser (skipMany1Till   p1 e1) (rMany1Till |>> ignore); pr(); er()


        try manyTill (preturn 0) (fail "test") stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()

        try many1Till (preturn 0) (fail "test") stream |> ignore; Fail ()
        with :? System.InvalidOperationException -> ()

    testManyTill()


    let testChain() =
        let chainSeq3 =
            seq {for p1      in ps1 do
                 for op1, p2 in sepByTestParsers (+) (expected "op1") 2 (expected "p2") do
                 for op2, p3 in sepByTestParsers (*) (expected "op2") 3 (expected "p3") do
                 yield [p1; p2; p3;], [op1; op2;]}

        for ps, ops in chainSeq3 do
            let p1,  p2,  pr  = seqParserAndReset2 ps
            let op1, op2, opr = seqParserAndReset2 ops

            checkParser (chainl  p1 op1 -1) (Reference.chainl  p2 op2 -1); pr(); opr()
            checkParser (chainl1 p1 op1)    (Reference.chainl1 p2 op2);    pr(); opr()
            checkParser (chainr  p1 op1 -1) (Reference.chainr  p2 op2 -1); pr(); opr()
            checkParser (chainr1 p1 op1)    (Reference.chainr1 p2 op2);    pr(); opr()

    testChain()


let run() =
    testPrimitives()

