// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.PrimitivesTests

open FParsec.Error

module Reference =
    [<Literal>]
    let Ok         = FParsec.Primitives.Ok
    [<Literal>]
    let Error      = FParsec.Primitives.Error
    [<Literal>]
    let FatalError = FParsec.Primitives.FatalError

    type Reply<'a,'u>  = FParsec.Primitives.Reply<'a,'u>
    type Parser<'a,'u> = FParsec.Primitives.Parser<'a,'u>

    type FParsec.Primitives.Reply<'a,'u>
      with
        member t.WithError(error: ErrorMessageList) =
            new Reply<'a,'u>(t.Status, t.Result, error, t.State)

    let reconstructErrorReply (reply: Reply<_,_>) =
        new Reply<_,_>(reply.Status, reply.Error, reply.State)

    let backtrack stateWhereErrorOccured error stateBacktrackedTo =
        let error = if stateWhereErrorOccured = stateBacktrackedTo then error
                    else backtrackError stateWhereErrorOccured error
        new Reply<_,_>(Error, error, stateBacktrackedTo)

    let preturn x =
        fun state ->
            new Reply<_,_>(x, state)

    let pzero : Parser<'a,'u> =
        fun state ->
            new Reply<_,_>(Error, NoErrorMessages, state)

    let (>>=) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) =
        fun state ->
            let reply1 = p state
            if reply1.Status = Ok then
                let p2 = f reply1.Result
                let reply2 = p2 reply1.State
                if reply2.State <> reply1.State then reply2
                else reply2.WithError(mergeErrors reply1.Error reply2.Error)
            else reconstructErrorReply reply1

    let (>>%) p x = p >>= fun _ -> preturn x

    let (>>.) p1 p2 = p1 >>= fun _ -> p2
    let (.>>) p1 p2 = p1 >>= fun x -> p2 >>% x

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

    let (<?>) (p: Parser<'a,'u>) label  =
        fun state ->
            let reply = p state
            if reply.State <> state then reply
            else reply.WithError(expectedError label)

    let (<??>) (p: Parser<'a,'u>) label  =
        fun state ->
            let reply = p state
            if reply.Status = Ok then
                if reply.State <> state then reply
                else reply.WithError(expectedError label)
            elif reply.State <> state then
                new Reply<_,_>(FatalError, compoundError label reply.State reply.Error, state)
            else
                let error = match reply.Error with
                            | AddErrorMessage(BacktrackPoint(pos, msgs), NoErrorMessages)
                                -> AddErrorMessage(CompoundError(label, pos, msgs), NoErrorMessages)
                            | _ -> expectedError label
                reply.WithError(error)

    let fail msg : Parser<'a,'u> =
        fun state -> new Reply<_,_>(Error, messageError msg, state)

    let failFatally msg : Parser<'a,'u> =
        fun state -> new Reply<_,_>(FatalError, messageError msg, state)

    let (<|>) (p1: Parser<'a,'u>) (p2: Parser<'a,'u>) =
        fun state ->
            let reply1 = p1 state
            if reply1.Status = Error && reply1.State = state then
                let reply2 = p2 state
                if reply2.State <> reply1.State then reply2
                else reply2.WithError(mergeErrors reply1.Error reply2.Error)
            else reply1

    let choice (ps: seq<Parser<'a,'u>>) =
        List.fold (fun p pc -> p <|> pc) pzero (List.ofSeq ps)

    let (<|>%) p x = p <|> preturn x

    let opt p = (p |>> Some) <|>% None

    let optional p = (p >>% ()) <|>% ()

    let attempt (p: Parser<'a,'u>) =
        fun state ->
            let reply = p state
            if reply.Status = Ok then reply
            else backtrack reply.State reply.Error state

    let (>>=?) (p: Parser<'a,'u>) (f: 'a -> Parser<'b,'u>) =
        fun state ->
            let reply1 = p state
            if reply1.Status = Ok then
                let p2 = f reply1.Result
                let reply2 = p2 reply1.State
                if reply2.State <> reply1.State then reply2
                else
                    let error = mergeErrors reply1.Error reply2.Error
                    if reply2.Status = Ok then reply2.WithError(error)
                    else backtrack reply2.State error state
            else reconstructErrorReply reply1

    let (>>?)  p1 p2 = p1 >>=? fun _ -> p2
    let (.>>?) p1 p2 = p1 >>=? fun x -> p2 >>% x

    let lookAhead (p: Parser<'a,'u>) =
        fun state ->
            let reply = p state
            if reply.Status = Ok then
                new Reply<_,_>(reply.Result, state)
            else
                backtrack reply.State reply.Error state

    let followedByE (p: Parser<'a,'u>) error =
        fun state ->
            let reply = p state
            if reply.Status = Ok then new Reply<_,_>((), state)
            else new Reply<_,_>(Error, error, state)

    let followedBy  p       = followedByE p NoErrorMessages
    let followedByL p label = followedByE p (expectedError label)

    let notFollowedByE (p: Parser<'a,'u>) error =
        fun state ->
            let reply = p state
            if reply.Status <> Ok then new Reply<_,_>((), state)
            else new Reply<_,_>(Error, error, state)

    let notFollowedBy  p       = notFollowedByE p NoErrorMessages
    let notFollowedByL p label = notFollowedByE p (unexpectedError label)

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
    // argument succeeds without changing the state.

    let rec many p = many1 p <|>% []
    and many1 p = p >>= fun hd -> many p |>> (fun tl -> hd::tl)

    /// a version of `(p1 >>. p2) <|>% x` that does not succeed if `p1` succeeds
    /// without changing the state and `p2` fails without changing the state
    let ifP1ThenP2ElseReturnX p1 p2 x =
        (p1 >>% p2 <|>% (preturn x)) >>= fun p -> p

    let rec sepBy p sep = sepBy1 p sep <|>% []
    and sepBy1 p sep =
        p >>= fun hd -> ifP1ThenP2ElseReturnX sep (sepBy1 p sep) [] |>> fun tl -> hd::tl

    let rec sepEndBy p sep = sepEndBy1 p sep <|>% []
    and sepEndBy1 p sep = p >>= fun hd -> sep >>. sepEndBy p sep <|>% [] |>> fun tl -> hd::tl

    let endBy  p sep = many  (p .>> sep)
    let endBy1 p sep = many1 (p .>> sep)

    let manyTill (p: Parser<'a,'u>) (endp: Parser<'b,'u>) =
        let rec parse acc error (state: FParsec.State<'u>) =
            let replyE = endp state
            if replyE.Status = Ok then
                let error = mergeErrorsIfNeeded state error replyE.State replyE.Error
                new Reply<_,_>(Ok, List.rev acc, error, replyE.State)
            else
                // in case endp failed after changing the state we tentatively backtrack...
                let replyP = p state
                if replyP.Status = Ok then
                    let error = mergeErrorsIfNeeded state error replyP.State replyP.Error
                    parse (replyP.Result::acc) error replyP.State
                elif replyP.Status = Error && replyP.State = state then
                    // ... but then return endp's reply if p fails non-fatally without changing the state
                    let error = if replyE.State <> state then replyE.Error
                                else mergeErrors (mergeErrors error replyE.Error) replyP.Error
                    new Reply<_,_>(replyE.Status, error, replyE.State)
                else
                    let error = mergeErrorsIfNeeded state error replyP.State replyP.Error
                    new Reply<_,_>(replyP.Status, error, replyP.State)

        fun state -> parse [] NoErrorMessages state

    let chainl1 p op =
        let rec fold x = (op >>= fun f ->
                          p  >>= fun y -> fold (f x y)) <|>% x
        p >>= fun x -> fold x

    let chainl p op x = chainl1 p op <|>% x

    let rec chainr1 p op =
        p >>= fun x ->
            (op >>= fun f -> chainr1 p op
                             |>> fun y -> f x y) <|>% x

    let rec chainr p op x = chainr1 p op <|>% x


open FParsec.Primitives
open FParsec.Test.Test


let testPrimitives() =
    let content = "the content doesn't matter"
    use cs = new FParsec.CharStream(content, 0, content.Length)
    let s0 = new FParsec.State<_>(cs, 0, "")

    let ps1  = constantTestParsers 1    (expectedError "1")
    let ps1b = constantTestParsers 11   (expectedError "1b")
    let ps1c = constantTestParsers 111  (expectedError "1c")
    let ps1d = constantTestParsers 1111 (expectedError "1d")

    let parserSeq4 =
        seq {for p1 in ps1 do
             for p2 in ps1b do
             for p3 in ps1c do
             for p4 in ps1d do
             yield [p1;p2;p3;p4]}

    let ps2  = constantTestParsers 2u   (expectedError "2")
    let ps2b = constantTestParsers 22u  (expectedError "2b")
    let ps2c = constantTestParsers 222u (expectedError "2c")
    let ps3  = constantTestParsers 3s (expectedError "3")
    let ps4  = constantTestParsers 4L (expectedError "4")
    let ps5  = constantTestParsers 5y (expectedError "5")

    let checkParser p1 p2 = checkParser p1 p2 s0

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
        let btestp = fun (state: FParsec.State<_>) -> new Reply<_,_>(Error, backtrackError (state.Advance(1)) (expectedError "test"), state)
        checkParser ((<??>) btestp "btest") (Reference.(<??>) btestp "btest")
        checkParser (fail "test") (Reference.fail "test")
        checkParser (failFatally "test") (Reference.failFatally "test")


        for p1 in ps1 do
            for p2 in ps1b do
                checkParser ((<|>) p1 p2) (Reference.(<|>) p1 p2)

        for ps in parserSeq4 do
            let refChoice  = Reference.choice ps
            let refChoiceL = refChoice <?> "test"

            // choice and choiceL use different implementations depending on whether
            // the type of the supplied sequence is a list, an array or a seq,
            // so we must test all 3 input types.
            let psa = Array.ofSeq ps
            let pss = match ps with
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
        checkComb  attempt        Reference.attempt
        checkBind  (>>=?)         Reference.(>>=?)
        checkComb2 (>>?)          Reference.(>>?)
        checkComb2 (.>>?)         Reference.(.>>?)
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

            let reply = Reference.many p2 s0
            let rMany = fun state -> reply
            checkParser (many       p1)       rMany;                           pr()
            checkParser (manyRev    p1)      (rMany |>> List.rev);             pr()
            checkParser (skipMany   p1)      (rMany |>> ignore);               pr()
            checkParser (manyFold   0  f p1) (rMany |>> List.fold f 0);        pr()
            checkParser (manyReduce f -1 p1) (rMany |>> reduceOrDefault f -1); pr()

            let reply1 = Reference.many1 p2 s0
            let rMany1 = fun state -> reply1
            checkParser (many1       p1)      rMany1;                    pr()
            checkParser (many1Rev    p1)     (rMany1 |>> List.rev);      pr()
            checkParser (skipMany1   p1)     (rMany1 |>> ignore);        pr()
            checkParser (many1Fold   0 f p1) (rMany1 |>> List.fold f 0); pr()
            checkParser (many1Reduce f p1)   (rMany1 |>> List.reduce f); pr()

        try many (preturn 0) s0 |> ignore; Fail ()
        with Failure msg -> ()
        try many1 (preturn 0) s0 |> ignore; Fail ()
        with Failure msg -> ()

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
            seq {for p1       in (constantTestParsers 1 (expectedError "p1")).[1..] do
                 for sep1, p2 in sepByTestParsers 'a' (expectedError "sep1") 2 (expectedError "p2") do
                 for sep2, p3 in sepByTestParsers 'b' (expectedError "sep2") 3 (expectedError "p3") do
                 yield [p1; p2; p3;], [sep1; sep2;]}

        let f = foldTestF

        let mutable i = 0
        for ps, ss in sepEndSeq3 do
            i <- i + 1
            let p1, p2, pr = seqParserAndReset2 ps
            let s1, s2, sr = seqParserAndReset2 ss

            let reply1 = Reference.sepBy p2 s2 s0
            let rSepBy = fun state -> reply1
            checkParser (sepBy       p1 s1)       rSepBy;                           pr(); sr()
            checkParser (sepByRev    p1 s1)      (rSepBy |>> List.rev);             pr(); sr()
            checkParser (skipSepBy   p1 s1)      (rSepBy |>> ignore);               pr(); sr()
            checkParser (sepByFold   0 f  p1 s1) (rSepBy |>> List.fold f 0);        pr(); sr()
            checkParser (sepByReduce f -1 p1 s1) (rSepBy |>> reduceOrDefault f -1); pr(); sr()

            let reply2 = Reference.sepBy1 p2 s2 s0
            let rSepBy1 = fun state -> reply2
            checkParser (sepBy1       p1 s1)      rSepBy1;                    pr(); sr()
            checkParser (sepBy1Rev    p1 s1)     (rSepBy1 |>> List.rev);      pr(); sr()
            checkParser (skipSepBy1   p1 s1)     (rSepBy1 |>> ignore);        pr(); sr()
            checkParser (sepBy1Fold   0 f p1 s1) (rSepBy1 |>> List.fold f 0); pr(); sr()
            checkParser (sepBy1Reduce f p1 s1)   (rSepBy1 |>> List.reduce f); pr(); sr()


            let reply3 = Reference.sepEndBy p2 s2 s0
            let rSepEndBy = fun state -> reply3
            checkParser (sepEndBy       p1 s1)       rSepEndBy;                           pr(); sr()
            checkParser (sepEndByRev    p1 s1)      (rSepEndBy |>> List.rev);             pr(); sr()
            checkParser (skipSepEndBy   p1 s1)      (rSepEndBy |>> ignore);               pr(); sr()
            checkParser (sepEndByFold   0 f  p1 s1) (rSepEndBy |>> List.fold f 0);        pr(); sr()
            checkParser (sepEndByReduce f -1 p1 s1) (rSepEndBy |>> reduceOrDefault f -1); pr(); sr()

            let reply4 = Reference.sepEndBy1 p2 s2 s0
            let rSepEndBy1 = fun state -> reply4
            checkParser (sepEndBy1       p1 s1)      rSepEndBy1;                    pr(); sr()
            checkParser (sepEndBy1Rev    p1 s1)     (rSepEndBy1 |>> List.rev);      pr(); sr()
            checkParser (skipSepEndBy1   p1 s1)     (rSepEndBy1 |>> ignore);        pr(); sr()
            checkParser (sepEndBy1Fold   0 f p1 s1) (rSepEndBy1 |>> List.fold f 0); pr(); sr()
            checkParser (sepEndBy1Reduce f p1 s1)   (rSepEndBy1 |>> List.reduce f); pr(); sr()

        try sepBy (preturn 0) (preturn 0) s0 |> ignore; Fail ()
        with Failure msg -> ()
        try sepBy1 (preturn 0) (preturn 0) s0 |> ignore; Fail ()
        with Failure msg -> ()
        try sepEndBy (preturn 0) (preturn 0) s0 |> ignore; Fail ()
        with Failure msg -> ()
        try sepEndBy1 (preturn 0) (preturn 0) s0 |> ignore; Fail ()
        with Failure msg -> ()

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

            let reply1 = Reference.manyTill p2 e2 s0
            let rManyTill = fun state -> reply1
            checkParser (manyTill       p1 e1)       rManyTill;                           pr(); er()
            checkParser (manyTillRev    p1 e1)      (rManyTill |>> List.rev);             pr(); er()
            checkParser (skipManyTill   p1 e1)      (rManyTill |>> ignore);               pr(); er()
            checkParser (manyTillFold   0 f p1 e1)  (rManyTill |>> List.fold f 0);        pr(); er()
            checkParser (manyTillReduce f -1 p1 e1) (rManyTill |>> reduceOrDefault f -1); pr(); er()

        try manyTill (preturn 0) (fail "test") s0 |> ignore; Fail ()
        with Failure msg -> ()

    testManyTill()


    let testChain() =
        let chainSeq3 =
            seq {for p1      in ps1 do
                 for op1, p2 in sepByTestParsers (+) (expectedError "op1") 2 (expectedError "p2") do
                 for op2, p3 in sepByTestParsers (*) (expectedError "op2") 3 (expectedError "p3") do
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