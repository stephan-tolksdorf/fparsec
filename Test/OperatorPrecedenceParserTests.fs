// Copyright (c) Stephan Tolksdorf 2008-2011
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.OperatorPrecedenceParserTests

open FParsec
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

open FParsec.Test.Test

// the tests for this module are somewhat ad hoc and ugly...

type Expr = O1 of Expr*Expr
          | O2 of Expr*Expr
          | O3 of Expr*Expr
          | Pre1 of Expr
          | Pre2 of Expr
          | Po1 of Expr
          | Po2 of Expr
          | Val of int
          | T1 of Expr*Expr*Expr
          | T2 of Expr*Expr*Expr

type Expr2 = Op    of (string*Position)*Expr2*Expr2
           | Pre   of (string*Position)*Expr2
           | Post  of (string*Position)*Expr2
           | Tern  of (string*Position)*(string*Position)*Expr2*Expr2*Expr2
           | Value of Position*int


let ws = spaces
let ws1 = spaces1

let testRemove (opp: OperatorPrecedenceParser<_,_,_>) (op: Operator<_,_,_>) =
    try opp.AddOperator(op); Fail()
    with :? System.ArgumentException -> ()
    opp.RemoveOperator(op) |> True
    opp.RemoveOperator(op) |> False
    opp.AddOperator(op)
    match op.Type with
    | OperatorType.Prefix  ->
        opp.RemovePrefixOperator(op.String)  |> True
        opp.RemovePrefixOperator(op.String)  |> False
    | OperatorType.Postfix ->
        opp.RemovePostfixOperator(op.String) |> True
        opp.RemovePostfixOperator(op.String) |> False
    | OperatorType.Infix when not op.IsTernary ->
        opp.RemoveInfixOperator(op.String) |> True
        opp.RemoveInfixOperator(op.String) |> False
    | OperatorType.Infix when op.IsTernary ->
        opp.RemoveTernaryOperator(op.String, op.TernaryRightString) |> True
        opp.RemoveTernaryOperator(op.String, op.TernaryRightString) |> False
    | _ -> Fail()

let testRemoveSeq rand opp ops =
    let ops = Seq.toArray ops
    shuffleArray rand ops
    for op in ops do
        testRemove opp op

let testOpParser() =
    let opp = new OperatorPrecedenceParser<_,_,_>()
    let expr = opp.ExpressionParser
    opp.TermParser <- preturn System.Int32.MinValue

    // check "greedy" op parsing, correct sorting and finding in internal op data structure
    opp.AddOperator(PrefixOperator("\u0302", ws, 1, true, fun _ -> 0))

    opp.AddOperator(PrefixOperator("\u0303", ws, 1, true, fun _ -> 1))
    opp.AddOperator(PrefixOperator("\u0203", ws, 1, true, fun _ -> 2))
    opp.AddOperator(PrefixOperator("\u0403", ws, 1, true, fun _ -> 3))
    opp.AddOperator(PrefixOperator("\u0503", ws, 1, true, fun _ -> 4))
    opp.AddOperator(PrefixOperator("\u0103", ws, 1, true, fun _ -> 5))

    opp.AddOperator(PrefixOperator("\u0304", ws, 1, true, fun _ -> -1))

    opp.AddOperator(PrefixOperator("\u0303\u0303", ws, 1, true, fun _ -> 6))
    opp.AddOperator(PrefixOperator("\u0303\u0302", ws, 1, true, fun _ -> 7))
    opp.AddOperator(PrefixOperator("\u0303\u0304", ws, 1, true, fun _ -> 8))

    opp.AddOperator(PrefixOperator("\u0203\u0202", ws, 1, true, fun _ -> 9))
    opp.AddOperator(PrefixOperator("\u0203\u0203", ws, 1, true, fun _ -> 10))
    opp.AddOperator(PrefixOperator("\u0203\u0204", ws, 1, true, fun _ -> 11))

    opp.AddOperator(PrefixOperator("\u0403\u0404", ws, 1, true, fun _ -> 12))
    opp.AddOperator(PrefixOperator("\u0403\u0403", ws, 1, true, fun _ -> 13))
    opp.AddOperator(PrefixOperator("\u0403\u0402", ws, 1, true, fun _ -> 14))

    opp.AddOperator(PrefixOperator("\u0503\u0403", ws, 1, true, fun _ -> 15))
    opp.AddOperator(PrefixOperator("\u0503\u0402", ws, 1, true, fun _ -> 16))
    opp.AddOperator(PrefixOperator("\u0503\u0404", ws, 1, true, fun _ -> 17))

    opp.AddOperator(PrefixOperator("\u0103\u0103\u0103\u0103", ws, 1, true, fun _ -> 18))
    opp.AddOperator(PrefixOperator("\u0103\u0103\u0103", ws, 1, true, fun _ -> 19))
    opp.AddOperator(PrefixOperator("\u0103\u0102\u0102", ws, 1, true, fun _ -> 20))
    opp.AddOperator(PrefixOperator("\u0103\u0102", ws, 1, true, fun _ -> 21))
    opp.AddOperator(PrefixOperator("\u0103\u0103", ws, 1, true, fun _ -> 22))
    opp.AddOperator(PrefixOperator("\u0103\u0101", ws, 1, true, fun _ -> 23))

    opp.AddOperator(PrefixOperator("\u0303\u0303\u0303", ws, 1, true, fun _ -> 24))
    opp.AddOperator(PrefixOperator("\u0303\u0303\u0303\u0303", ws, 1, true, fun _ -> 25))
    opp.AddOperator(PrefixOperator("\u0303\u0302\u0302", ws, 1, true, fun _ -> 26))

    opp.AddOperator(PrefixOperator("\u0203\u0202\u0202\u0202", ws, 1, true, fun _ -> 27))
    opp.AddOperator(PrefixOperator("\u0203\u0202\u0202", ws, 1, true, fun _ -> 28))
    opp.AddOperator(PrefixOperator("\u0203\u0203\u0203", ws, 1, true, fun _ -> 29))

    opp.AddOperator(PrefixOperator("\u0403\u0403\u0403", ws, 1, true, fun _ -> 30))
    opp.AddOperator(PrefixOperator("\u0403\u0402\u0402", ws, 1, true, fun _ -> 31))
    opp.AddOperator(PrefixOperator("\u0403\u0402\u0402\u402", ws, 1, true, fun _ -> 32))

    opp.AddOperator(PrefixOperator("\u0603\u0602", ws, 1, true, fun _ -> 33))
    opp.AddOperator(PrefixOperator("\u0603\u0603", ws, 1, true, fun _ -> 34))

    let expectedPrefix = Errors.ExpectedPrefixOperator

    let ROk content result parser = ROkE content content.Length result expectedPrefix parser
    let ROkI content i result parser = ROkE content i result expectedPrefix parser

    expr |> ROkI "\u0301" 0 System.Int32.MinValue

    expr |> ROk "\u0302" 0

    expr |> ROk "\u0303" 1
    expr |> ROk "\u0203" 2
    expr |> ROk "\u0403" 3
    expr |> ROk "\u0503" 4
    expr |> ROk "\u0103" 5

    expr |> ROkI "\u0003" 0 System.Int32.MinValue
    expr |> ROkI "\u0703" 0 System.Int32.MinValue

    expr |> ROk "\u0304" -1

    expr |> ROk "\u0303\u0303" 6
    expr |> ROk "\u0303\u0302" 7
    expr |> ROk "\u0303\u0304" 8

    expr |> ROkI "\u0003\u0303" 0 System.Int32.MinValue
    expr |> ROkI "\u0703\u0302" 0 System.Int32.MinValue

    expr |> ROkI "\u0603\u0601" 0 System.Int32.MinValue
    expr |> ROk "\u0603\u0602" 33
    expr |> ROk "\u0603\u0603" 34
    expr |> ROkI "\u0603\u0604" 0 System.Int32.MinValue

    expr |> ROk "\u0203\u0202" 9
    expr |> ROk "\u0203\u0203" 10
    expr |> ROk "\u0203\u0204" 11

    expr |> ROk "\u0403\u0404" 12
    expr |> ROk "\u0403\u0403" 13
    expr |> ROk "\u0403\u0402" 14

    expr |> ROk "\u0503\u0403" 15
    expr |> ROk "\u0503\u0402" 16
    expr |> ROk "\u0503\u0404" 17

    expr |> ROk "\u0103\u0103\u0103\u0103" 18
    expr |> ROk "\u0103\u0103\u0103" 19
    expr |> ROkI "\u0103\u0103\u0103\u0102" 3 19
    expr |> ROk "\u0103\u0102\u0102" 20
    expr |> ROk "\u0103\u0102" 21
    expr |> ROk "\u0103\u0103" 22
    expr |> ROk "\u0103\u0101" 23
    expr |> ROkI "\u0103\u0101\u0102" 2 23

    expr |> ROk "\u0303\u0303\u0303" 24
    expr |> ROk "\u0303\u0303\u0303\u0302" 24
    expr |> ROk "\u0303\u0303\u0303\u0303" 25
    expr |> ROk "\u0303\u0302\u0302" 26

    expr |> ROk "\u0203\u0202\u0202\u0202" 27
    expr |> ROk "\u0203\u0202\u0202" 28
    expr |> ROkI "\u0203\u0202\u0202\u0201" 3 28
    expr |> ROk "\u0203\u0203\u0203" 29

    expr |> ROk "\u0403\u0403\u0403" 30
    expr |> ROk "\u0403\u0402\u0402" 31
    expr |> ROk "\u0403\u0402\u0402\u402" 32
    expr |> ROkI "\u0403\u0402\u0402\u0401" 3 31

    // check whitespace parsing and parser state propagation

    let withMsg m p = p .>> (fail m <|>% ())


    let testPrefixOpParser wsParser termParser =
        opp.AddOperator(PrefixOperator("+", wsParser, 1, true, fun x -> x + 1))
        opp.TermParser <- termParser
        let expr = opp.ExpressionParser
        let expr2 = pipe2 (many (pchar '+' >>? wsParser <?> Strings.PrefixOperator)) termParser
                          (fun ps i -> i + List.length ps)

        checkParserStr expr expr2 ""
        checkParserStr expr expr2 "+"
        checkParserStr expr expr2 "1"
        checkParserStr expr expr2 "+"
        checkParserStr expr expr2 "+ "
        checkParserStr expr expr2 "+1"
        checkParserStr expr expr2 "+ 1"
        checkParserStr expr expr2 "++"
        checkParserStr expr expr2 "+ +"
        checkParserStr expr expr2 "+ + "
        checkParserStr expr expr2 "++1"
        checkParserStr expr expr2 "++ 1"
        checkParserStr expr expr2 "+ + 1"

        opp.RemovePrefixOperator("+") |> True


    testPrefixOpParser (ws |> withMsg "e1") (preturn 0 |> withMsg "e2")
    testPrefixOpParser (ws |> withMsg "e1") (fail "e2")
    testPrefixOpParser (ws |> withMsg "e1") (failFatally "e2")
    testPrefixOpParser (ws |> withMsg "e1") (preturn 0 |> withMsg "e2")
    testPrefixOpParser (fail "e1")  (preturn 0 |> withMsg "e2" )
    testPrefixOpParser (failFatally "e1")  (preturn 0 |> withMsg "e2" )
    testPrefixOpParser (ws |> withMsg "e1") (pint32 |> withMsg "e2")
    testPrefixOpParser (ws1 |> withMsg "e1") (preturn 0 |> withMsg "e2")
    testPrefixOpParser (ws1 >>. fail "e1") (preturn 0 |> withMsg "e2")
    testPrefixOpParser (ws1 |> withMsg "e1") (pint32 |> withMsg "e2")

    let testInfixOpParser wsParser termParser =
        opp.AddOperator(InfixOperator("+", wsParser, 1, Associativity.Left, fun x y -> x + y))
        opp.TermParser <- termParser
        let expr = opp.ExpressionParser

        let expect label = preturn () <?> label
        let term = expect Strings.PrefixOperator >>. termParser
        let infixOp = pstring "+" >>? wsParser <?> Strings.InfixOperator
        let expr2 = pipe2 term ((infixOp >>. (term .>> (opt infixOp))) <|>% 0)
                          (fun x y -> x + y)

        checkParserStr expr expr2 "+"
        checkParserStr expr expr2 "+ "
        checkParserStr expr expr2 "1+"
        checkParserStr expr expr2 "1 +"
        checkParserStr expr expr2 "1 + "
        checkParserStr expr expr2 "1+2"
        checkParserStr expr expr2 "1 +2"
        checkParserStr expr expr2 "1 + 2"
        checkParserStr expr expr2 "1 + 2"
        checkParserStr expr expr2 "1+2 "
        checkParserStr expr expr2 "1 +2 "
        checkParserStr expr expr2 "1 + 2 "
        checkParserStr expr expr2 "1 + 2 "

        opp.RemoveOperator(InfixOperator("+", wsParser, 1, Associativity.Left, fun x y -> x + y)) |> False
        opp.RemoveInfixOperator("+") |> True

    testInfixOpParser (ws |> withMsg "e1")  (preturn 0 |> withMsg "e2")
    testInfixOpParser (fail "e1")           (pint32 .>> ws |> withMsg "e2")
    testInfixOpParser (failFatally "e1")    (pint32 .>> ws |> withMsg "e2")
    testInfixOpParser (ws |> withMsg "e1")  (pint32 .>> ws |> withMsg "e2")
    testInfixOpParser (ws1 |> withMsg "e1") (pint32 .>> ws |> withMsg "e2")
    testInfixOpParser (ws1 >>. fail "e1")   (pint32 .>> ws |> withMsg "e2")
    testInfixOpParser (ws1 |> withMsg "e1") (pint32 .>> ws |> withMsg "e2")


    let testTernary2ndOpParser opWsParser =
        let wsm =  (ws |> withMsg "e1")

        let term = (pint32 |> withMsg "e2") .>> wsm
        opp.TermParser <- term
        opp.AddOperator(TernaryOperator("?", wsm, ":", opWsParser, 1, Associativity.Left, fun x y z -> x + y + z))
        opp.MissingTernary2ndStringErrorFormatter <- fun (_, _, op, _) -> expected op.TernaryRightString

        let expr2 =
            let op str wsParser = skipString str >>? wsParser
            let expect label = preturn () <?> label
            let term = expect "prefix operator" >>. term
            let op1 = op "?" wsm <?> "infix operator"
            let op2 = expect Strings.InfixOperator >>. (op ":" opWsParser <?> ":")
            pipe2 term (tuple2 (op1 >>. term) (op2 >>. term .>> expect Strings.InfixOperator) <|>% (0,0))
                  (fun x (y,z) -> x + y + z)

        checkParserStr expr expr2 "1 ?"
        checkParserStr expr expr2 "1 ?"
        checkParserStr expr expr2 "1 ?: 3"
        checkParserStr expr expr2 "1 ? : 3"
        checkParserStr expr expr2 "1 ? 2"
        checkParserStr expr expr2 "1 ? 2 "
        checkParserStr expr expr2 "1 ? 2: "
        checkParserStr expr expr2 "1 ? 2 :"
        checkParserStr expr expr2 "1 ? 2:3"
        checkParserStr expr expr2 "1 ? 2: 3"
        checkParserStr expr expr2 "1 ? 2 :3"
        checkParserStr expr expr2 "1 ? 2 : 3"

        opp.RemoveTernaryOperator("?", ":") |> True

    testTernary2ndOpParser (ws |> withMsg "e")
    testTernary2ndOpParser (ws1 |> withMsg "e")
    testTernary2ndOpParser (fail "e")
    testTernary2ndOpParser (failFatally "e")
    testTernary2ndOpParser (ws1 >>. fail "e")

    let rand = new System.Random(1234)
    testRemoveSeq rand opp opp.Operators


let testConflictAfterStringParserHandling() =
    let opp = new OperatorPrecedenceParser<_,_,_>()
    let expr = opp.ExpressionParser
    opp.TermParser <- pint32

    let conflictError = messageError "conflict"
    opp.OperatorConflictErrorFormatter <- fun _ _ -> conflictError
    opp.AddOperator(PrefixOperator("+",  spaces, 1, false, fun x -> x + 1))
    opp.AddOperator(PrefixOperator("++", spaces, 1, true, fun x -> x + 2))

    expr |> ROk "+ ++1" 5 4

    opp.RemovePrefixOperator("++") |> True
    opp.AddOperator(PrefixOperator("++", spaces, 1, false, fun x -> x + 2))

    expr |> RError "+ ++1" 2 conflictError

    opp.RemovePrefixOperator("++") |> True
    opp.AddOperator(PrefixOperator("++", spaces1, 1, false, fun x -> x + 2))

    expr |> RError "+ ++1" 2 (mergeErrors Errors.ExpectedPrefixOperator Errors.ExpectedInt32)

    opp.RemovePrefixOperator("++") |> True
    opp.AddOperator(PrefixOperator("++", failFatally "e", 1, false, fun x -> x + 2))

    expr |> RFatalError "+ ++1" 4 (messageError "e")

    opp.RemovePrefixOperator("++") |> True

    opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> x + y))
    opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Right, fun x y -> x - y))

    expr |> RError "1+2- 3" 3 conflictError

    opp.RemoveInfixOperator("-") |> True
    opp.AddOperator(InfixOperator("-", spaces1, 1, Associativity.Right, fun x y -> x - y))

    expr |> ROkE "1+2-3" 3 3 Errors.ExpectedInfixOperator

    opp.RemoveInfixOperator("-") |> True
    opp.AddOperator(InfixOperator("-",  failFatally "e", 1, Associativity.Right, fun x y -> x - y))
    expr |> RFatalError "1+2- 3" 4 (messageError "e")

let testAlternativeOpConstructors() =
    let opp = new OperatorPrecedenceParser<_,_,_>()
    let expr = opp.ExpressionParser
    let str = skipString

    let posWS = getPosition .>> ws

    let term = pipe2 getPosition (pint32 .>> ws) (fun pos x -> Value(pos, x))
    opp.TermParser <- term

    opp.AddOperator(PrefixOperator("-", posWS, 1, true, (), fun pos x -> Pre(("-", pos), x)))
    opp.AddOperator(PostfixOperator("++", posWS, 1, true, (), fun pos x -> Post(("++", pos), x)))
    opp.AddOperator(InfixOperator("*", posWS, 1, Associativity.Left, (), fun pos x y -> Op(("*", pos), x, y)))
    opp.AddOperator(TernaryOperator("?", posWS, ":", posWS, 1, Associativity.Left, (), fun pos1 pos2 x y z -> Tern(("?", pos1), (":", pos2), x, y, z)))

    let op = many1SatisfyL (fun c -> match c with '*' | '+' | '-' | '?' | ':' -> true | _ -> false) "operator" .>>. posWS
    let expectInfixOrPostfix = fun stream -> Reply(Ok, Errors.ExpectedInfixOrPostfixOperator)
    let expr2 =
        pipe3 (tuple4 term op term op) (tuple2 op term) (tuple2 op (tuple2 op term))
              (fun (v12, multOp, v3, plusPlusOp) (qMarkOp, v4) (colonOp, (minusOp, v5)) ->
                  Tern(qMarkOp, colonOp, Op(multOp, v12, Post(plusPlusOp, v3)), v4, Pre(minusOp, v5)))
        .>> expectInfixOrPostfix

    checkParserStr expr expr2 "12 * 3++ ? 4 : -5"

    let rand = new System.Random(1234)
    testRemoveSeq rand opp opp.Operators


let testPrecAndAssoc() =
    let opp = new OperatorPrecedenceParser<_,_,_>()
    let expr = opp.ExpressionParser

    opp.TermParser <- pint32 .>> ws |>> Val

    opp.AddOperator(InfixOperator("o1l", ws, 1, Associativity.Left,   fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o1r", ws, 1, Associativity.Right,  fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o1n", ws, 1, Associativity.None,   fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o2l", ws, 1, Associativity.Left,   fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o2r", ws, 1, Associativity.Right,  fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o2n", ws, 1, Associativity.None,   fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o3l", ws, 1, Associativity.Left,   fun x y -> O3(x,y)))
    opp.AddOperator(InfixOperator("o3r", ws, 1, Associativity.Right,  fun x y -> O3(x,y)))

    opp.AddOperator(InfixOperator("o1l*",  ws, 2, Associativity.Left,  fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o1r*",  ws, 2, Associativity.Right, fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o1n*",  ws, 2, Associativity.None,  fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o2l*",  ws, 2, Associativity.Left,  fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o2r*",  ws, 2, Associativity.Right, fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o2n*",  ws, 2, Associativity.None,  fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o3l*", ws, 2, Associativity.Left,   fun x y -> O3(x,y)))
    opp.AddOperator(InfixOperator("o3r*", ws, 2, Associativity.Right,  fun x y -> O3(x,y)))

    opp.AddOperator(InfixOperator("o1l**", ws, 3, Associativity.Left,   fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o2l**", ws, 3, Associativity.Left,   fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o3l**", ws, 3, Associativity.Left,   fun x y -> O3(x,y)))
    opp.AddOperator(InfixOperator("o1r**", ws, 3, Associativity.Right,  fun x y -> O1(x,y)))
    opp.AddOperator(InfixOperator("o2r**", ws, 3, Associativity.Right,  fun x y -> O2(x,y)))
    opp.AddOperator(InfixOperator("o3r**", ws, 3, Associativity.Right,  fun x y -> O3(x,y)))

    opp.AddOperator(TernaryOperator("t1l", ws, "tt1l", ws, 1, Associativity.Left,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOperator("t1r", ws, "tt1r", ws, 1, Associativity.Right, fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOperator("t1n", ws, "tt1n", ws, 1, Associativity.None,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOperator("t2l", ws, "tt2l", ws, 1, Associativity.Left,  fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOperator("t2r", ws, "tt2r", ws, 1, Associativity.Right, fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOperator("t2n", ws, "tt2n", ws, 1, Associativity.None,  fun x y z -> T2(x,y,z)))

    opp.AddOperator(TernaryOperator("t1l*",  ws, "tt1l*",  ws, 2, Associativity.Left,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOperator("t1l**", ws, "tt1l**", ws, 3, Associativity.Left,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOperator("t1r*",  ws, "tt1r*",  ws, 2, Associativity.Right, fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOperator("t1n*",  ws, "tt1n*",  ws, 2, Associativity.None,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOperator("t2l*",  ws, "tt2l*",  ws, 2, Associativity.Left,  fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOperator("t2r*",  ws, "tt2r*",  ws, 2, Associativity.Right, fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOperator("t2n*",  ws, "tt2n*",  ws, 2, Associativity.None,  fun x y z -> T2(x,y,z)))

    let poOp1 = PostfixOperator("po1",  ws, 1, true,  fun x -> Po1(x))
    opp.AddOperator(poOp1)
    opp.AddOperator(PostfixOperator("po1n", ws, 1, false, fun x -> Po1(x)))
    opp.AddOperator(PostfixOperator("po2",  ws, 1, true,  fun x -> Po2(x)))
    opp.AddOperator(PostfixOperator("po2n", ws, 1, false, fun x -> Po2(x)))

    opp.AddOperator(PostfixOperator("po1*",  ws, 2, true,  fun x -> Po1(x)))
    opp.AddOperator(PostfixOperator("po1n*", ws, 2, false, fun x -> Po1(x)))
    opp.AddOperator(PostfixOperator("po2*",  ws, 2, true,  fun x -> Po2(x)))
    opp.AddOperator(PostfixOperator("po2n*", ws, 2, false, fun x -> Po2(x)))

    // do some tests without prefix operators defined (there's a separate code branch in OPP.ParseExpression)
    let expectedInfixOrPostfix = Errors.ExpectedInfixOrPostfixOperator
    let ROk content result parser = ROkE content content.Length result expectedInfixOrPostfix parser

    expr |> RError "" 0 Errors.ExpectedInt32
    expr |> ROk "1 o1l  2 o2l  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r* 2 o2r  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r  2 o2r  3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> ROk "1 o1l  2 o2l* 3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> ROk "1 o1n 2 po1n"    (O1(Val(1), Po1(Val(2))))

    // add prefix operators

    opp.AddOperator(PrefixOperator("pre1",  ws, 1, true,  fun x -> Pre1(x)))

    expr |> RError "po1" 0 (ErrorMessageList.Merge(Errors.ExpectedPrefixOperator, Errors.ExpectedInt32))

    opp.AddOperator(PrefixOperator("pre1n", ws, 1, false, fun x -> Pre1(x)))
    opp.AddOperator(PrefixOperator("pre2",  ws, 1, true,  fun x -> Pre2(x)))
    opp.AddOperator(PrefixOperator("pre2n", ws, 1, false, fun x -> Pre2(x)))

    opp.AddOperator(PrefixOperator("pre1*",  ws, 2, true,  fun x -> Pre1(x)))
    opp.AddOperator(PrefixOperator("pre1n*", ws, 2, false, fun x -> Pre1(x)))
    opp.AddOperator(PrefixOperator("pre2*",  ws, 2, true,  fun x -> Pre2(x)))
    opp.AddOperator(PrefixOperator("pre2n*", ws, 2, false, fun x -> Pre2(x)))

    // add operators a second time with opposite fixity

    opp.AddOperator(PrefixOperator("o1l", ws, 1, true, fun x -> failwith "o1l"))
    opp.AddOperator(PrefixOperator("o1r", ws, 1, true, fun x -> failwith "o1r"))
    opp.AddOperator(PrefixOperator("o1n", ws, 1, true, fun x -> failwith "o1n"))
    opp.AddOperator(PrefixOperator("o2l", ws, 1, true, fun x -> failwith "o2l"))
    opp.AddOperator(PrefixOperator("o2r", ws, 1, true, fun x -> failwith "o2r"))
    opp.AddOperator(PrefixOperator("o2n", ws, 1, true, fun x -> failwith "o2n"))

    opp.AddOperator(PrefixOperator("o1l*",  ws, 2, true, fun x -> failwith "o1l*"))
    opp.AddOperator(PrefixOperator("o1l**", ws, 3, true, fun x -> failwith "o1l**"))
    opp.AddOperator(PrefixOperator("o1r*", ws, 2, true, fun x -> failwith "o1r*"))
    opp.AddOperator(PrefixOperator("o1n*", ws, 2, true, fun x -> failwith "o1n*"))
    opp.AddOperator(PrefixOperator("o2l*", ws, 2, true, fun x -> failwith "o2l*"))
    opp.AddOperator(PrefixOperator("o2r*", ws, 2, true, fun x -> failwith "o2r*"))
    opp.AddOperator(PrefixOperator("o2n*", ws, 2, true, fun x -> failwith "o2n*"))

    opp.AddOperator(PrefixOperator("t1l", ws, 1, true, fun x -> failwith "t1l"))
    opp.AddOperator(PrefixOperator("t1r", ws, 1, true, fun x -> failwith "t1r"))
    opp.AddOperator(PrefixOperator("t1n", ws, 1, true, fun x -> failwith "t1n"))
    opp.AddOperator(PrefixOperator("t2l", ws, 1, true, fun x -> failwith "t2l"))
    opp.AddOperator(PrefixOperator("t2r", ws, 1, true, fun x -> failwith "t2r"))
    opp.AddOperator(PrefixOperator("t2n", ws, 1, true, fun x -> failwith "t2n"))

    opp.AddOperator(PrefixOperator("t1l*",  ws, 2, true, fun x -> failwith "t1l*"))
    opp.AddOperator(PrefixOperator("t1l**", ws, 3, true, fun x -> failwith "t1l**"))
    opp.AddOperator(PrefixOperator("t1r*", ws, 2, true, fun x -> failwith "t1r*"))
    opp.AddOperator(PrefixOperator("t1n*", ws, 2, true, fun x -> failwith "t1n*"))
    opp.AddOperator(PrefixOperator("t2l*", ws, 2, true, fun x -> failwith "t2l*"))
    opp.AddOperator(PrefixOperator("t2r*", ws, 2, true, fun x -> failwith "t2r*"))
    opp.AddOperator(PrefixOperator("t2n*", ws, 2, true, fun x -> failwith "t2n*"))

    opp.AddOperator(PrefixOperator("po1",  ws, 1, true, fun x -> failwith "po1"))
    opp.AddOperator(PrefixOperator("po1n", ws, 1, true, fun x -> failwith "po1n"))
    opp.AddOperator(PrefixOperator("po2",  ws, 1, true, fun x -> failwith "po2"))
    opp.AddOperator(PrefixOperator("po2n", ws, 1, true, fun x -> failwith "po2n"))

    opp.AddOperator(PrefixOperator("po1*",  ws, 2, true, fun x -> failwith "po1*"))
    opp.AddOperator(PrefixOperator("po1n*", ws, 2, true, fun x -> failwith "po1n*"))
    opp.AddOperator(PrefixOperator("po2*",  ws, 2, true, fun x -> failwith "po2*"))
    opp.AddOperator(PrefixOperator("po2n*", ws, 2, true, fun x -> failwith "po2n*"))

    opp.AddOperator(InfixOperator("pre1",  ws, 1, Associativity.Left, fun x y -> failwith "pre1"))
    opp.AddOperator(InfixOperator("pre1n", ws, 1, Associativity.Left, fun x y -> failwith "pre1n"))
    opp.AddOperator(PostfixOperator("pre2",  ws, 1, true, fun x -> failwith "pre2"))
    opp.AddOperator(PostfixOperator("pre2n", ws, 1, true, fun x -> failwith "pre2n"))

    opp.AddOperator(InfixOperator("pre1*",  ws, 2, Associativity.Left, fun x y -> failwith "pre1*"))
    opp.AddOperator(InfixOperator("pre1n*", ws, 2, Associativity.Left, fun x y -> failwith "pre1n*"))
    opp.AddOperator(PostfixOperator("pre2*",  ws, 2, true, fun x -> failwith "pre2*"))
    opp.AddOperator(PostfixOperator("pre2n*", ws, 2, true, fun x -> failwith "pre2n"))

    expr |> ROk "1 o1l  2 o2l  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r* 2 o2r  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r  2 o2r  3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> ROk "1 o1l  2 o2l* 3" (O1(Val(1),O2(Val(2),Val(3))))

    expr |> ROk "1 o1l  2 o2l*  3 o3l** 4" (O1(Val(1),O2(Val(2),O3(Val(3),Val(4)))))
    expr |> ROk "1 o1l  2 o2l*  3 o3l   4" (O3((O1(Val(1),O2(Val(2),Val(3)))),Val(4)))
    expr |> ROk "1 o1l  2 o2l** 3 o3l*  4" (O1(Val(1),O3(O2(Val(2),Val(3)),Val(4))))

    expr |> ROk "1 o1r  2 o2r*  3 o3r** 4" (O1(Val(1),O2(Val(2),O3(Val(3),Val(4)))))
    expr |> ROk "1 o1r  2 o2r*  3 o3r   4" (O1(Val(1),O3(O2(Val(2),Val(3)),Val(4))))

    expr |> ROk "1 t1l  2 tt1l  3 t2l 4 tt2l 5"   (T2(T1(Val(1),Val(2),Val(3)),Val(4),Val(5)))
    expr |> ROk "1 t1r* 2 tt1r* 3 t2r 4 tt2r 5"   (T2(T1(Val(1),Val(2),Val(3)),Val(4),Val(5)))
    expr |> ROk "1 t1r  2 tt1r  3 t2r 4 tt2r 5"   (T1(Val(1),Val(2),T2(Val(3),Val(4),Val(5))))
    expr |> ROk "1 t1l  2 tt1l  3 t2l* 4 tt2l* 5" (T1(Val(1),Val(2),T2(Val(3),Val(4),Val(5))))

    expr |> ROk "1 t1l* 2 po1 tt1l* 3 t2l* 4 o1l 5 tt2l* 6" (T2(T1(Val(1),Po1(Val(2)),Val(3)),O1(Val(4),Val(5)),Val(6)))
    expr |> ROk "1 t1n 2 o1n 3 tt1n 4" (T1(Val(1),O1(Val(2),Val(3)),Val(4)))

    expr |> ROk "pre1  1 o1l 2"  (O1(Pre1(Val(1)), Val(2)))
    expr |> ROk "pre1* 1 o1l 2"  (O1(Pre1(Val(1)), Val(2)))
    expr |> ROk "pre1  1 o1l* 2" (Pre1(O1(Val(1), Val(2))))

    expr |> ROk "1 o1l pre1 2"  (O1(Val(1), Pre1(Val(2))))
    expr |> ROk "1 o1l pre1* 2" (O1(Val(1), Pre1(Val(2))))
    expr |> ROk "1 o1l* pre1 2" (O1(Val(1), Pre1(Val(2))))

    expr |> ROk "1 o1l 2 po1"  (O1(Val(1), Po1(Val(2))))
    expr |> ROk "1 o1l 2 po1*" (O1(Val(1), Po1(Val(2))))
    expr |> ROk "1 o1l* 2 po1" (Po1(O1(Val(1), Val(2))))

    expr |> ROk "1 o1l  pre1  2 po1"  (O1(Val(1), Po1(Pre1(Val(2)))))
    expr |> ROk "1 o1l  pre1* 2 po1"  (O1(Val(1), Po1(Pre1(Val(2)))))
    expr |> ROk "1 o1l  pre1  2 po1*" (O1(Val(1), Pre1(Po1(Val(2)))))
    expr |> ROk "1 o1l* pre1  2 po1"  (Po1(O1(Val(1), Pre1(Val(2)))))
    expr |> ROk "1 o1r* pre1  2 po1"  (Po1(O1(Val(1), Pre1(Val(2)))))
    expr |> ROk "1 o1l* pre1  2 po1*" (O1(Val(1), Pre1(Po1(Val(2)))))

    expr |> ROk "1 o1l 2 po1*" (O1(Val(1), Po1(Val(2))))
    expr |> ROk "1 o1l* 2 po1" (Po1(O1(Val(1), Val(2))))

    expr |> ROk "1 o1l 2 o2l* 3 po1" (O1(Val(1),Po1(O2(Val(2),Val(3)))))

    expr |> ROk "1 o1l pre1  pre2  2" (O1(Val(1), Pre1(Pre2(Val(2)))))
    expr |> ROk "1 o1l pre1* pre2  2" (O1(Val(1), Pre1(Pre2(Val(2)))))
    expr |> ROk "1 o1l pre1  pre2* 2" (O1(Val(1), Pre1(Pre2(Val(2)))))

    expr |> ROk "1 o1l  2 po1  po2"  (O1(Val(1), Po2(Po1(Val(2)))))
    expr |> ROk "1 o1l* 2 po1  po2"  (Po2(Po1(O1(Val(1), Val(2)))))
    expr |> ROk "1 o1l* 2 po1  po2*" (Po2(Po1(O1(Val(1), Val(2)))))
    expr |> ROk "1 o1l* 2 po1* po2"  (Po2(O1(Val(1), Po1(Val(2)))))

    expr |> ROk "1 o1l  pre1 2 po1  po2"  (O1(Val(1), Po2(Po1(Pre1(Val(2))))))
    expr |> ROk "1 o1l  pre1 2 po1* po2"  (O1(Val(1), Po2(Pre1(Po1(Val(2))))))
    expr |> ROk "1 o1l  pre1 2 po1* po2*" (O1(Val(1), Pre1(Po2(Po1(Val(2))))))
    expr |> ROk "1 o1l  pre1 2 po1  po2*" (O1(Val(1), Po2(Po1(Pre1(Val(2))))))
    expr |> ROk "1 o1l* pre1 2 po1  po2"  (Po2(Po1(O1(Val(1), Pre1(Val(2))))))
    expr |> ROk "1 o1l* pre1 2 po1* po2"  (Po2(O1(Val(1), Pre1(Po1(Val(2))))))
    expr |> ROk "1 o1l* pre1 2 po1* po2"  (Po2(O1(Val(1), Pre1(Po1(Val(2))))))

    expr |> ROk "pre1  1 o1l   2 po1"  (O1(Pre1(Val(1)), Po1(Val(2))))
    expr |> ROk "pre1  1 o1l*  2 po1"  (Po1(Pre1(O1(Val(1), Val(2)))))
    expr |> ROk "pre1* 1 o1l*  2 po1"  (Po1(O1(Pre1(Val(1)), Val(2))))
    expr |> ROk "pre1  1 o1l*  2 po1*" (Pre1(O1(Val(1), Po1(Val(2)))))
    expr |> ROk "pre1  1 o1l** 2 po1*" (Pre1(Po1(O1(Val(1), Val(2)))))

    opp.OperatorConflictErrorFormatter <- fun _ _ -> messageError "conflict"

    let conflictE = messageError "conflict"

    expr |> ROk "pre1n 1 o1n 2" (O1(Pre1(Val(1)), Val(2)))
    expr |> ROk "1 po1n o1n 2"  (O1(Po1(Val(1)), Val(2)))
    expr |> ROk "1 o1n pre1n 2" (O1(Val(1), Pre1(Val(2))))
    expr |> ROk "1 o1n 2 po1n"  (O1(Val(1), Po1(Val(2))))


    expr |> ROk "1 o1l* 2 o2r  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1l  2 o2r* 3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> RError "1 o1l  2 o2r 3" 9 conflictE
    expr |> RError "1 o1l  2 o2n 3" 9 conflictE
    expr |> RError "1 o1n  2 o2n 3" 9 conflictE
    expr |> RError "1 o1l  2 o2l* 3 o2r 4" 16 conflictE
    expr |> RError "1 o1l  2 o2l* 3 o2n 4" 16 conflictE
    expr |> RError "1 o1n  2 o2l* 3 o2n 4" 16 conflictE

    expr |> ROk "1 t1l* 2 tt1l* 3 t2r  4 tt2r  5" (T2(T1(Val(1),Val(2),Val(3)),Val(4),Val(5)))
    expr |> ROk "1 t1l  2 tt1l  3 t2r* 4 tt2r* 5" (T1(Val(1),Val(2),T2(Val(3),Val(4),Val(5))))
    expr |> RError "1 t1l  2 tt1l  3 t2r  4 tt2r  5" 17 conflictE

    expr |> RError "1 t1l  2 tt1l  3 o1r  4" 17 conflictE
    expr |> RError "1 o1r  2 t1l   3 tt1l 4" 9 conflictE

    expr |> ROk    "pre1n  1 po1"   (Po1(Pre1(Val(1))))
    expr |> ROk    "pre1   1 po1n"  (Po1(Pre1(Val(1))))
    expr |> ROk    "pre1n* 1 po1n"  (Po1(Pre1(Val(1))))
    expr |> ROk    "pre1n  1 po1n*" (Pre1(Po1(Val(1))))
    expr |> RError "pre1n  1 po1n" 9 conflictE

    expr |> ROk    "pre1   pre2n  1" (Pre1(Pre2(Val(1))))
    expr |> ROk    "pre1n  pre2   1" (Pre1(Pre2(Val(1))))
    expr |> ROk    "pre1n* pre2n  1" (Pre1(Pre2(Val(1))))
    expr |> ROk    "pre1n  pre2n* 1" (Pre1(Pre2(Val(1))))
    expr |> RError "pre1n  pre2n  1" 7 conflictE

    expr |> ROk    "1 po1    po2n"  (Po2(Po1(Val(1))))
    expr |> ROk    "1 po1n   po2"   (Po2(Po1(Val(1))))
    expr |> ROk    "1 po1n*  po2n"  (Po2(Po1(Val(1))))
    expr |> ROk    "1 po1n   po2n*" (Po2(Po1(Val(1))))
    expr |> RError "1 po1n   po2n" 9 conflictE

    let rand = new System.Random(1234)
    testRemoveSeq rand opp opp.Operators

let testExceptions() =
    let opp = new OperatorPrecedenceParser<int,unit,unit>()
    opp.AddOperator(TernaryOperator("?", spaces, ":", spaces, 1, Associativity.Left, fun _ _ _ -> 0))
    try opp.AddOperator(TernaryOperator("??", spaces, ":", spaces, 1, Associativity.Left, fun _ _ _ -> 0))
    with :? System.ArgumentException -> ()
    try opp.AddOperator(PrefixOperator<int,unit,unit>(":", spaces, 2, false, fun x -> 0))
    with :? System.ArgumentException -> ()

    opp.AddOperator(PrefixOperator<int,unit,unit>("+", spaces, 1, true, fun x -> 0))
    opp.AddOperator(InfixOperator<int,unit,unit>("-", spaces, 1, Associativity.Left, fun x y -> 0))
    try opp.AddOperator(PrefixOperator<int,unit,unit>(":", spaces, 2, false, fun x -> 0))
    with :? System.ArgumentException -> ()
    try opp.AddOperator(TernaryOperator("x", spaces, "+", spaces, 1, Associativity.Left, fun _ _ _ -> 0))
    with :? System.ArgumentException -> ()
    try opp.AddOperator(TernaryOperator("x", spaces, "-", spaces, 1, Associativity.Left, fun _ _ _ -> 0))
    with :? System.ArgumentException -> ()

    try PrefixOperator<int,unit,unit>(null, spaces, 1, true, fun x -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try PrefixOperator<int,unit,unit>("", spaces, 1, true, fun x -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try PrefixOperator<int,unit,unit>("+", Unchecked.defaultof<_>, 1, true, fun x -> x) |> ignore; Fail()
    with :? System.ArgumentNullException -> ()
    try PrefixOperator<int,unit,unit>("+", spaces, 0, true, fun x -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()

    (PrefixOperator<int,unit,unit>("+", spaces, 1, true, fun x -> 0)).IsAssociative |> True
    try InfixOperator<int,unit,unit>("+", spaces, 1, enum -1 , fun x y -> x + y) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try InfixOperator<int,unit,unit>("+", spaces, 1, enum 3 , fun x y -> x + y) |> ignore; Fail()
    with :? System.ArgumentException -> ()

    try PrefixOperator<int,unit,unit>("+", spaces, 1, true, (), Unchecked.defaultof<_>) |> ignore; Fail()
    with :? System.ArgumentNullException -> ()
    try InfixOperator<int,unit,unit>("+", spaces, 1, Associativity.Left, (), Unchecked.defaultof<_>) |> ignore; Fail()
    with :? System.ArgumentNullException -> ()

    try TernaryOperator<int,unit,unit>(null, spaces, "2", spaces, 1, Associativity.Left, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("", spaces, "2", spaces, 1, Associativity.Left, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", spaces, null, spaces, 1, Associativity.Left, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", spaces, "", spaces, 1, Associativity.Left, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", Unchecked.defaultof<_>, "2", spaces, 1, Associativity.Left, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", spaces, "2", Unchecked.defaultof<_>, 1, Associativity.Left, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", spaces, "2", spaces, 0, Associativity.Left, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", spaces, "2", spaces, 1, enum -1, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", spaces, "2", spaces, 1, enum 3, fun x y z -> x) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try TernaryOperator<int,unit,unit>("1", spaces, "2", spaces, 1, Associativity.Left, (), Unchecked.defaultof<_>) |> ignore; Fail()
    with :? System.ArgumentException -> ()

let run() =
    testOpParser()
    testConflictAfterStringParserHandling()
    testAlternativeOpConstructors()
    testPrecAndAssoc()
    testExceptions()
