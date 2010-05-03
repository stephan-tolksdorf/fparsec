// Copyright (c) Stephan Tolksdorf 2008-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.OperatorPrecedenceParserTests

open FParsec
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.OperatorPrecedenceParser

open FParsec.Test.Test

// the tests for this module are somewhat ad hoc and ugly...

type Expr2 = Op    of State<unit>*Expr2*Expr2
           | Pre   of State<unit>*Expr2
           | Post  of State<unit>*Expr2
           | Tern  of State<unit>*State<unit>*Expr2*Expr2*Expr2
           | Value of State<unit>*int

type Expr = O1 of Expr*Expr
          | O2 of Expr*Expr
          | Pre1 of Expr
          | Pre2 of Expr
          | Po1 of Expr
          | Po2 of Expr
          | Val of int
          | T1 of Expr*Expr*Expr
          | T2 of Expr*Expr*Expr

let ws = spaces
let ws1 = spaces1

let testRemove (opp: OperatorPrecedenceParser<_,_>) (op: PrecedenceParserOp<_,_>) =
    try opp.AddOperator(op); Fail()
    with :? System.ArgumentException -> ()
    opp.RemoveOperator(op) |> True
    opp.AddOperator(op)
    match op with
    | PrefixOp  (str,_,_,_,_)
    | PrefixOp'  (str,_,_,_,_) -> opp.RemovePrefixOp(str)  |> True
    | PostfixOp (str,_,_,_,_)
    | PostfixOp' (str,_,_,_,_) -> opp.RemovePostfixOp(str) |> True
    | InfixOp   (str,_,_,_,_)
    | InfixOp'   (str,_,_,_,_) -> opp.RemoveInfixOp(str)   |> True
    | TernaryOp (str,_,str2,_, _,_,_)
    | TernaryOp' (str,_,str2,_,_,_,_) -> opp.RemoveTernaryOp(str, str2) |> True

let testRemoveSeq rand opp ops =
    let ops = Seq.toArray ops
    shuffleArray rand ops
    for op in ops do
        testRemove opp op

let testOpParser() =
    let opp = new OperatorPrecedenceParser<_,_>()
    let expr = opp.ExpressionParser
    opp.TermParser <- preturn 0

    // check "greedy" op parsing, correct sorting and finding in internal op data structure
    opp.AddOperator(PrefixOp("\u0302", ws, 1, true, fun _ -> 0))

    opp.AddOperator(PrefixOp("\u0303", ws, 1, true, fun _ -> 1))
    opp.AddOperator(PrefixOp("\u0203", ws, 1, true, fun _ -> 2))
    opp.AddOperator(PrefixOp("\u0403", ws, 1, true, fun _ -> 3))
    opp.AddOperator(PrefixOp("\u0503", ws, 1, true, fun _ -> 4))
    opp.AddOperator(PrefixOp("\u0103", ws, 1, true, fun _ -> 5))

    opp.AddOperator(PrefixOp("\u0304", ws, 1, true, fun _ -> -1))

    opp.AddOperator(PrefixOp("\u0303\u0303", ws, 1, true, fun _ -> 6))
    opp.AddOperator(PrefixOp("\u0303\u0302", ws, 1, true, fun _ -> 7))
    opp.AddOperator(PrefixOp("\u0303\u0304", ws, 1, true, fun _ -> 8))

    opp.AddOperator(PrefixOp("\u0203\u0202", ws, 1, true, fun _ -> 9))
    opp.AddOperator(PrefixOp("\u0203\u0203", ws, 1, true, fun _ -> 10))
    opp.AddOperator(PrefixOp("\u0203\u0204", ws, 1, true, fun _ -> 11))

    opp.AddOperator(PrefixOp("\u0403\u0404", ws, 1, true, fun _ -> 12))
    opp.AddOperator(PrefixOp("\u0403\u0403", ws, 1, true, fun _ -> 13))
    opp.AddOperator(PrefixOp("\u0403\u0402", ws, 1, true, fun _ -> 14))

    opp.AddOperator(PrefixOp("\u0503\u0403", ws, 1, true, fun _ -> 15))
    opp.AddOperator(PrefixOp("\u0503\u0402", ws, 1, true, fun _ -> 16))
    opp.AddOperator(PrefixOp("\u0503\u0404", ws, 1, true, fun _ -> 17))

    opp.AddOperator(PrefixOp("\u0103\u0103\u0103\u0103", ws, 1, true, fun _ -> 18))
    opp.AddOperator(PrefixOp("\u0103\u0103\u0103", ws, 1, true, fun _ -> 19))
    opp.AddOperator(PrefixOp("\u0103\u0102\u0102", ws, 1, true, fun _ -> 20))
    opp.AddOperator(PrefixOp("\u0103\u0102", ws, 1, true, fun _ -> 21))
    opp.AddOperator(PrefixOp("\u0103\u0103", ws, 1, true, fun _ -> 22))
    opp.AddOperator(PrefixOp("\u0103\u0101", ws, 1, true, fun _ -> 23))

    opp.AddOperator(PrefixOp("\u0303\u0303\u0303", ws, 1, true, fun _ -> 24))
    opp.AddOperator(PrefixOp("\u0303\u0303\u0303\u0303", ws, 1, true, fun _ -> 25))
    opp.AddOperator(PrefixOp("\u0303\u0302\u0302", ws, 1, true, fun _ -> 26))

    opp.AddOperator(PrefixOp("\u0203\u0202\u0202\u0202", ws, 1, true, fun _ -> 27))
    opp.AddOperator(PrefixOp("\u0203\u0202\u0202", ws, 1, true, fun _ -> 28))
    opp.AddOperator(PrefixOp("\u0203\u0203\u0203", ws, 1, true, fun _ -> 29))

    opp.AddOperator(PrefixOp("\u0403\u0403\u0403", ws, 1, true, fun _ -> 30))
    opp.AddOperator(PrefixOp("\u0403\u0402\u0402", ws, 1, true, fun _ -> 31))
    opp.AddOperator(PrefixOp("\u0403\u0402\u0402\u402", ws, 1, true, fun _ -> 32))

    let expectedPrefix = expectedError "prefix operator"

    let ROk content result parser = ROkE content content.Length result expectedPrefix parser
    let ROkI content i result parser = ROkE content i result expectedPrefix parser

    expr |> ROk "\u0302" 0

    expr |> ROk "\u0303" 1

    expr |> ROk "\u0303" 1
    expr |> ROk "\u0203" 2
    expr |> ROk "\u0403" 3
    expr |> ROk "\u0503" 4
    expr |> ROk "\u0103" 5

    expr |> ROk "\u0304" -1

    expr |> ROk "\u0303\u0303" 6
    expr |> ROk "\u0303\u0302" 7
    expr |> ROk "\u0303\u0304" 8

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

    let testOpParser wsParser termParser =
        opp.AddOperator(PrefixOp("+", wsParser, 1, true, fun x -> x + 1))
        opp.TermParser <- termParser
        let expr2 = pipe2 (many (pchar '+' >>? wsParser <?> "prefix operator")) termParser
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

        opp.RemovePrefixOp("+") |> True

    let withMsg m p = p .>> (fail m <|>% ())

    testOpParser (ws |> withMsg "e1") (preturn 0 |> withMsg "e2")
    testOpParser (fail "e1")  (preturn 0 |> withMsg "e2" )
    testOpParser (ws |> withMsg "e1") (pint32 |> withMsg "e2")
    testOpParser (ws1 |> withMsg "e1") (preturn 0 |> withMsg "e2")
    testOpParser (ws1 |> withMsg "e1") (pint32 |> withMsg "e2")

    let testTernary2ndOpParser opWsParser =
        let wsm =  (ws |> withMsg "e1")

        let term = (pint32 |> withMsg "e2") .>> wsm
        opp.TermParser <- term

        opp.AddOperator(TernaryOp("?", opWsParser, ":", opWsParser, 1, Assoc.Left, fun x y z -> x + y + z))

        let expr2 =
            let expect label = fun state -> Reply(Ok, (), (expectedError label), state)
            let term = expect "prefix operator" >>. term
            let op1 = pstring "?" >>? opWsParser <?> "infix operator"
            let op2 = expect "infix operator" >>. (pstring ":" >>? opWsParser <?> "':'")
            pipe2 term (tuple2 (op1 >>. term) (op2 >>. term .>> expect "infix operator") <|>% (0,0))
                  (fun x (y,z) -> x + y + z)

        checkParserStr expr expr2 "1 ?"
        checkParserStr expr expr2 "1 ?"
        checkParserStr expr expr2 "1 ?: 3"
        checkParserStr expr expr2 "1 ? : 3"
        checkParserStr expr expr2 "1 ? 2"
        checkParserStr expr expr2 "1 ? 2: "
        checkParserStr expr expr2 "1 ? 2 "
        checkParserStr expr expr2 "1 ? 2 :"
        checkParserStr expr expr2 "1 ? 2 :3"
        checkParserStr expr expr2 "1 ? 2 : "
        checkParserStr expr expr2 "1 ? 2 : 3"

        opp.RemoveTernaryOp("?", ":") |> True

    testTernary2ndOpParser (ws |> withMsg "e")
    testTernary2ndOpParser (ws1 |> withMsg "e")

    let rand = new System.Random(1234)
    testRemoveSeq rand opp opp.Operators

let testAlternativeOpConstructors() =
    let opp = new OperatorPrecedenceParser<_,_>()
    let expr = opp.ExpressionParser
    let str = skipString
    let getState = fun state -> Reply<_,_>(state, state)

    let term = pipe2 getState (pint32 .>> ws) (fun state x -> Value(state, x))
    opp.TermParser <- term

    opp.AddOperator(PrefixOp'("-", ws, 1, true, fun state x -> Pre(state, x)))
    opp.AddOperator(PostfixOp'("++", ws, 1, true, fun state x -> Post(state, x)))
    opp.AddOperator(InfixOp'("*", ws, 1, Assoc.Left, fun state x y -> Op(state, x, y)))
    opp.AddOperator(TernaryOp'("?", ws, ":", ws, 1, Assoc.Left, fun state1 state2 x y z -> Tern(state1, state2, x, y, z)))

    let op = getState .>> many1SatisfyL (fun c -> match c with '*' | '+' | '-' | '?' | ':' -> true | _ -> false) "operator" .>> ws
    let expr2 =
        pipe3 (tuple4 term op term op) (tuple2 op term) (tuple2 op (tuple2 op term))
              (fun (v12, sMult, v3, sPlusPlus) (sQMark, v4) (sColon, (sMinus, v5)) ->
                  Tern(sQMark, sColon, Op(sMult, v12, Post(sPlusPlus, v3)), v4, Pre(sMinus, v5)))
        .>> (str "xx" <?> "infix or postfix operator" <|>% ())

    checkParserStr expr expr2 "12 * 3++ ? 4 : -5"

    let rand = new System.Random(546433)
    testRemoveSeq rand opp opp.Operators


let testPrecAndAssoc() =
    let opp = new OperatorPrecedenceParser<_,_>()
    let expr = opp.ExpressionParser

    opp.TermParser <- pint32 .>> ws |>> Val

    opp.AddOperator(InfixOp("o1l", ws, 1, Assoc.Left,  fun x y -> O1(x,y)))
    opp.AddOperator(InfixOp("o1r", ws, 1, Assoc.Right, fun x y -> O1(x,y)))
    opp.AddOperator(InfixOp("o1n", ws, 1, Assoc.None, fun x y -> O1(x,y)))
    opp.AddOperator(InfixOp("o2l", ws, 1, Assoc.Left,  fun x y -> O2(x,y)))
    opp.AddOperator(InfixOp("o2r", ws, 1, Assoc.Right, fun x y -> O2(x,y)))
    opp.AddOperator(InfixOp("o2n", ws, 1, Assoc.None, fun x y -> O2(x,y)))

    opp.AddOperator(InfixOp("o1l*",  ws, 2, Assoc.Left,  fun x y -> O1(x,y)))
    opp.AddOperator(InfixOp("o1l**", ws, 3, Assoc.Left,  fun x y -> O1(x,y)))
    opp.AddOperator(InfixOp("o1r*",  ws, 2, Assoc.Right, fun x y -> O1(x,y)))
    opp.AddOperator(InfixOp("o1n*",  ws, 2, Assoc.None, fun x y -> O1(x,y)))
    opp.AddOperator(InfixOp("o2l*",  ws, 2, Assoc.Left,  fun x y -> O2(x,y)))
    opp.AddOperator(InfixOp("o2r*",  ws, 2, Assoc.Right, fun x y -> O2(x,y)))
    opp.AddOperator(InfixOp("o2n*",  ws, 2, Assoc.None, fun x y -> O2(x,y)))

    opp.AddOperator(TernaryOp("t1l", ws, "tt1l", ws, 1, Assoc.Left,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOp("t1r", ws, "tt1r", ws, 1, Assoc.Right, fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOp("t1n", ws, "tt1n", ws, 1, Assoc.None,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOp("t2l", ws, "tt2l", ws, 1, Assoc.Left,  fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOp("t2r", ws, "tt2r", ws, 1, Assoc.Right, fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOp("t2n", ws, "tt2n", ws, 1, Assoc.None,  fun x y z -> T2(x,y,z)))

    opp.AddOperator(TernaryOp("t1l*",  ws, "tt1l*",  ws, 2, Assoc.Left, fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOp("t1l**", ws, "tt1l**", ws, 3, Assoc.Left, fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOp("t1r*",  ws, "tt1r*",  ws, 2, Assoc.Right, fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOp("t1n*",  ws, "tt1n*",  ws, 2, Assoc.None,  fun x y z -> T1(x,y,z)))
    opp.AddOperator(TernaryOp("t2l*",  ws, "tt2l*",  ws, 2, Assoc.Left,  fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOp("t2r*",  ws, "tt2r*",  ws, 2, Assoc.Right, fun x y z -> T2(x,y,z)))
    opp.AddOperator(TernaryOp("t2n*",  ws, "tt2n*",  ws, 2, Assoc.None,  fun x y z -> T2(x,y,z)))

    opp.AddOperator(PostfixOp("po1",  ws, 1, true,  fun x -> Po1(x)))
    opp.AddOperator(PostfixOp("po1n", ws, 1, false, fun x -> Po1(x)))
    opp.AddOperator(PostfixOp("po2",  ws, 1, true,  fun x -> Po2(x)))
    opp.AddOperator(PostfixOp("po2n", ws, 1, false, fun x -> Po2(x)))

    opp.AddOperator(PostfixOp("po1*",  ws, 2, true,  fun x -> Po1(x)))
    opp.AddOperator(PostfixOp("po1n*", ws, 2, false, fun x -> Po1(x)))
    opp.AddOperator(PostfixOp("po2*",  ws, 2, true,  fun x -> Po2(x)))
    opp.AddOperator(PostfixOp("po2n*", ws, 2, false, fun x -> Po2(x)))

    // do some tests without prefix operators defined (there's a separate code branch in OPP.ParseExpression)
    let expectedInfixOrPostfix = expectedError "infix or postfix operator"
    let ROk content result parser = ROkE content content.Length result expectedInfixOrPostfix parser

    expr |> RError "" 0 (expectedError "integer number (32-bit, signed)")
    expr |> ROk "1 o1l  2 o2l  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r* 2 o2r  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r  2 o2r  3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> ROk "1 o1l  2 o2l* 3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> ROk "1 o1n 2 po1n"    (O1(Val(1), Po1(Val(2))))

    // add prefix operators

    opp.AddOperator(PrefixOp("pre1",  ws, 1, true,  fun x -> Pre1(x)))

    expr |> RError "po1" 0 (unexpectedError "postfix operator 'po1' (precedence: 1)")

    opp.AddOperator(PrefixOp("pre1n", ws, 1, false, fun x -> Pre1(x)))
    opp.AddOperator(PrefixOp("pre2",  ws, 1, true,  fun x -> Pre2(x)))
    opp.AddOperator(PrefixOp("pre2n", ws, 1, false, fun x -> Pre2(x)))

    opp.AddOperator(PrefixOp("pre1*",  ws, 2, true,  fun x -> Pre1(x)))
    opp.AddOperator(PrefixOp("pre1n*", ws, 2, false, fun x -> Pre1(x)))
    opp.AddOperator(PrefixOp("pre2*",  ws, 2, true,  fun x -> Pre2(x)))
    opp.AddOperator(PrefixOp("pre2n*", ws, 2, false, fun x -> Pre2(x)))

    // add operators a second time with opposite fixity

    opp.AddOperator(PrefixOp("o1l", ws, 1, true, fun x -> failwith "o1l"))
    opp.AddOperator(PrefixOp("o1r", ws, 1, true, fun x -> failwith "o1r"))
    opp.AddOperator(PrefixOp("o1n", ws, 1, true, fun x -> failwith "o1n"))
    opp.AddOperator(PrefixOp("o2l", ws, 1, true, fun x -> failwith "o2l"))
    opp.AddOperator(PrefixOp("o2r", ws, 1, true, fun x -> failwith "o2r"))
    opp.AddOperator(PrefixOp("o2n", ws, 1, true, fun x -> failwith "o2n"))

    opp.AddOperator(PrefixOp("o1l*",  ws, 2, true, fun x -> failwith "o1l*"))
    opp.AddOperator(PrefixOp("o1l**", ws, 3, true, fun x -> failwith "o1l**"))
    opp.AddOperator(PrefixOp("o1r*", ws, 2, true, fun x -> failwith "o1r*"))
    opp.AddOperator(PrefixOp("o1n*", ws, 2, true, fun x -> failwith "o1n*"))
    opp.AddOperator(PrefixOp("o2l*", ws, 2, true, fun x -> failwith "o2l*"))
    opp.AddOperator(PrefixOp("o2r*", ws, 2, true, fun x -> failwith "o2r*"))
    opp.AddOperator(PrefixOp("o2n*", ws, 2, true, fun x -> failwith "o2n*"))

    opp.AddOperator(PrefixOp("t1l", ws, 1, true, fun x -> failwith "t1l"))
    opp.AddOperator(PrefixOp("t1r", ws, 1, true, fun x -> failwith "t1r"))
    opp.AddOperator(PrefixOp("t1n", ws, 1, true, fun x -> failwith "t1n"))
    opp.AddOperator(PrefixOp("t2l", ws, 1, true, fun x -> failwith "t2l"))
    opp.AddOperator(PrefixOp("t2r", ws, 1, true, fun x -> failwith "t2r"))
    opp.AddOperator(PrefixOp("t2n", ws, 1, true, fun x -> failwith "t2n"))

    opp.AddOperator(PrefixOp("t1l*",  ws, 2, true, fun x -> failwith "t1l*"))
    opp.AddOperator(PrefixOp("t1l**", ws, 3, true, fun x -> failwith "t1l**"))
    opp.AddOperator(PrefixOp("t1r*", ws, 2, true, fun x -> failwith "t1r*"))
    opp.AddOperator(PrefixOp("t1n*", ws, 2, true, fun x -> failwith "t1n*"))
    opp.AddOperator(PrefixOp("t2l*", ws, 2, true, fun x -> failwith "t2l*"))
    opp.AddOperator(PrefixOp("t2r*", ws, 2, true, fun x -> failwith "t2r*"))
    opp.AddOperator(PrefixOp("t2n*", ws, 2, true, fun x -> failwith "t2n*"))

    opp.AddOperator(PrefixOp("po1",  ws, 1, true, fun x -> failwith "po1"))
    opp.AddOperator(PrefixOp("po1n", ws, 1, true, fun x -> failwith "po1n"))
    opp.AddOperator(PrefixOp("po2",  ws, 1, true, fun x -> failwith "po2"))
    opp.AddOperator(PrefixOp("po2n", ws, 1, true, fun x -> failwith "po2n"))

    opp.AddOperator(PrefixOp("po1*",  ws, 2, true, fun x -> failwith "po1*"))
    opp.AddOperator(PrefixOp("po1n*", ws, 2, true, fun x -> failwith "po1n*"))
    opp.AddOperator(PrefixOp("po2*",  ws, 2, true, fun x -> failwith "po2*"))
    opp.AddOperator(PrefixOp("po2n*", ws, 2, true, fun x -> failwith "po2n*"))

    opp.AddOperator(InfixOp("pre1",  ws, 1, Assoc.Left, fun x y -> failwith "pre1"))
    opp.AddOperator(InfixOp("pre1n", ws, 1, Assoc.Left, fun x y -> failwith "pre1n"))
    opp.AddOperator(PostfixOp("pre2",  ws, 1, true, fun x -> failwith "pre2"))
    opp.AddOperator(PostfixOp("pre2n", ws, 1, true, fun x -> failwith "pre2n"))

    opp.AddOperator(InfixOp("pre1*",  ws, 2, Assoc.Left, fun x y -> failwith "pre1*"))
    opp.AddOperator(InfixOp("pre1n*", ws, 2, Assoc.Left, fun x y -> failwith "pre1n*"))
    opp.AddOperator(PostfixOp("pre2*",  ws, 2, true, fun x -> failwith "pre2*"))
    opp.AddOperator(PostfixOp("pre2n*", ws, 2, true, fun x -> failwith "pre2n"))


    expr |> ROk "1 o1l  2 o2l  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r* 2 o2r  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r  2 o2r  3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> ROk "1 o1l  2 o2l* 3" (O1(Val(1),O2(Val(2),Val(3))))

    expr |> ROk "1 t1l  2 tt1l  3 t2l 4 tt2l 5"   (T2(T1(Val(1),Val(2),Val(3)),Val(4),Val(5)))
    expr |> ROk "1 t1r* 2 tt1r* 3 t2r 4 tt2r 5"   (T2(T1(Val(1),Val(2),Val(3)),Val(4),Val(5)))
    expr |> ROk "1 t1r  2 tt1r  3 t2r 4 tt2r 5"   (T1(Val(1),Val(2),T2(Val(3),Val(4),Val(5))))
    expr |> ROk "1 t1l  2 tt1l  3 t2l* 4 tt2l* 5" (T1(Val(1),Val(2),T2(Val(3),Val(4),Val(5))))

    expr |> ROk "1 t1l* 2 po1 tt1l* 3 t2l* 4 o1l 5 tt2l* 6" (T2(T1(Val(1),Po1(Val(2)),Val(3)),O1(Val(4),Val(5)),Val(6)))
    expr |> ROk "1 t1n 2 o1n 3 tt1n 4" (T1(Val(1),O1(Val(2),Val(3)),Val(4)))

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

    opp.OperatorConflictHandler <- fun _ _ _ _ -> "conflict"

    let conflictE = messageError "conflict"

    expr |> ROk "pre1n 1 o1n 2" (O1(Pre1(Val(1)), Val(2)))
    expr |> ROk "1 po1n o1n 2"  (O1(Po1(Val(1)), Val(2)))
    expr |> ROk "1 o1n pre1n 2" (O1(Val(1), Pre1(Val(2))))
    expr |> ROk "1 o1n 2 po1n"  (O1(Val(1), Po1(Val(2))))


    expr |> ROk "1 o1l* 2 o2r  3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1l  2 o2r* 3" (O1(Val(1),O2(Val(2),Val(3))))
    expr |> RError "1 o1l  2 o2r  3" 9 conflictE
    expr |> RError "1 o1l  2 o2n  3" 9 conflictE
    expr |> RError "1 o1n  2 o2n  3" 9 conflictE

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

    opp.OperatorConflictHandler <- fun _ _ _ _ -> null

    expr |> ROk "1 o1l 2 o2r 3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1r 2 o2n 3" (O2(O1(Val(1),Val(2)),Val(3)))
    expr |> ROk "1 o1n 2 o2n 3" (O2(O1(Val(1),Val(2)),Val(3)))

    expr |> ROk "1 t1l 2 tt1l 3 t2r 4 tt2r 5" (T2(T1(Val(1),Val(2),Val(3)),Val(4),Val(5)))

    expr |> ROk "1 t1l  2 tt1l  3 o1r  4" (O1(T1(Val(1),Val(2),Val(3)), Val(4)))
    expr |> ROk "1 o1r  2 t1l   3 tt1l 4" (T1(O1(Val(1),Val(2)),Val(3), Val(4)))

    expr |> ROk "pre1n 1 po1n"  (Po1(Pre1(Val(1))))
    expr |> ROk "pre1n pre2n 1" (Pre1(Pre2(Val(1))))

    expr |> ROk "1 po1n po2n" (Po2(Po1(Val(1))))

    let rand = new System.Random(6975)
    testRemoveSeq rand opp opp.Operators

let run() =
    testOpParser()
    testAlternativeOpConstructors()
    testPrecAndAssoc()
