// Copyright (c) Stephan Tolksdorf 2007-2008
// License: Simplified BSD License. See accompanying documentation.

// the parser definition
////////////////////////

open FParsec.Primitives
open FParsec.CharParsers
open FParsec.OperatorPrecedenceParser

let ws = spaces // skips any whitespace

let ch c = skipChar c >>. ws

// we calculate with double precision floats
let number = pfloat .>> ws

// we set up an operator precedence parser for parsing the arithmetic expressions
let opp = new OperatorPrecedenceParser<_,_>()
let expr = opp.ExpressionParser
opp.TermParser <- number <|> between (ch '(') (ch ')') expr

// operator definitions follow the schema
// operator type, string, trailing whitespace parser, precedence, associativity, function to apply

opp.AddOperator(InfixOp("+", ws, 1, Assoc.Left, fun x y -> x + y))
opp.AddOperator(InfixOp("-", ws, 1, Assoc.Left, fun x y -> x - y))

opp.AddOperator(InfixOp("*", ws, 2, Assoc.Left, fun x y -> x * y))
opp.AddOperator(InfixOp("/", ws, 2, Assoc.Left, fun x y -> x / y))

opp.AddOperator(InfixOp("^", ws, 3, Assoc.Right, fun x y -> System.Math.Pow(x, y)))

opp.AddOperator(PrefixOp("-", ws, 4, true, fun x -> -x))

// we also want to accept the operators "exp" and "log", but we don't want to accept
// expressions like "logexp" 2, so we require that non-symbolic operators are not
// followed by letters

let ws1 = notFollowedBy letter >>. ws
opp.AddOperator(PrefixOp("log", ws1, 4, true, fun x -> System.Math.Log(x)))
opp.AddOperator(PrefixOp("exp", ws1, 4, true, fun x -> System.Math.Exp(x)))

let completeExpression = ws >>. expr .>> eof // we append the eof parser to make
                                            // sure all input is consumed

// running and testing the parser
////////////////////////////////

let calculate s = run completeExpression s

let equals expectedValue r =
    match r with
    | Success (v, _, _) when v = expectedValue -> ()
    | Success (v, _, _)     -> failwith "Math is hard, let's go shopping!"
    | Failure (msg, err, _) -> printf "%s" msg; failwith msg

let test() =
    calculate "10.5 + 123.25 + 877"  |> equals 1010.75
    calculate "10/2 + 123.125 + 877" |> equals 1005.125
    calculate "(123 + log 1 + 877) * 9/3" |> equals 3000.
    calculate " ( ( exp 0 + (6 / ( 1 +2 ) )- 123456 )/ 2+123 + 877) * 3^2 / 3" |> equals (-182179.5)
    printfn "No errors"

// currently the program only executes some tests
do test()

