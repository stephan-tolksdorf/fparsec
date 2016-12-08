// Copyright (c) Stephan Tolksdorf 2011
// License: Simplified BSD License. See accompanying documentation.

// Source code for the tutorial in the documentation

// 2 Parsing a single float

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"
test pfloat "1.25E 2"

// 3 Parsing a float between brackets

let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

test floatBetweenBrackets "[1.0]"
test floatBetweenBrackets "[]"
test floatBetweenBrackets "[1.0]"

// 4 Abstracting parsers

let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
let floatBetweenBrackets_ = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets_ = pfloat |> betweenStrings "[[" "]]"

test floatBetweenBrackets_ "[1.0]"
test floatBetweenDoubleBrackets_ "[[1.0]]"

let between_ pBegin pEnd p  = pBegin >>. p .>> pEnd
let betweenStrings_ s1 s2 p = p |> between_ (str s1) (str s2)

// 5 Parsing a list of floats

test (many floatBetweenBrackets) ""
test (many floatBetweenBrackets) "[1.0]"
test (many floatBetweenBrackets) "[2][3][4]"
test (many floatBetweenBrackets) "[1][2.0E]"

test (many1 floatBetweenBrackets) "(1)"

test (many1 (floatBetweenBrackets <?> "float between brackets")) "(1)"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[]"
test floatList "[1.0]"
test floatList "[4,5,6]"

test floatList "[1.0,"

// 6 Handling whitespace

test floatBetweenBrackets "[1.0, 2.0]"

let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList @"[ 1 ,
                          2 ] "

test numberList @"[ 1,
                         2; 3]"

let numberListFile = ws >>. numberList .>> eof
test numberListFile " [1, 2, 3] [4]"

// 7 Parsing string data

test (many (str "a" <|> str "b")) "abba"

test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitepace

test identifier "_"
test identifier "_test1="
test identifier "1"

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

test stringLiteral "\"abc\""
test stringLiteral "\"abc\\\"def\\\\ghi\""
test stringLiteral "\"abc\\def\""

let stringLiteral2 =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (manyStrings (normalCharSnippet <|> escapedChar))

test stringLiteral2 "\"abc\""
test stringLiteral2 "\"abc\\\"def\\\\ghi\""
test stringLiteral2 "\"abc\\def\""

let stringLiteral3 =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

test stringLiteral3 "\"abc\""
test stringLiteral3 "\"abc\\\"def\\\\ghi\""
test stringLiteral3 "\"abc\\def\""

// 8 Sequentially applying parsers

let product = pipe2 float_ws (str_ws "*" >>. float_ws)
                    (fun x y -> x * y)

test product "3 * 5";;

type StringConstant = StringConstant of string * string

let stringConstant = pipe3 identifier (str_ws "=") stringLiteral
                           (fun id _ str -> StringConstant(id, str))

test stringConstant "myString = \"stringValue\""

test (float_ws .>>. (str_ws "," >>. float_ws)) "123, 456"

let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
    pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7)
          (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7)

// 9 Parsing alternatives

let boolean =     (stringReturn "true"  true)
              <|> (stringReturn "false" false)

test boolean "false"
test boolean "true"
test boolean "tru"

test ((ws >>. str "a") <|> (ws >>. str "b")) " b"

test (ws >>. (str "a" <|> str "b")) " b"
