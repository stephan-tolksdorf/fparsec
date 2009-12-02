// Copyright (c) Stephan Tolksdorf 2008
// License: Simplified BSD License. See accompanying documentation.

module Parser

open FParsec.Primitives
open FParsec.CharParsers

open Ast

// This is a general JSON parser that will parse any JSON file into an AST.
// See e.g. http://www.json.org/, for a specification of JSON.

// Note that in typical applications you often don't need to parse any general
// JSON file, but only files describing objects of a certain type. In those cases
// it might be more convenient to parse the input with specialized parsers
// instead of using the indirect approach via an intermediate AST. The parser
// definitions below should be useful in any case.

// some abbreviations
let ws  = spaces // eats any whitespace
let chr c = skipChar c
let ch c  = skipChar c >>. ws // c followed by whitespace
let str s = skipString s >>. ws

let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> '\b'
                      | 'f' -> '\u000C'
                      | 'n' -> '\n'
                      | 'r' -> '\r'
                      | 't' -> '\t'
                      | c   -> c // every other char is mapped to itself

    let unicodeEscape =
        chr 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                        let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
                        (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                        |> char
                    )

    between (chr '"') (chr '"' >>. ws)
            (manyChars (    noneOf "\"\\"
                        <|> (chr '\\' >>. (escape <|> unicodeEscape))))

let jstring = stringLiteral |>> JString

let jnumber = pfloat |>> JNumber // pfloat will accept a little more than specified by JSON
                                 // as valid numbers (such as NaN or Infinity), but that makes
                                 // it only more robust

// jvalue, jlist and jobject are three mutually recursive grammar productions.
// In order to break the cyclic dependency, we make jvalue a parser that
// forwards all calls to a parser in a reference cell.
let jvalue, jvalueRef = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

let jlist = between (ch '[') (ch ']')
                    (sepBy jvalue (ch ',') |>> JList)

let jobject =
    let keyValue = tuple2 stringLiteral (ch ':' >>. jvalue)
    between (ch '{') (ch '}')
            // we read the object as a list of key-value pairs and then turn it into a Map
            (sepBy keyValue (ch ',') |>> (Map.ofList >> JObject))


do jvalueRef := choice [jobject;
                        jlist;
                        jstring;
                        jnumber;
                        str "true"  >>% JBool true;
                        str "false" >>% JBool false;
                        str "null"  >>% JNull
                       ]

let json = ws >>. jvalue .>> eof


let parseJsonString str = run json str

// UTF8 is the default, but it will detect UTF16 or UTF32 byte-order marks automatically
let parseJsonFile fileName encoding =
    runParserOnFile json () fileName System.Text.Encoding.UTF8

let parseJsonStream stream encoding =
    runParserOnStream json () "" stream System.Text.Encoding.UTF8