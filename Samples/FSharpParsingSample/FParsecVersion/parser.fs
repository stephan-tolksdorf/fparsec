// Copyright (c) Stephan Tolksdorf 2008.
// License: Simplified BSD License. See accompanying documentation.

// Compare this parser implementation with the implementation in ../LexYaccVersion.

module Parser

open System

open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

open Ast

// some lexical definitions
///////////////////////////

let ws  = spaces // skips any whitespace

let ch  c = skipChar c >>. ws
let str s = skipString s >>. ws

// identifiers are strings of lower ascii chars that are not keywords
let id : Parser<string, unit> =
    let idStr = many1Satisfy isLower .>> ws // [a-z]+

    let keyWordSet =
        System.Collections.Generic.HashSet<_>(
            [|"while"; "begin"; "end"; "do"; "if"; "then"; "else"; "print"; "decr"|]
        )

    let expectedId = expectedError "id"

    fun state -> // we define our own "primitive" that checks that the parsed id is no keyword
        let reply = idStr state
        if reply.Status = Ok then
            let id = reply.Result
            if not (keyWordSet.Contains(id)) then
                Reply(reply.Result, reply.State)
            else
                Reply(Error, expectedId, state)
        else // reconstruct error
            Reply(reply.Status, reply.Error, reply.State)

let numberFormat =     NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent
let numberLit = numberLiteral numberFormat "number" .>> ws


// parsers for the original grammar productions
///////////////////////////////////////////////

let pval = id |>> Val

let number =
    numberLit
    |>> fun nl -> // an overflow will throw an exception, as in the original sample
            if nl.IsInteger then Int (int32 nl.String)
            else Float (float nl.String)

// expr and decr are mutually recursive grammar grammar productions.
// In order to break the cyclic dependency, we make expr a parser that
// forwards all calls to a parser in a reference cell.
let expr, exprRef = createParserForwardedToRef() // initially exprRef holds a reference to a dummy parser

let pdecr = str "decr" >>. ch '(' >>. expr .>> ch ')' |>> Decr

// replace dummy parser reference in exprRef
do exprRef:= choice [pval; pdecr; number] // we need to try pval first, so we don't 
                                          // accidentally try to parse an identifier
                                          // starting with "decr..." as a Decr statement
                                          // (this is a disadvantage of not having a tokenizer)

let stmt, stmtRef = createParserForwardedToRef()

let stmtList = sepBy1 stmt (ch ';')

let assign =
    pipe2 id (str ":=" >>. expr) (fun id e -> Assign(id, e))

let print = str "print" >>. expr |>> Print

let pwhile =
    pipe2 (str "while" >>. expr) (str "do" >>. stmt) (fun e s -> While(e, s))

let seq =
    str "begin" >>. stmtList .>> str "end" |>> Seq

let ifthen =
    pipe3 (str "if" >>. expr) (str "then" >>. stmt) (opt (str "else" >>. stmt))
          (fun e s1 optS2 ->
               match optS2 with
               | None    -> IfThen(e, s1)
               | Some s2 -> IfThenElse(e, s1, s2))

do stmtRef:= choice [assign; ifthen; pwhile; seq; print] // try assign first, so that an 
                                                         // identifier starting with a 
                                                         // keyword doesn't trigger an error

let prog =
    ws >>. stmtList .>> eof |>> Prog


