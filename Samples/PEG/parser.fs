// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

module Parser

open System
open FParsec

open Ast

// The following is a close translation of the grammar on page 2 of
// Parsing Expression Grammars: A Recognition-Based Syntactic Foundation, Bryan Ford.
// 31st ACM Symposium on Principles of Programming Languages, January 14-16, 2004, Venice, Italy.
// http://www.bford.info/pub/lang/peg.pdf

// If you're new to FParsec, take a look at
// http://www.quanttec.com/fparsec/reference/parser-overview.html

// some abbreviations
let str s = pstring s

// Lexical syntax

let pEndOfFile = eof
//let pEndOfLine = skipNewline
//let pSpace     = skipAnyOf " \t\n"

let pComment   = str "#" >>. skipRestOfLine true

let pSpacing   = // literal translation:
                 //  skipManyChars (pSpace <|> pComment)
                 // more efficient:
                     skipSepBy spaces pComment

let LEFTARROW  = str "<-" >>. pSpacing
let SLASH      = str "/" >>. pSpacing
let AND        = str "&" >>. pSpacing
let NOT        = str "!" >>. pSpacing
//let QUESTION  = str "?" .>> pSpacing
//let STAR      = str "*" .>> pSpacing
//let PLUS      = str "+" .>> pSpacing
let OPEN       = str "(" >>. pSpacing
let CLOSE      = str ")" >>. pSpacing
let DOT        = str "." >>. pSpacing

// Instead of the odd octal escapes in the original grammar,
// we accept the usual UTF16 character escapes '\uxxxx'
let pChar =
    let escape = anyOf "nrt'\"[]\\" |>> function
                                        | 'n' -> '\n'
                                        | 'r' -> '\r'
                                        | 't' -> '\t'
                                        | c   -> c

    let unicodeEscape =
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                        let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
                        (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                        |> char
                   )

    satisfy ((<>) '\\') <|> (str "\\" >>. (escape <|> unicodeEscape))


let pRange =
    pipe2 pChar (opt (str "-" >>. pChar))
          (fun c1 c2Opt ->
              match c2Opt with
              | None     -> Char c1
              | Some(c2) -> Range(c1, c2))


let pClass   = str "[" >>. (manyTill pRange (str "]") .>> pSpacing |>> Class)

let pLiteralString = (    (str "\'" >>. (manyCharsTill pChar (str "\'")))
                      <|> (str "\"" >>. (manyCharsTill pChar (str "\"" )))) .>> pSpacing
let pLiteral = pLiteralString |>> Literal


let isIdentifierStart = fun c -> isAsciiLetter c || c = '_' // "A-Za-z_"
let isIdentifierCont  = fun c -> isAsciiLetter c || isDigit c || c = '_' // A-Za-z_0-9
let pIdentifierString = many1Satisfy2 isIdentifierStart isIdentifierCont .>> pSpacing
let pIdentifier = pIdentifierString |>> Identifier

let pDot = DOT >>% Dot


// Hierarchical syntax

// expression, sequence, prefix, suffix and primary are mutually recursive
// grammar productions. In order to break the cyclic dependency, we make
// pPrimary a parser that forwards all calls to a parser in a reference cell.
let pPrimary, pPrimaryRef = createParserForwardedToRef() // initially pPrimary holds a reference to a dummy parser

let pSuffix =
                   // returns 'x' if there is no '?', '*' or '+'
    pipe2 pPrimary (anyOf "?*+" <|>% 'x')
          (fun p c ->
               match c with
               | '?' -> Opt p
               | '*' -> Star p
               | '+' -> Plus p
               | _   -> p)


let pPrefix = choice [AND >>. (pSuffix |>> And)
                      NOT >>. (pSuffix |>> Not)
                      pSuffix]
              .>> pSpacing

let pSequence = many pPrefix |>> function
                                 | [exp] -> exp
                                 | exps  -> Seq exps

let pExpression = sepBy1 pSequence SLASH |>> function
                                             | [exp] -> exp
                                             | exps  -> Alt exps // only use an Alt for more than one alternative

pPrimaryRef:= choice [pIdentifier .>>? notFollowedByString "<-" // backtracks to the beginning if the id is followed by "<-"
                      between OPEN CLOSE pExpression
                      pLiteral
                      pClass
                      pDot]

let pDefinition = pipe2 pIdentifierString (LEFTARROW >>. pExpression)
                        (fun s e -> Def (s, e))

let pGrammar: Parser<_, unit> =  // one type annotation is enough for the whole parser
    pSpacing >>. many1 pDefinition .>> pEndOfFile
