// Copyright (c) Stephan Tolksdorf 2007-2008
// License: Simplified BSD License. See accompanying documentation.

module Parser

open System
open FParsec.Primitives
open FParsec.CharParsers

open Ast

// the following is a translation of the grammar on page 2 of
// Parsing Expression Grammars: A Recognition-Based Syntactic Foundation, Bryan Ford.
// 31st ACM Symposium on Principles of Programming Languages, January 14-16, 2004, Venice, Italy.
// http://www.bford.info/pub/lang/peg.pdf

// In general PEGs can be translated almost literally to FParsec grammars:
// Like the `/` operator in PEGs FParsec's `<|>` operator is a prioritized
// choice operator. The only difference is that by default `<|>` only employs
// a one token look-ahead while the operator `/` in PEGs has no look-ahead
// restriction. For example, the PEG rule `ab/c` will try to match the
// alternative `c` if `b` failed to match after `a` succeeded. In contrast,
// the FParsec parser `(a >>. b) <|> c` will not try to apply `c` if `b`
// fails to parse after `a` consumed input. If you need more than
// one token look-ahead you need to make this explict by using the `attempt`
// combinator, as in `attempt (a >>. b) <|> c`. The different default
// sometimes makes FParsec a litte more verbose but has the advantage
// that potentially exponential behaviour is clearly signalled. Often you
// can eliminate the use of `attempt` by left factoring the grammar,
// though this can be a bit inconvenient to do manually (a good  PEG
// compiler will do this automatically).
// The FParsec counterparts to the PEG predicates `&` and `!` are the
// combinators `followedBy` and `notFollowedBy`. You can use the combinators
// exactly like the predicates.
// As the operator `*` and `+` in PEGs, the FParsec combinators `many` and
// `many1` are greedy. Below we often use the operator `manyChars`, which is
// an optimized variant of `many` for char parsers.

// In case you didn't yet have time to look into the FParsec documentation:

// `p |>> f` parses `p` and then applies the function `f` to the result.
// `pipe2 p1 p2 f` parses `p1` and `p2` in sequence and then applies `f` to the results.
// `p1 .>> p2` parses `p1` followed by `p2` and returns the result of `p1`.
// `p1 >>. p2` parses `p1` followed by `p2` and returns the result of `p2`.
// `p <|>$ x` parses `p` or, in case `p` fails, returns `x`.

// In Visual Studio you can just hover to the mouse over any of the
// FParsec parsers to get a documentation tooltip displayed.

// some abbreviations
let ch c = skipChar c
let str s = skipString s

// Lexical syntax

let pEndOfFile = eof
//let pEndOfLine = newline |>> ignore
let pSpace     = whitespace |>> ignore


let pComment   = ch '#' >>. skipRestOfLine

let pSpacing   = skipManyChars (pSpace <|> pComment)
                 // more efficient:
                 // spaces >>. skipMany (pComment >>. spaces)

let LEFTARROW  = str "<-" >>. pSpacing
let SLASH      = ch '/' >>. pSpacing
let AND        = ch '&' >>. pSpacing
let NOT        = ch '!' >>. pSpacing
//let QUESTION  = ch '?' .>> pSpacing
//let STAR      = ch '*' .>> pSpacing
//let PLUS      = ch '+' .>> pSpacing
let OPEN       = ch '(' >>. pSpacing
let CLOSE      = ch ')' >>. pSpacing
let DOT        = ch '.' >>. pSpacing

// Instead of the odd octal escapes in the original grammar,
// we accept the usual UTF16 character escapes '\uxxxx'
let pChar =
    let escape =  anyOf "nrt'\"[]\\" |>> function
                                         | 'n' -> '\n'
                                         | 'r' -> '\r'
                                         | 't' -> '\t'
                                         | c   -> c

    let unicodeEscape =
        ch 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                       let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
                       (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                       |> char
                   )

    satisfy ((<>) '\\') <|> (ch '\\' >>. (escape <|> unicodeEscape))


let pRange =
    pipe2 pChar (opt (ch '-' >>. pChar))
          (fun c1 c2Opt ->
              match c2Opt with
              | None     -> Char c1
              | Some(c2) -> Range(c1, c2))


let pClass   = ch '[' >>. (manyTill pRange (ch ']') .>> pSpacing |>> Class)

let pLiteralString = (    (ch '\'' >>. (manyCharsTill pChar (ch '\'')))
                      <|> (ch '"'  >>. (manyCharsTill pChar (ch '"' )))) .>> pSpacing
let pLiteral = pLiteralString |>> Literal


let isIdentifierStart = fun c -> isAsciiLetter c || c = '_' // "A-Za-z_"
let isIdentifierCont  = fun c -> isAsciiLetter c || isDigit c || c = '_' // A-Za-z_0-9
let pIdentifierString = many1Satisfy2 isIdentifierStart isIdentifierCont .>> pSpacing
let pIdentifier = pIdentifierString |>> Identifier

let pDot = DOT >>$ Dot


// Hierarchical syntax

// expression, sequence, prefix, suffix and primary are mutually recursive
// grammar productions. In order to break the cyclic dependency, we make
// pPrimary a parser that forwards all calls to a parser in a reference cell.
let pPrimary, pPrimaryRef = createParserForwardedToRef() // initially pPrimary holds a reference to a dummy parser

let pSuffix =
                   // returns 'x' if there is no '?', '*' or '+'
    pipe2 pPrimary (anyOf "?*+" <|>$ 'x')
          (fun p c ->
               match c with
               | '?' -> Opt p
               | '*' -> Star p
               | '+' -> Plus p
               | _   -> p)


let pPrefix = choice [AND >>. (pSuffix |>> And);
                      NOT >>. (pSuffix |>> Not);
                      pSuffix]
              .>> pSpacing

let pSequence = many pPrefix |>> function
                                 | [exp] -> exp
                                 | exps  -> Seq exps

let pExpression = sepBy1 pSequence SLASH |>> function
                                             | [exp] -> exp
                                             | exps  -> Alt exps // only use an Alt for more than one alternative

pPrimaryRef:= choice [pIdentifier .>>? notFollowedBy LEFTARROW; // backtracks to the beginning if the id is followed by "<-"
                      between OPEN CLOSE pExpression;
                      pLiteral;
                      pClass;
                      pDot]

let pDefinition = pipe2 pIdentifierString (LEFTARROW >>. pExpression)
                        (fun s e -> Def (s, e))

let pGrammar: Parser<_, unit> =  // one type annotation is enough for the whole parser
    pSpacing >>. many1 pDefinition .>> pEndOfFile