// Copyright (c) Stephan Tolksdorf 2007-2008
// License: Simplified BSD License. See accompanying documentation.

// This is a simple parser for PEG grammars.
// See parser.fs for more information.

open FParsec

open Ast

[<EntryPoint>]
let main(args: string[]) =
    if args.Length <> 1 then
        printf "usage: peg.exe <file>\n"
        exit 1

    // The parser is run on the file path in args[0].
    // If the file has no byte order marks, System.Text.Encoding.Default
    // is assumed to be the encoding.
    // The parser result will be the abstract syntax tree of the input file.
    let fileName = args[0]
    let result = runParserOnFile Parser.pGrammar () fileName System.Text.Encoding.UTF8
    // for the moment we just print out the AST
    match result with
    | Success (result=v) -> printf "The ast for the input file is:\n%A\n" v
    | Failure (message=msg) -> printf "%s\n" msg
    0
