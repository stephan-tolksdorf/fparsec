
// Original code:
//     Copyright (c) Microsoft Corporation 2005-2006.
//     This sample code is provided "as is" without warranty of any kind.
//     We disclaim all warranties, either express or implied, including the
//     warranties of merchantability and fitness for a particular purpose.

// Modifications:
//    Copyright (c) Stephan Tolksdorf 2015.
//    License: Simplified BSD License. See accompanying documentation.

// This program uses FsLex and FsYacc:
// http://fsprojects.github.io/FsLexYacc/

open Ast
open Printf

open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main(argv: string[]) =

    if argv.Length <> 1 then
        printf "usage: interp.exe <file>\n"
        exit 1

    let stream = new System.IO.StreamReader(argv.[0], System.Text.Encoding.UTF8)
    let myProg =

        // Create the lexer, presenting the bytes to the lexer as ASCII regardless of the original
        // encoding of the stream (the lexer specification
        // is designed to consume ASCII)
        let lexbuf = LexBuffer<char>.FromTextReader(stream)

        // Call the parser
        try
            Parser.start Lex.token lexbuf
        with e ->
            let pos = lexbuf.EndPos
            printf "error near line %d, character %d\n%s\n" pos.Line pos.Column (e.ToString());
            exit 1

    // Now look at the resulting AST, e.g. count the number of top-level
    // statements, and then the overall number of nodes.
    printf "#stmts = %d\n" (List.length (match myProg with Prog l -> l));
    printf "running program...\n";
    Interp.prog myProg
    0




