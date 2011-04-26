// Copyright (c) Stephan Tolksdorf 2008
// License: Simplified BSD License. See accompanying documentation.

// This is a port of the parsing sample that comes with F# 1.9.4.19
// distribution (see http://research.microsoft.com/fsharp/release.aspx).

// This project requires the F# PowerPack available at
// http://fsharppowerpack.codeplex.com/


open FParsec

open Ast

[<EntryPoint>]
let main(argv: string[]) =
    if argv.Length <> 1 then
        printf "usage: interp.exe <file>\n"
        exit 1

    // Run the parser prog on the file path in argv.[0]
    // If the file has no byte order marks, System.Text.Encoding.Default
    // is assumed to be the encoding.
    let result = runParserOnFile Parser.prog () (argv.[0]) System.Text.Encoding.Default
    let myProg =
        match result with
        | Success (v, _, _) -> v
        | Failure (msg, _, _) ->
            System.Console.WriteLine(msg)
            exit 1

    // count statements
    printf "#stmts = %d\n" (List.length (match myProg with Prog l -> l));
    printf "running program...\n";
    Interp.prog myProg
    0

