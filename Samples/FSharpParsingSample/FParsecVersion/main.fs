// Copyright (c) Stephan Tolksdorf 2008
// License: Simplified BSD License. See accompanying documentation.

// This is a port of the parsing sample that came with the F# 1.9.4.19
// distribution.

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
    let fileName = argv.[0]
    let result =
    #if PCL_FPARSEC
        runParserOnString Parser.prog () fileName (System.IO.File.ReadAllText(fileName, System.Text.Encoding.UTF8))
    #else
        runParserOnFile Parser.prog () fileName System.Text.Encoding.Default
    #endif

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

