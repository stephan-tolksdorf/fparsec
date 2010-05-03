// Copyright (c) Stephan Tolksdorf 2007-2010
// License: Simplified BSD License. See accompanying documentation.

let run() =
    printfn "Testing FParsec.Cloning ..."
    FParsec.Test.CloningTests.run()
    printfn "Testing FParsec.CharStream ..."
    FParsec.Test.CharStreamTests.run()
    printfn "Testing FParsec.State<_> ..."
    FParsec.Test.StateTests.run()
    printfn "Testing FParsec.Primitives ..."
    FParsec.Test.PrimitivesTests.run()
    printfn "Testing FParsec.CharParsers ..."
    FParsec.Test.CharParsersTests.run()
    FParsec.Test.HexLiteralHelpersTests.run()
    printfn "Testing FParsec.OperatorPrecedenceParser ..."
    FParsec.Test.OperatorPrecedenceParserTests.run()
    printfn "No errors."
run()
