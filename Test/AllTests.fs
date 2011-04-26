// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.


let run() =
    printfn "Testing FParsec.Buffer ..."
    FParsec.Test.BufferTests.run()
    printfn "Testing FParsec.CharSet ..."
    FParsec.Test.CharSetTests.run()
    printfn "Testing FParsec.HexFloat ..."
    FParsec.Test.HexFloatTests.run()
    printfn "Testing FParsec.Text ..."
    FParsec.Test.TextTests.run()
#if LOW_TRUST
#else
    printfn "Testing FParsec.Cloning ..."
    FParsec.Test.CloningTests.run()
    printfn "Testing FParsec.StringBuffer ..."
    FParsec.Test.StringBufferTests.run()
#endif
    printfn "Testing FParsec.CharStream ..."
    FParsec.Test.CharStreamTests.run()
    printfn "Testing FParsec.Primitives ..."
    FParsec.Test.PrimitivesTests.run()
    printfn "Testing FParsec.CharParsers ..."
    FParsec.Test.CharParsersTests.run()
    printfn "Testing FParsec.OperatorPrecedenceParserTests ..."
    FParsec.Test.OperatorPrecedenceParserTests.run()
    printfn "Testing FParsec.IdentifierValidator ..."
    FParsec.Test.IdentifierValidatorTests.run()
#if LOW_TRUST
#else
    printfn "Testing FParsec.StaticMapping ... "
    printfn "(this can take a while)"
    if System.Diagnostics.Debugger.IsAttached then
        printfn "Note: When the Visual Studio debugger is attached, this test requires lots of memory."
    FParsec.Test.RangeTests.run()
    FParsec.Test.StaticMappingTests.run()
#endif

run()