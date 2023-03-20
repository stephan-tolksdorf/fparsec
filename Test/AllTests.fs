module AllTests
// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.


let run() =
    printfn "Testing FParsec.Text ..."
    FParsec.Test.TextTests.run()
#if !LOW_TRUST
  #if !DISABLE_STREAM_BACKTRACKING_TESTS
    // In .NET Core System.Text.Decoder no longer support serialization, see https://github.com/stephan-tolksdorf/fparsec/issues/95
    printfn "Testing FParsec.Cloning ..."
    FParsec.Test.CloningTests.run()
  #endif
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
#if !LOW_TRUST
    printfn "Testing FParsec.StaticMapping ... "
    printfn "(this can take a while)"
    if System.Diagnostics.Debugger.IsAttached then
        printfn "Note: When the Visual Studio debugger is attached, this test requires lots of memory."
    FParsec.Test.RangeTests.run()
    FParsec.Test.StaticMappingTests.run()
#endif
    printfn "No error was found."

// [<EntryPoint>]
// let main _argv =
//
// #if NETCOREAPP
//     System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance);
// #endif
//
//     try
//         run()
//         0
//     with
//     | ex ->
//         printfn $"error: {ex}"
//         1
