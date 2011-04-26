// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.CharStreamTests

#nowarn "9" // "Uses of this construct may result in the generation of unverifiable .NET IL code."
#nowarn "51" // "The address-of operator may result in non-verifiable code."

open System.Text.RegularExpressions

open Microsoft.FSharp.NativeInterop

open FParsec
open FParsec.Test.Test


let EOS = CharStream.EndOfStreamChar

let testNonStreamConstructors() =
     let s = "1234567890"
     let cs = s.ToCharArray()

     let regex = new System.Text.RegularExpressions.Regex(".*")

     let testStream (stream: CharStream) (index: int) (length: int) (indexOffset: int64) (supportsRegex: bool) =
        stream.IndexOfFirstChar |> Equal indexOffset
        stream.Index |> Equal indexOffset
        stream.LineBegin |> Equal indexOffset
        stream.Line |> Equal 1L
        stream.Encoding |> Equal System.Text.Encoding.Unicode

        if length > 0 then
            stream.Peek() |> Equal s.[index]
            stream.Skip(s.Substring(index, length)) |> True
            stream.Index |> Equal (indexOffset + int64 length)
            stream.IsEndOfStream |> Equal true
            stream.Seek(indexOffset)
            stream.Index |> Equal indexOffset
            if supportsRegex then
                stream.Match(regex).Value |> Equal (s.Substring(index, length))
            else
                try stream.Match(regex).Value |> ignore; Fail()
                with :? System.NotSupportedException -> ()
        else
            stream.Peek() |> Equal EOS
            stream.IsEndOfStream |> True

     let testStringStream() =
         use stream = new CharStream<unit>(s)
         testStream stream 0 s.Length 0L true

         use stream = new CharStream<unit>(s, 0, s.Length)
         testStream stream 0 s.Length 0L true
         use stream = new CharStream<unit>(s, 0, s.Length, 1000L)
         testStream stream 0 s.Length 1000L true
         use stream = new CharStream<unit>(s, 1, s.Length - 1)
         testStream stream 1 (s.Length - 1) 0L true
         use stream = new CharStream<unit>(s, 1, 1, 1000L)
         testStream stream 1 1 1000L true
         use stream = new CharStream<unit>(s, 1, 0, 1000L)
         testStream stream 1 0 1000L true

         try new CharStream<unit>((null: string), 1, 10) |> ignore; Fail()
         with :? System.ArgumentNullException -> ()
         try new CharStream<unit>(s, -1, 1) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(s, 11, 0) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(s, 1, 10) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(s, 0, 10, -1L) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(s, 0, 10, (1L <<< 60)) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
     testStringStream()

 #if LOW_TRUST
 #else
     let testCharArrayStream() =
         use stream = new CharStream<unit>(cs, 0, s.Length)
         testStream stream 0 s.Length 0L false
         use stream = new CharStream<unit>(cs, 0, s.Length, 1000L)
         testStream stream 0 s.Length 1000L false
         use stream = new CharStream<unit>(cs, 1, s.Length - 1)
         testStream stream 1 (s.Length - 1) 0L false
         use stream = new CharStream<unit>(cs, 1, 1, 1000L)
         testStream stream 1 1 1000L false
         use stream = new CharStream<unit>(cs, 1, 0, 1000L)
         testStream stream 1 0 1000L false

         try new CharStream<unit>((null: char[]), 1, 10) |> ignore; Fail()
         with :? System.ArgumentNullException -> ()
         try new CharStream<unit>(cs, -1, 1) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(cs, 11, 0) |> ignore; Fail()
         with :?  System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(cs, 1, 10) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(cs, 0, 10, -1L) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(cs, 0, 10, (1L <<< 60)) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
     testCharArrayStream()

     let testCharPointerStream() =
         let handle = System.Runtime.InteropServices.GCHandle.Alloc(cs, System.Runtime.InteropServices.GCHandleType.Pinned)
         let cp = NativePtr.ofNativeInt (handle.AddrOfPinnedObject())

         use stream = new CharStream<unit>(NativePtr.add cp 0, s.Length)
         testStream stream 0 s.Length 0L false
         use stream = new CharStream<unit>(NativePtr.add cp 0, s.Length, 1000L)
         testStream stream 0 s.Length 1000L false
         use stream = new CharStream<unit>(NativePtr.add cp 1, s.Length - 1)
         testStream stream 1 (s.Length - 1) 0L false
         use stream = new CharStream<unit>(NativePtr.add cp 1, 1, 1000L)
         testStream stream 1 1 1000L false
         use stream = new CharStream<unit>(NativePtr.add cp 1, 0, 1000L)
         testStream stream 1 0 1000L false

         try new CharStream<unit>(NativePtr.ofNativeInt 0n, 10) |> ignore; Fail()
         with :? System.ArgumentNullException -> ()
         try new CharStream<unit>(cp, -1) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         if sizeof<System.IntPtr> = 4 then
            try new CharStream<unit>(cp, System.Int32.MaxValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(cp, 10, -1L) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         try new CharStream<unit>(cp, 10, (1L <<< 60)) |> ignore; Fail()
         with :? System.ArgumentOutOfRangeException -> ()
         handle.Free()
     ()
     testCharPointerStream()
    #endif

let testStreamConstructorArgumentChecking() =
    let encoding = System.Text.Encoding.UTF8
    let str = "1234567890"
    let streamBytes = Array.append (encoding.GetPreamble()) (encoding.GetBytes(str))
    use stream = new System.IO.MemoryStream(streamBytes)
    try new CharStream<unit>((null: System.IO.Stream), false, encoding) |> ignore; Fail()
    with :? System.ArgumentNullException -> ()
    try new CharStream<unit>(stream, null) |> ignore; Fail()
    with :? System.ArgumentNullException -> ()

    let tempFilePath = System.IO.Path.GetTempFileName()
    use nonReadableStream = new System.IO.FileStream(tempFilePath, System.IO.FileMode.Open, System.IO.FileAccess.Write)
    try new CharStream<unit>(nonReadableStream, encoding) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    nonReadableStream.Write(streamBytes, 0, streamBytes.Length)
    nonReadableStream.Close()

    try new CharStream<unit>((null: string), encoding) |> ignore; Fail()
    with :? System.ArgumentNullException -> ()
    try new CharStream<unit>("", (null: System.Text.Encoding)) |> ignore; Fail()
    with :? System.ArgumentNullException -> ()

    use charStream = new CharStream<unit>(tempFilePath, System.Text.Encoding.ASCII, true)
    charStream.Read(str.Length + 1) |> Equal str
    charStream.Dispose()

    System.IO.File.Delete(tempFilePath)


type CustomPreambleUTF8Encoding(preamble: byte[]) =
    inherit System.Text.UTF8Encoding()
    override t.GetPreamble() = preamble

let testEncodingDetection() =
    let s = "1234567890"
    let gb18030 = System.Text.Encoding.GetEncoding(54936) // an encoding we can't detect

    let test (e: System.Text.Encoding) =
        let bs0 = e.GetPreamble()
        use cs0 = new CharStream<unit>(new System.IO.MemoryStream(bs0, false), gb18030);
        cs0.Encoding.CodePage |> Equal (e.CodePage)

        bs0.[1] <- 33uy
        use cs0 = new CharStream<unit>(new System.IO.MemoryStream(bs0, false), gb18030);
        cs0.Encoding|> ReferenceEqual gb18030

        let bs = Array.append (e.GetPreamble()) (e.GetBytes(s))
        use cs = new CharStream<unit>(new System.IO.MemoryStream(bs, false), gb18030);
        cs.Encoding.CodePage |> Equal (e.CodePage)
        cs.Read(s.Length) |> Equal s
        use cs2 = new CharStream<unit>(new System.IO.MemoryStream(bs, false), e);
        cs2.Encoding |> ReferenceEqual e
        cs2.Read(s.Length) |> Equal s
        use cs3 = new CharStream<unit>(new System.IO.MemoryStream(bs, false), false, gb18030, false);
        cs3.Encoding |> ReferenceEqual gb18030

#if SILVERLIGHT
    try test (System.Text.UTF32Encoding(false, true)); Fail()
    with :? System.NotSupportedException -> ()
    try test (System.Text.UTF32Encoding(true, true)); Fail()
    with :? System.NotSupportedException -> ()
#else
    test (System.Text.UTF32Encoding(false, true))
    test (System.Text.UTF32Encoding(true, true))
#endif

    test (System.Text.UnicodeEncoding(false, true))
    test (System.Text.UnicodeEncoding(true, true))
    test (System.Text.UTF8Encoding(true))

    let e = CustomPreambleUTF8Encoding([|0uy;1uy;2uy;3uy;4uy|])
    let bs = Array.append (e.GetPreamble()) (e.GetBytes(s))
    use cs = new CharStream<unit>(new System.IO.MemoryStream(bs, false), e);
    cs.Encoding.CodePage |> Equal (e.CodePage)
    cs.Read(s.Length) |> Equal s



/// creates a CharStream with block size 8 and block overlap 3
let createMultiBlockTestStream byteStream encoding =
    new CharStream<int>(byteStream, false,
                         encoding, true,
                         #if LOW_TRUST
                         #else
                            8, 3,
                         #endif
                            16);


let createMultiBlockUtf8TestStream (chars: char[]) =
    let e =  System.Text.Encoding.UTF8
    let bs = e.GetBytes(chars)
    createMultiBlockTestStream (new System.IO.MemoryStream(bs, false)) e

type NonSeekableMemoryStream(bytes: byte[]) =
     inherit System.IO.MemoryStream(bytes)
     override t.Seek(offset, origin) = raise (System.NotSupportedException())
     override t.CanSeek = false

#if LOW_TRUST
#else

[<AutoSerializable(false)>]
type NonSerializableUTF8Decoder() =
    inherit System.Text.Decoder()
    let decoder = System.Text.Encoding.UTF8.GetDecoder()
    override t.GetCharCount(bytes: byte[], index: int, count: int) : int =
        raise (System.NotImplementedException())
    override t.GetChars(bytes: byte[], byteIndex: int, byteCount: int, chars: char[], charIndex: int): int =
        raise (System.NotImplementedException())

    override t.Reset() = decoder.Reset()
    override t.Convert(bytes, byteCount, chars, charCount, flush,  bytesUsed: byref<int>, charsUsed: byref<int>, completed: byref<bool>) =
        decoder.Convert(bytes, byteCount, chars, charCount, flush, &bytesUsed, &charsUsed, &completed)

    interface System.Runtime.Serialization.ISerializable with
        member t.GetObjectData(info, context) = raise (System.NotSupportedException())

type UTF8EncodingWithNonSerializableDecoder() =
    inherit System.Text.UTF8Encoding()
    override t.GetDecoder() = new NonSerializableUTF8Decoder() :> System.Text.Decoder

let testNonSeekableCharStreamHandling() =
    let str = "1234567890ABCDEFGHIJKLMNOPQ"
    let streamBytes = Array.append (System.Text.Encoding.UTF8.GetPreamble()) (System.Text.Encoding.UTF8.GetBytes(str))

    let testNonSeekableByteStream() =
        let encoding = System.Text.Encoding.UTF8
        use byteStream = new NonSeekableMemoryStream(streamBytes)
        use stream = createMultiBlockTestStream byteStream System.Text.Encoding.Unicode
        stream.Skip(9)
        try stream.Skip(-9) |> ignore
            Fail()
        with :? System.NotSupportedException as e -> ()

        use byteStream2 = new NonSeekableMemoryStream(streamBytes.[..(6 + 3)])
        use stream2 = createMultiBlockTestStream byteStream2 System.Text.Encoding.Unicode
        stream2.Read(7) |> Equal str.[..6]
        stream2.IsEndOfStream |> True

    testNonSeekableByteStream()

    let testNonSerializableEncoding() =
        let nsEncoding = UTF8EncodingWithNonSerializableDecoder()
        use byteStream = new System.IO.MemoryStream(streamBytes)
        use stream = createMultiBlockTestStream byteStream nsEncoding
        // seeking forward should work
        stream.Read(str.Length) |> Equal str
        stream.IsEndOfStream |> True
        // ... and backtracking to the first block should work too
        stream.SkipAndPeek(-str.Length) |> Equal str.[0]
        stream.Seek(int64 str.Length - 1L)
        stream.Read() |> Equal str.[str.Length - 1]
        stream.IsEndOfStream |> True
        // ... but backtracking to a block other than the first should fail
        try
            stream.Seek(8L)
            Fail()
        with :? System.NotSupportedException as e -> ()
    testNonSerializableEncoding()

#endif


let testDecoderFallbackExceptionHandling() =
    let encoding = System.Text.Encoding.GetEncoding("utf-32", System.Text.EncoderFallback.ExceptionFallback, System.Text.DecoderFallback.ExceptionFallback)

    let getStreamBytes bytes =
        Array.concat [|[|0x00uy|]; encoding.GetPreamble(); bytes|]

    let test (byteStream: System.IO.Stream) multiBlock (position: int64) =
        try
            use stream = if not multiBlock then new CharStream<_>(byteStream, encoding)
                         else createMultiBlockTestStream byteStream encoding
            stream.Read(int position + 4) |> ignore
            Fail()
        with :? System.Text.DecoderFallbackException as e ->
            unbox (e.Data.["Stream.Position"]) |> Equal position

    let shortStreamBytes = getStreamBytes (encoding.GetBytes("123\u00005"))
    shortStreamBytes.[1 + 4 + 3*4 + 1] <- 0xd8uy

    use shortByteStream = new System.IO.MemoryStream(shortStreamBytes)
    shortByteStream.ReadByte() |> ignore
    test shortByteStream false (int64 (1 + 4 + 3*4))
    use nsShortByteStream = new NonSeekableMemoryStream(shortStreamBytes)
    nsShortByteStream.ReadByte() |> ignore
    test nsShortByteStream false (int64 (    4 + 3*4))

    let longStreamBytes = getStreamBytes (encoding.GetBytes("12345678901\u00003"))
    longStreamBytes.[1 + 4 + 11*4 + 1] <- 0xd8uy

    use longByteStream = new System.IO.MemoryStream(longStreamBytes)
    longByteStream.ReadByte() |> ignore
    test longByteStream true (int64 (1 + 4 + 11*4))

    use nsLongByteStream = new NonSeekableMemoryStream(longStreamBytes)
    nsLongByteStream.ReadByte() |> ignore
    test nsLongByteStream true (int64 (    4 + 11*4))



let testEmptyStream (stream: CharStream<_>) =

    let index0 = stream.Index
    stream.IsBeginOfStream |> True
    stream.IsEndOfStream |> True
    stream.IndexOfFirstChar |> Equal index0
    stream.IndexOfLastCharPlus1 |> Equal index0

    stream.Seek(index0 + 1L); stream.Index |> Equal index0

    stream.Peek() |> Equal EOS
    stream.Peek2() |> Equal (TwoChars(EOS, EOS))
    stream.Peek(0) |> Equal EOS
    stream.Peek(1) |> Equal EOS
    stream.Peek(-1) |> Equal EOS
    stream.Peek(System.Int32.MaxValue) |> Equal EOS
    stream.Peek(System.Int32.MinValue) |> Equal EOS
    stream.Peek(0u) |> Equal EOS
    stream.Peek(1u) |> Equal EOS
    stream.Peek(System.UInt32.MaxValue) |> Equal EOS
    stream.PeekString(0) |> Equal ""
    stream.PeekString(1) |> Equal ""
    stream.PeekString(System.Int32.MaxValue) |> Equal ""
    let array = [|'x'|]
    stream.PeekString(array, 0, 1) |> Equal 0
    array.[0] |> Equal 'x'
    #if LOW_TRUST
    #else
    let handle = System.Runtime.InteropServices.GCHandle.Alloc(array, System.Runtime.InteropServices.GCHandleType.Pinned)
    let arrayPtr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject())

    stream.PeekString(arrayPtr, 1) |> Equal 0
    array.[0] |> Equal 'x'
    #endif

    stream.Read() |> Equal EOS;                     stream.Index |> Equal index0
    stream.Read(0) |> Equal "";                     stream.Index |> Equal index0
    stream.Read(1) |> Equal "";                     stream.Index |> Equal index0
    stream.Read(System.Int32.MaxValue) |> Equal ""; stream.Index |> Equal index0
    stream.Read(array, 0, 1) |> Equal 0;            stream.Index |> Equal index0
    #if LOW_TRUST
    #else
    stream.Read(arrayPtr, 1) |> Equal 0
    array.[0] |> Equal 'x';                            stream.Index |> Equal index0
    #endif

    stream.Match(EOS) |> False
    stream.Match("")  |> True
    stream.Match("x") |> False
    stream.MatchCaseFolded(EOS) |> False
    stream.MatchCaseFolded("")  |> True
    stream.MatchCaseFolded("x") |> False
    stream.Match([||],0,0) |> True
    stream.Match([|'x'|],0,1) |> False
    #if LOW_TRUST
    #else
    stream.Match(arrayPtr, 0) |> True
    stream.Match(arrayPtr, 1) |> False
    stream.MatchCaseFolded(arrayPtr, 0) |> True
    stream.MatchCaseFolded(arrayPtr, 1) |> False
    #endif
    stream.Match(Regex("x")).Success |> False

    stream.Skip(EOS) |> False;           stream.Index |> Equal index0
    stream.Skip("")  |> True;            stream.Index |> Equal index0
    stream.Skip("x") |> False;           stream.Index |> Equal index0
    stream.Skip(EOS) |> False;           stream.Index |> Equal index0
    stream.Skip("")  |> True;            stream.Index |> Equal index0
    stream.Skip("x") |> False;           stream.Index |> Equal index0
    stream.Skip([||], 0, 0) |> True;     stream.Index |> Equal index0
    stream.Skip([|'x'|], 0, 1) |> False; stream.Index |> Equal index0
    #if LOW_TRUST
    #else
    stream.Skip(arrayPtr, 0) |> True;  stream.Index |> Equal index0
    stream.Skip(arrayPtr, 1) |> False; stream.Index |> Equal index0
    stream.Skip(arrayPtr, 0) |> True;  stream.Index |> Equal index0
    stream.Skip(arrayPtr, 1) |> False; stream.Index |> Equal index0
    #endif

    let tag = stream.StateTag
    stream.Skip(); stream.Index |> Equal index0
    stream.StateTag |> Equal tag

    stream.Skip(0); stream.Index |> Equal index0
    stream.Skip(1); stream.Index |> Equal index0
    stream.Skip(System.Int32.MaxValue); stream.Index |> Equal index0

    try stream.Skip(-1) |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()
    try stream.Skip(System.Int32.MinValue) |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()

    stream.Skip(0L);                    stream.Index |> Equal index0
    stream.Skip(1L);                    stream.Index |> Equal index0
    stream.Skip(System.Int64.MaxValue); stream.Index |> Equal index0

    try stream.Skip(-1L) |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()
    try stream.Skip(System.Int64.MinValue) |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()

    stream.Skip(0u);                     stream.Index |> Equal index0
    stream.Skip(1u);                     stream.Index |> Equal index0
    stream.Skip(System.UInt32.MaxValue); stream.Index |> Equal index0

    stream.SkipAndPeek(0) |> Equal EOS;                     stream.Index |> Equal index0
    stream.SkipAndPeek(1) |> Equal EOS;                     stream.Index |> Equal index0
    stream.SkipAndPeek(System.Int32.MaxValue) |> Equal EOS; stream.Index |> Equal index0
    stream.SkipAndPeek(-1) |> Equal EOS;                    stream.Index |> Equal index0
    stream.SkipAndPeek(System.Int32.MinValue) |> Equal EOS; stream.Index |> Equal index0

    stream.SkipAndPeek(0u) |> Equal EOS;                     stream.Index |> Equal index0
    stream.SkipAndPeek(1u) |> Equal EOS;                     stream.Index |> Equal index0
    stream.SkipAndPeek(System.UInt32.MaxValue) |> Equal EOS; stream.Index |> Equal index0

    let state = stream.State
    stream.ReadFrom(state, false) |> Equal ""

    let tag = stream.StateTag

    stream.SkipWhitespace() |> False;        stream.Index |> Equal index0
    stream.SkipUnicodeWhitespace() |> False; stream.Index |> Equal index0
    stream.SkipNewline() |> False;           stream.Index |> Equal index0
    stream.SkipUnicodeNewline() |> False;    stream.Index |> Equal index0

    stream.SkipNewlineThenWhitespace(8, true) |> Equal -1;    stream.Index |> Equal index0

    stream.SkipRestOfLine(false); stream.Index |> Equal index0
    stream.SkipRestOfLine(true);  stream.Index |> Equal index0

    stream.ReadRestOfLine(false) |> Equal ""; stream.Index |> Equal index0
    stream.ReadRestOfLine(true) |> Equal "";  stream.Index |> Equal index0

    stream.ReadCharOrNewline() |> Equal EOS

    stream.SkipCharsOrNewlines(1) |> Equal 0;                     stream.Index |> Equal index0
    stream.SkipCharsOrNewlines(System.Int32.MaxValue) |> Equal 0; stream.Index |> Equal index0

    stream.ReadCharsOrNewlines(1, false) |> Equal "";                     stream.Index |> Equal index0
    stream.ReadCharsOrNewlines(1, true)  |> Equal "";                     stream.Index |> Equal index0
    stream.ReadCharsOrNewlines(System.Int32.MaxValue, false) |> Equal ""; stream.Index |> Equal index0
    stream.ReadCharsOrNewlines(System.Int32.MaxValue, true)  |> Equal ""; stream.Index |> Equal index0

    stream.SkipCharsOrNewlinesWhile((fun c -> true)) |> Equal 0;  stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesWhile((fun c -> true), 0, 1) |> Equal 0;                      stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesWhile((fun c -> true), 0, System.Int32.MaxValue) |> Equal 0;  stream.Index |> Equal index0

    stream.ReadCharsOrNewlinesWhile((fun c -> true), false) |> Equal ""; stream.Index |> Equal index0
    stream.ReadCharsOrNewlinesWhile((fun c -> true), true)  |> Equal ""; stream.Index |> Equal index0
    stream.ReadCharsOrNewlinesWhile((fun c -> true), 0, 1, false) |> Equal ""; stream.Index |> Equal index0
    stream.ReadCharsOrNewlinesWhile((fun c -> true), 0, 1, true) |> Equal "";  stream.Index |> Equal index0
    stream.ReadCharsOrNewlinesWhile((fun c -> true), 0, System.Int32.MaxValue, false) |> Equal ""; stream.Index |> Equal index0
    stream.ReadCharsOrNewlinesWhile((fun c -> true), 0, System.Int32.MaxValue, true) |> Equal "";  stream.Index |> Equal index0

    let mutable b = false
    stream.SkipCharsOrNewlinesUntilString("1", 1, &b) |> Equal 0;                               stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesUntilString("1", System.Int32.MaxValue, &b) |> Equal 0;           stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesUntilCaseFoldedString("1", 1, &b) |> Equal 0;                     stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesUntilCaseFoldedString("1", System.Int32.MaxValue, &b) |> Equal 0; stream.Index |> Equal index0

    let mutable s = ""
    stream.SkipCharsOrNewlinesUntilString("1", 1, false, &s) |> Equal 0;                                stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesUntilString("1", System.Int32.MaxValue, false, &s) |> Equal 0;            stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesUntilCaseFoldedString("1", 1, false, &s) |> Equal 0;                      stream.Index |> Equal index0
    stream.SkipCharsOrNewlinesUntilCaseFoldedString("1", System.Int32.MaxValue, false, &s) |> Equal 0;  stream.Index |> Equal index0

    stream.StateTag |> Equal tag

    #if LOW_TRUST
    #else
    handle.Free()
    #endif

let testBasicCharStreamMethods (stream: CharStream<int>) (refString: string) blockSize blockOverlap minRegexSpace =
    let index0 = stream.IndexOfFirstChar
    let dollarString = new string('$', refString.Length)
    let N = refString.Length

    let state0 = stream.State

    let seekStreamTo (i: int) =
        stream.BacktrackTo(state0)
        stream.Skip(uint32 i)

    let testProperties() =
        stream.BacktrackTo(state0)
        let tag = stream.StateTag

        stream.Name <- "Name2"
        stream.Name |> Equal "Name2"
        stream.StateTag |> Equal (tag + _1)

        stream.UserState <- -333
        stream.UserState |> Equal -333
        stream.StateTag |> Equal (tag + _1 + _1)

        stream.Skip()
        stream.Column |> Equal (stream.Index - stream.LineBegin + 1L)

        stream.StateTag <- tag
        stream.StateTag |> Equal tag

        let l = stream.Line
        stream.SetLine_WithoutCheckAndWithoutIncrementingTheStateTag(l + 2L)
        stream.Line |> Equal 3L
        stream.StateTag |> Equal tag

        let lb = stream.LineBegin
        stream.SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag(lb + 1L)
        stream.LineBegin |> Equal (lb + 1L)
        stream.StateTag |> Equal tag

        (stream.BlockOverlap >= 0) |> True
        let minRegexSpace = stream.MinRegexSpace
        if minRegexSpace <> 0 then
            stream.MinRegexSpace <- minRegexSpace - 1
            stream.MinRegexSpace |> Equal (minRegexSpace - 1)
            try stream.MinRegexSpace <- -1; Fail()
            with :? System.ArgumentException -> ()
            try stream.MinRegexSpace <- stream.BlockOverlap + 1; Fail()
            with :? System.ArgumentException -> ()
            stream.MinRegexSpace <- minRegexSpace

    testProperties()

    let testRegisterNewlines() =
        stream.BacktrackTo(state0)
        let line0 = stream.Line
        let lineBegin0 = stream.LineBegin
        let tag0 = stream.StateTag

        stream.Skip()
        stream.RegisterNewline()
        stream.StateTag |> Equal (tag0 + _1 + _1)
        stream.Line |> Equal (line0 + 1L)
        stream.LineBegin |> Equal stream.Index

        stream.BacktrackTo(state0)
        stream.Skip(3)
        stream.RegisterNewlines(2, 1)
        stream.StateTag |> Equal (tag0 + _1 + _1)
        stream.Line |> Equal (line0 + 2L)
        stream.LineBegin |> Equal (stream.Index - 1L)

        stream.BacktrackTo(state0)
        stream.Skip(3)
        stream.RegisterNewlines(2L, 1L)
        stream.StateTag |> Equal (tag0 + _1 + _1)
        stream.Line |> Equal (line0 + 2L)
        stream.LineBegin |> Equal (stream.Index - 1L)

    testRegisterNewlines()

    let testMove i1 i2 =
        let tag1 = state0.Tag + _1
        let tag2 = tag1 + _1

        let index1 = index0 + int64 (min i1 N)
        let index2 = index0 + int64 (min i2 N)

        let c1 = if i1 < N then refString.[i1] else EOS
        let c2 = if i2 < N then refString.[i2] else EOS

        let d = i2 - min i1 N

        stream.BacktrackTo(state0)
        stream.Seek(index0 + int64 i1)
        stream.Index |> Equal index1
        let indexToken1 = stream.IndexToken
        stream.StateTag |> Equal tag1
        stream.Peek() |> Equal c1
        stream.IsBeginOfStream |> Equal (i1 = 0)
        stream.IsEndOfStream |> Equal (i1 >= N)

        stream.Seek(index0 + int64 i2)
        stream.Index |> Equal index2
        stream.StateTag |> Equal tag2
        stream.Peek() |> Equal c2
        stream.IsBeginOfStream |> Equal (i2 = 0)
        stream.IsEndOfStream |> Equal (i2 >= N)

        indexToken1.GetIndex(stream) |> Equal index1
        stream.Seek(indexToken1)
        stream.Index |> Equal index1
        stream.Peek() |> Equal c1

        seekStreamTo i1
        stream.Peek(d) |> Equal c2
        stream.Index |> Equal index1
        stream.StateTag |> Equal tag1

        if d >= 0 then
            seekStreamTo i1
            stream.Peek(uint32 d) |> Equal c2
            stream.Index |> Equal index1
            stream.StateTag |> Equal tag1

        let checkStream() =
            stream.Index |> Equal index2
            if index1 <> index2 then
                stream.StateTag |> Equal tag2
            else
                (stream.StateTag = tag2 - _1 || stream.StateTag = tag2) |> True

        seekStreamTo i1
        stream.Skip(d)
        checkStream()

        seekStreamTo i1
        stream.Skip(int64 d)
        checkStream()

        seekStreamTo i1
        stream.SkipAndPeek(d) |> Equal c2
        checkStream()

        if d >= 0 then
            if d = 1 then
                seekStreamTo i1
                stream.Skip()
                stream.Index |> Equal index2
                if index2 <> index1 then
                    stream.StateTag |> Equal tag2
                else
                    stream.StateTag |> Equal tag1

                seekStreamTo i1
                stream.SkipAndPeek() |> Equal c2
                stream.Index |> Equal index2
                if index2 <> index1 then
                    stream.StateTag |> Equal tag2
                else
                    stream.StateTag |> Equal tag1

            seekStreamTo i1
            stream.Skip(uint32 d)
            checkStream()

            seekStreamTo i1
            stream.SkipAndPeek(uint32 d) |> Equal c2
            checkStream()

        elif i2 = 0 then // d <= 0
            seekStreamTo i1
            stream.Peek(d - 1) |> Equal EOS
            stream.Index |> Equal index1
            stream.StateTag |> Equal tag1
            stream.Peek(System.Int32.MinValue) |> Equal EOS
            stream.Index |> Equal index1
            stream.StateTag |> Equal tag1

            seekStreamTo i1
            stream.SkipAndPeek(d - 1) |> Equal EOS
            checkStream()

            seekStreamTo i1
            stream.SkipAndPeek(System.Int32.MinValue) |> Equal EOS
            checkStream()

    for i1 = 0 to N + 2 do
        for i2 = 0 to N + 2 do
            testMove i1 i2


    let testMoveException() =
        let endIndex = index0 + int64 N
        stream.Seek(System.Int64.MaxValue)
        stream.Index |> Equal endIndex
        stream.IsEndOfStream |> True
        try  stream.Seek(-1L) |> ignore; Fail ()
        with :? System.ArgumentOutOfRangeException -> ()
        try  stream.Seek(index0 - 1L) |> ignore; Fail ()
        with :? System.ArgumentOutOfRangeException -> ()
        try  stream.Seek(System.Int64.MinValue) |> ignore; Fail ()
        with :? System.ArgumentOutOfRangeException -> ()
        stream.Seek(index0)

        let indexToken = CharStreamIndexToken()
        try stream.Seek(indexToken) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try indexToken.GetIndex(stream) |> ignore; Fail()
        with :? System.InvalidOperationException -> ()

        let state = CharStreamState()
        try stream.BacktrackTo(state); Fail()
        with :? System.ArgumentException -> ()
        try state.GetIndex(stream) |> ignore; Fail()
        with :? System.InvalidOperationException -> ()
        try state.GetPosition(stream) |> ignore; Fail()
        with :? System.InvalidOperationException -> ()
        try stream.ReadFrom(state, true) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try stream.CreateSubstream(state) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try state.IndexToken |> ignore; Fail()
        with :? System.InvalidOperationException -> ()



        for i = 0 to N do
            seekStreamTo i;
            stream.Skip(System.Int32.MaxValue)
            stream.Index |> Equal endIndex

            seekStreamTo i;
            stream.Skip(System.UInt32.MaxValue)
            stream.Index |> Equal endIndex

            seekStreamTo i;
            stream.Skip(System.Int64.MaxValue)
            stream.Index |> Equal endIndex

            seekStreamTo i;
            stream.SkipAndPeek(System.Int32.MaxValue) |> Equal EOS
            stream.Index |> Equal endIndex

            seekStreamTo i;
            stream.SkipAndPeek(System.UInt32.MaxValue) |> Equal EOS
            stream.Index |> Equal endIndex

            seekStreamTo i;
            stream.Peek(System.Int32.MaxValue) |> Equal EOS

            seekStreamTo i;
            stream.Peek(System.UInt32.MaxValue) |> Equal EOS

            // MinValue behaviour is checked in testMove

            try  seekStreamTo i; stream.Seek(index0 - 1L) |> ignore; Fail ()
            with :? System.ArgumentOutOfRangeException -> ()
            try  seekStreamTo i; stream.Seek(-1L) |> ignore; Fail ()
            with :? System.ArgumentOutOfRangeException -> ()
            try  seekStreamTo i; stream.Seek(System.Int64.MinValue) |> ignore; Fail ()
            with :? System.ArgumentOutOfRangeException -> ()

            try seekStreamTo i; stream.Skip(-i - 1) |> ignore; Fail ()
            with :? System.ArgumentOutOfRangeException -> ()
            try  seekStreamTo i; stream.Skip(int64 (-i - 1)) |> ignore; Fail ()
            with :? System.ArgumentOutOfRangeException -> ()
            try  seekStreamTo i; stream.Skip(System.Int32.MinValue) |> ignore; Fail ()
            with :? System.ArgumentOutOfRangeException -> ()
            try  seekStreamTo i; stream.Skip(System.Int64.MinValue) |> ignore; Fail ()
            with :? System.ArgumentOutOfRangeException -> ()

    testMoveException()

    let regex = new Regex(".*", RegexOptions.Singleline)

    let testMatch i n =
        let test (str: string) (result: bool) =
            assert (str.Length = n)
            let strA = str.ToCharArray()
            let cfStr = FParsec.Text.FoldCase(str)
            let cfStrA = cfStr.ToCharArray()

            seekStreamTo i
            let tag1 = stream.StateTag
            let index1 = stream.Index
            let tag2 = tag1 + _1
            let index2 = min (stream.Index + int64 n) stream.IndexOfLastCharPlus1

            let checkStream charsAreSkipped =
                if not charsAreSkipped then
                    stream.Index |> Equal index1
                    stream.StateTag |> Equal tag1
                else
                    stream.Index |> Equal index2
                    if n <> 0 then
                        stream.StateTag |> Equal tag2
                    else
                        (stream.StateTag = tag1 || stream.StateTag = tag2) |> True

            stream.Match(str) |> Equal result
            checkStream false

            seekStreamTo i
            stream.Skip(str) |> Equal result
            checkStream result

            seekStreamTo i
            stream.MatchCaseFolded(cfStr) |> Equal result
            checkStream false

            seekStreamTo i
            stream.SkipCaseFolded(cfStr) |> Equal result
            checkStream result

            seekStreamTo i
            stream.Match(strA, 0, n) |> Equal result
            checkStream false

            seekStreamTo i
            stream.Skip(strA, 0, n) |> Equal result
            checkStream result

            if n = 1 then
                seekStreamTo i
                stream.Match(str.[0]) |> Equal result
                checkStream false

                seekStreamTo i
                stream.Skip(str.[0]) |> Equal result
                checkStream result

                seekStreamTo i
                stream.MatchCaseFolded(cfStr.[0]) |> Equal result
                checkStream false

                seekStreamTo i
                stream.SkipCaseFolded(cfStr.[0]) |> Equal result
                checkStream result

            elif n = 2 then
                seekStreamTo i
                if stream.Skip(FParsec.TwoChars(str.[0], str.[1])) <> result then
                    stream.Skip(FParsec.TwoChars(str.[0], str.[1])) |> Equal result
                //stream.Skip(FParsec.TwoChars(str.[0], str.[1])) |> Equal result
                checkStream result

            elif n > 1 then
                seekStreamTo i
                let restIsEqual = stream.Peek(n - 1) = str.[n - 1] // str only differs in first or last char
                seekStreamTo (i + 1)
                let index11 = stream.Index
                stream.Match(strA, 1, n - 1) |> Equal restIsEqual
                stream.Index |> Equal index11
                stream.StateTag |> Equal tag1

                seekStreamTo (i + 1)
                stream.Skip(strA, 1, n - 1) |> Equal restIsEqual
                if restIsEqual then
                    stream.Index |> Equal index2
                    stream.StateTag |> Equal tag2
                else
                    stream.Index |> Equal index11
                    stream.StateTag |> Equal tag1
        #if LOW_TRUST
        #else
            if n > 0 then
                let handle = System.Runtime.InteropServices.GCHandle.Alloc(str, System.Runtime.InteropServices.GCHandleType.Pinned)
                let strPtr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject())
                let cfHandle = System.Runtime.InteropServices.GCHandle.Alloc(cfStr, System.Runtime.InteropServices.GCHandleType.Pinned)
                let cfStrPtr = NativePtr.ofNativeInt (cfHandle.AddrOfPinnedObject())

                seekStreamTo i
                stream.Match(strPtr, n) |> Equal result
                checkStream false

                seekStreamTo i
                stream.Skip(strPtr, n) |> Equal result
                checkStream result

                seekStreamTo i
                stream.MatchCaseFolded(cfStrPtr, n) |> Equal result
                checkStream false

                seekStreamTo i
                stream.SkipCaseFolded(cfStrPtr, n) |> Equal result
                checkStream result

                handle.Free()
                cfHandle.Free()
            else
                let handle = System.Runtime.InteropServices.GCHandle.Alloc("$", System.Runtime.InteropServices.GCHandleType.Pinned)
                let ptr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject())
                let mutable c = '$'
                seekStreamTo i
                stream.Match(ptr, n) |> Equal true
                checkStream false

                seekStreamTo i
                stream.Skip(ptr, n) |> Equal true
                checkStream true

                seekStreamTo i
                stream.MatchCaseFolded(ptr, n) |> Equal true
                checkStream false

                seekStreamTo i
                stream.SkipCaseFolded(ptr, n) |> Equal true
                checkStream true
                handle.Free()
        #endif

        if n = 0 then
            test "" true
        elif i < N then
            let ci1 = char (int refString.[i] + 1)
            if n > 0 && i + n <= N then
                test (refString.Substring(i, n)) true
                if n = 1 then
                    test (ci1.ToString()) false
                else
                    test (ci1.ToString() + refString.Substring(i + 1, n - 1)) false
                    test (refString.Substring(i, n - 1) + ((char (int (refString.[i + n - 1]) + 1)).ToString())) false
            else
                test (refString.Substring(i, N - i) + (new string(refString.[N - 1], n - (N - i)))) false

            seekStreamTo i
            let index = stream.Index
            let tag = stream.StateTag
            let mstr = stream.Match(regex).Value
            stream.Index |> Equal index
            stream.StateTag |> Equal tag
            let minLength = if blockOverlap = 0 then N - i else min minRegexSpace (N - i)
            (mstr.Length >= minLength) |> True
            mstr |> Equal (refString.Substring(i, mstr.Length))
        else
            let str = new string(refString.[N - 1], n)
            test str false

            seekStreamTo i
            let index = stream.Index
            let tag = stream.StateTag
            stream.Match(regex).Value |> Equal ""
            stream.Index |> Equal index
            stream.StateTag |> Equal tag

    for i = 0 to N do
        for n = 0 to N + 15 - i do
            testMatch i n


    let testMatchException() =
        let str = "$$$"
        let a = str.ToCharArray()

        for i in [0; 1; N - 1; N] do
            seekStreamTo i

            try  stream.Match(null: string) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try  stream.Skip(null: string) |> ignore; Fail()
            with :? System.NullReferenceException -> ()

            try  stream.MatchCaseFolded(null) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try  stream.SkipCaseFolded(null) |> ignore; Fail()
            with :? System.NullReferenceException -> ()

            try  stream.Match((null: char[]), 0, 0) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try  stream.Match(a, 0, 4) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, 2, 2) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, 3, 1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, -1, 0) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, 0, -1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, 0, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, System.Int32.MinValue, 0) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(a, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
        #if LOW_TRUST
        #else
            if i <> N then
                try  stream.Match(NativePtr.ofNativeInt 0n, 1) |> ignore; Fail()
                with :? System.NullReferenceException -> ()

            let handle = System.Runtime.InteropServices.GCHandle.Alloc([|'$'|], System.Runtime.InteropServices.GCHandleType.Pinned)
            let ptr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject())
            try  stream.Match(ptr, -1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Match(ptr, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()

            if i <> N then
                try  stream.MatchCaseFolded(NativePtr.ofNativeInt 0n, 1) |> ignore; Fail()
                with :? System.NullReferenceException -> ()
            try  stream.MatchCaseFolded(ptr, -1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.MatchCaseFolded(ptr, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            handle.Free()
        #endif

            try stream.Match(null: Regex) |> ignore; Fail()
            with :? System.NullReferenceException -> ()

    testMatchException()

    let testRead i n =

        seekStreamTo i
        let tag1 = stream.StateTag
        let index1 = stream.Index
        let tag2 = tag1 + _1
        let index2 = min (index1 + int64 n) stream.IndexOfLastCharPlus1

        let str = if i < N then refString.Substring(i, min n (N - i)) else ""

        let checkStream charsAreSkipped =
            if not charsAreSkipped then
                stream.Index |> Equal index1
                stream.StateTag |> Equal tag1
            else
                stream.Index |> Equal index2
                if str.Length <> 0 then
                    stream.StateTag |> Equal tag2
                else
                    (stream.StateTag = tag1 || stream.StateTag = tag2) |> True


        stream.Read(n) |> Equal str
        checkStream true

        seekStreamTo i
        stream.PeekString(n) |> Equal str
        checkStream false

        seekStreamTo i
        stream.PeekString(n) |> Equal str
        checkStream false

        let cs = Array.create (N + 3) '$'
        seekStreamTo i
        stream.Read(cs, i%3, min n N) |> Equal str.Length
        checkStream true
        new string(cs, i%3, str.Length) |> Equal str
        for j = 0 to i%3 - 1 do cs.[j] |> Equal '$'
        for j = i%3 + str.Length to N - 2 do cs.[j] |> Equal '$'

        Array.fill cs 0 (N + 3) '$'
        seekStreamTo i
        stream.PeekString(cs, i%3, min n N) |> Equal str.Length
        checkStream false
        new string(cs, i%3, str.Length) |> Equal str
        for j = 0 to i%3 - 1 do cs.[j] |> Equal '$'
        for j = i%3 + str.Length to N - 2 do cs.[j] |> Equal '$'

    #if LOW_TRUST
    #else
        let handle = System.Runtime.InteropServices.GCHandle.Alloc(cs, System.Runtime.InteropServices.GCHandleType.Pinned)
        let ptr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject())
        Array.fill cs 0 (N + 3) '$'
        seekStreamTo i
        stream.Read(NativePtr.add ptr (i%3), min n N) |> Equal str.Length
        checkStream true
        new string(cs, i%3, str.Length) |> Equal str
        for j = 0 to i%3 - 1 do cs.[j] |> Equal '$'
        for j = i%3 + str.Length to N - 2 do cs.[j] |> Equal '$'

        Array.fill cs 0 (N + 3) '$'
        seekStreamTo i
        stream.PeekString(NativePtr.add ptr (i%3), min n N) |> Equal str.Length
        new string(cs, i%3, str.Length) |> Equal str
        for j = 0 to i%3 - 1 do cs.[j] |> Equal '$'
        for j = i%3 + str.Length to N - 2 do cs.[j] |> Equal '$'

        handle.Free()
    #endif

        if n = 1 then
            seekStreamTo i
            stream.Read() |> Equal (if str.Length > 0 then str.[0] else EOS)
            checkStream true
        elif n = 2 then
            let c2 = new FParsec.TwoChars((if str.Length > 0 then str.[0] else EOS), (if str.Length = 2 then str.[1] else EOS))
            seekStreamTo i
            stream.Peek2() |> Equal c2
            checkStream false

        seekStreamTo i
        let indexToken = stream.IndexToken
        stream.Skip(n)
        stream.ReadFrom(indexToken) |> Equal str
        checkStream true

        seekStreamTo i
        let pos1 = stream.Position
        let state = stream.State
        stream.Skip(n)
        state.GetIndex(stream) |> Equal index1
        state.IndexToken.GetIndex(stream) |> Equal index1
        state.GetPosition(stream) |> Equal pos1
        stream.ReadFrom(state, false) |> Equal str
        checkStream true

    for i = 0 to N do
        for n = 0 to N + 15 - i do
            testRead i n

    let testReadException() =
        for i in [0; 1; N - 1; N] do
            seekStreamTo i

            let str2 = if i < N then refString.[i..] else ""
            try
                let str = stream.Read(System.Int32.MaxValue)
                str |> Equal str2
                seekStreamTo i
            with :? System.OutOfMemoryException -> ()
            try  stream.Read(-1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()

            try
                let str = stream.PeekString(System.Int32.MaxValue)
                str |> Equal str2
                stream.Index |> Equal (index0 + int64 i)
            with :? System.OutOfMemoryException -> ()
            try  stream.PeekString(-1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.PeekString(System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()

            let a = Array.create 3 '$'

            try  stream.Read(null, 0, 1) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try  stream.Read(a, 0, 4) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, 2, 2) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, 3, 1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, -1, 0) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, 0, -1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, 0, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, System.Int32.MinValue, 0) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(a, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()

        #if LOW_TRUST
        #else
            if i <> N then
                try  stream.Read(NativePtr.ofNativeInt 0n, 1) |> ignore; Fail()
                with :? System.NullReferenceException -> ()

            let handle = System.Runtime.InteropServices.GCHandle.Alloc([|'_'|], System.Runtime.InteropServices.GCHandleType.Pinned)
            let ptr = NativePtr.ofNativeInt (handle.AddrOfPinnedObject())
            let mutable c = '_'
            try  stream.Read(ptr, -1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try  stream.Read(ptr, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            handle.Free()
        #endif

            let mutable indexToken = CharStreamIndexToken()
            try stream.ReadFrom(indexToken) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            let mutable state = CharStreamState<_>()
            try stream.ReadFrom(&state, false) |> ignore; Fail()
            with :? System.ArgumentException -> ()

            if not stream.IsEndOfStream then
                stream.Skip()
            state <- stream.State
            indexToken <- stream.IndexToken
            stream.Skip(-1)

            try stream.ReadFrom(indexToken) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.ReadFrom(&state, false) |> ignore; Fail()
            with :? System.ArgumentException -> ()

    testReadException()

/// Cross verify the CharStream string wrapper version against the normal stream
/// version. This is done by generating a random string and then checking
/// randomly generated access sequences on CharStream instances with random parameters.
let xTest() =
    let rand = System.Random(43563456)

    let generateRandomUnicodeChars size =
        let cs = Array.zeroCreate size
        let mutable i = 0
        while i < cs.Length do
            let r = rand.Next()
            // see http://www.unicode.org/Public/UNIDATA/Blocks.txt
            if r &&& 0xffff < 0xfffe then
                if r < (System.Int32.MaxValue/3)*2 then
                    // generate a char from the BMP with about a prob. of 2/3
                    let c = r % 0xffff
                    if (c < 0xd800 || c > 0xdfff) then
                        cs.[i] <- char c
                        i <- i + 1
                else
                    let c_ = 0x10000 + (r % 0x25000)
                    let c = if c_ < 0x30000 then c_
                            else 0xe0000 ||| (c_ &&& 0xfff)
                    let v = c - 0x10000
                    let h = char (0xd800 ||| (c >>> 10))
                    let l = char (0xdc00 ||| (c &&& 0x3ff))
                    if i + 1 < cs.Length then
                        cs.[i]     <- h
                        cs.[i + 1] <- l
                        i <- i + 2
        cs

    let maxBlockSize = 100 // extremely small size for testing purposes only
    let maxReadSize = 120
    let maxJumpSize = 200

    let readBuffer = Array.create maxReadSize '_'


    let utf8 = System.Text.Encoding.GetEncoding(65001, System.Text.EncoderFallback.ReplacementFallback,
                                                                   new System.Text.DecoderReplacementFallback("XYZ"))

    // The handling of invalid input is buggy in the .NET decoder routines for UTF16 and GB18030,
    // so we can only use them for valid input (and we use the ExceptionFallback to verify that the input is valid).
    let utf16 = System.Text.Encoding.GetEncoding(1200, System.Text.EncoderFallback.ExceptionFallback,
                                                      System.Text.DecoderFallback.ExceptionFallback) // utf16 litte endian
    let gb18030 = System.Text.Encoding.GetEncoding(54936, System.Text.EncoderFallback.ExceptionFallback,
                                                          System.Text.DecoderFallback.ExceptionFallback)

    for j = 1 to 50 do
        let encoding, bytes, chars =
            match rand.Next()%4 with // 0 = utf8, 1 = utf16, 2-3 = gb18030
            | 0 ->
                let bytes = Array.zeroCreate (1 <<< 16)
                rand.NextBytes(bytes)
                let chars = utf8.GetChars(bytes)
                utf8, bytes, chars
            | r ->
                let encoding = utf16//if r = 1 then utf16 else gb18030
                let chars = generateRandomUnicodeChars (1 <<< 17)
                let bytes = encoding.GetBytes(chars)
                encoding, bytes, chars

    #if LOW_TRUST
        let chars = new string(chars)
    #endif

        use stringStream = new CharStream<unit>(chars, 0, chars.Length)

        let blockSize = 16 + rand.Next(maxBlockSize - 16)
        let maxCharsForOneByte = encoding.GetMaxCharCount(1)
        let blockOverlap = maxCharsForOneByte + rand.Next(blockSize/2 - maxCharsForOneByte)
        let byteBufferLength = 8 + rand.Next(maxBlockSize)
        let blockSizeMinusOverlap = blockSize - blockOverlap
        use charStream  = new CharStream<unit>(new System.IO.MemoryStream(bytes), false,
                                               encoding, true,
                                           #if LOW_TRUST
                                           #else
                                               blockSize, blockOverlap,
                                           #endif
                                               byteBufferLength)

        if j%10 = 1 then
            charStream.Seek(0L)
            stringStream.Seek(0L)

            for i = 1 to chars.Length do
                Equal (stringStream.Read()) (charStream.Read())
                if i%blockSizeMinusOverlap = blockOverlap && i >= blockSize then
                    Equal (stringStream.Peek(-blockSize)) (charStream.Peek(-blockSize))

        let mutable index = int32 (rand.Next(chars.Length))
        charStream.Seek(int64 index)
        stringStream.Seek(int64 index)
        // a random walk with jumps...
        let mutable i = 0;
        let mutable resetCounter = 0
        while i < 10000 do
                                                  // biased towards jumping backwards
            let jumpSize = rand.Next(maxJumpSize) * if rand.Next(4) = 0 then 1 else -1
            index <- index + jumpSize
            if 0 < index && index < chars.Length then
                charStream.Skip(jumpSize)
                stringStream.Skip(jumpSize)
            else
                resetCounter <- resetCounter + 1
                index <- rand.Next(chars.Length)
                charStream.Seek(int64 index)
                stringStream.Seek(int64 index)

            let mutable doContinue = true
            while doContinue do
               if i % 500 = 0 then
                   index <- rand.Next(chars.Length)
                   charStream.Seek(int64 index)
                   stringStream.Seek(int64 index)
                   charStream.PeekString(3) |> Equal (stringStream.PeekString(3))

               stringStream.Index |> Equal (int64 index)
               charStream.Index |> Equal (int64 index)
               match rand.Next(4) with
               | 0 -> let c = stringStream.Read()
                      if c <> EOS || not stringStream.IsEndOfStream then
                          charStream.Skip(c) |> True
                          index <- index + 1
                      elif index >= 7 then
                          charStream.Peek(-7) |> Equal (stringStream.Peek(-7))

               | 1 -> let n = 1 + rand.Next(maxReadSize)
                      let str = stringStream.Read(n)
                      charStream.Skip(str) |> True
                      index <- index + str.Length

               | 2 -> let n = 1 + rand.Next(maxReadSize)
                      let str = stringStream.Read(n)
                      let cfStr = FParsec.Text.FoldCase(str)
                      charStream.SkipCaseFolded(cfStr) |> True
                      index <- index + str.Length

               | _ -> let n = 1 + rand.Next(maxReadSize)
                      let str = charStream.Read(n)
                      stringStream.Skip(str) |> True
                      index <- index + str.Length

               doContinue <- rand.Next(4) <> 0 // jump every 4th iteration on average
               i <- i + 1

let testSkipWhitespace() =
    // check fast path

    let testChars = [|'\t'; '\n'; '\r'; ' '; '\u0008'; '\f'; '\u0021'; |]

    // returns with the stream state unchanged
    let checkSkipWhitespace (cs: char[]) iBegin (stream: CharStream<_>) =
        let mutable line = 1
        let mutable lineBegin = 0
        let mutable i = iBegin
        let mutable indentation = System.Int32.MinValue
        let mutable containsFormFeed = false
        let mutable fLine = line
        let mutable fLineBegin = lineBegin
        let mutable fI = i
        let mutable fIndentation = indentation

        let tabStopDistance = 8
        while i < cs.Length
              && (match cs.[i] with
                  | ' '  ->
                       indentation <- indentation + 1
                       true
                  | '\t' ->
                       indentation <- indentation + (tabStopDistance - indentation%tabStopDistance)
                       true
                  | '\r' ->
                      line <- line + 1; lineBegin <- i + 1;
                      indentation <- 0
                      true
                  | '\n' ->
                      if i = iBegin || cs.[i - 1] <> '\r' then
                          line <- line + 1
                      lineBegin <- i + 1
                      indentation <- 0
                      true
                  | '\f' ->
                      if not containsFormFeed then
                          containsFormFeed <- true
                          fLine <- line
                          fLineBegin <- lineBegin
                          fI <- i
                          fIndentation <- indentation
                      indentation <- 0
                      true
                  | _ ->
                      false)
           do i <- i + 1

        if not containsFormFeed then
            fLine <- line
            fLineBegin <- lineBegin
            fI <- i
            fIndentation <- indentation

        let state0 = stream.State
        let index0 = stream.Index
        let indexOffset = int32 index0 - iBegin

        let tag  = if  i <> iBegin then state0.Tag + _1 else state0.Tag
        let fTag = if fI <> iBegin then state0.Tag + _1 else state0.Tag
        let index  = int64 ( i + indexOffset)
        let fIndex = int64 (fI + indexOffset)
        let lineBegin  = if  line <> 1 then int64 ( lineBegin + indexOffset) else state0.LineBegin
        let fLineBegin = if fLine <> 1 then int64 (fLineBegin + indexOffset) else state0.LineBegin
        let line  = int64  line
        let fLine = int64 fLine

        try
            stream.SkipWhitespace() |> Equal (fI <> iBegin)
            stream.StateTag |> Equal fTag
            stream.Index |> Equal fIndex
            stream.Line |> Equal fLine
            stream.LineBegin |> Equal fLineBegin
            stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)
        with :? TestFailed ->
            stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)
            stream.SkipWhitespace() |> Equal (fI <> iBegin)

        stream.SkipUnicodeWhitespace() |> Equal (i <> iBegin)
        stream.StateTag |> Equal tag
        stream.Index |> Equal index
        stream.Line  |> Equal line
        stream.LineBegin |> Equal lineBegin
        stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)

        let mutable ind = -3
        if cs.[iBegin] = '\r' || cs.[iBegin] = '\n' then
            stream.SkipNewlineThenWhitespace(tabStopDistance, false) |> Equal fIndentation
            stream.StateTag |> Equal fTag
            stream.Index |> Equal fIndex
            stream.Line |> Equal fLine
            stream.LineBegin |> Equal fLineBegin
            stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)

            stream.SkipNewlineThenWhitespace(tabStopDistance, true) |> Equal indentation
            stream.StateTag |> Equal tag
            stream.Index |> Equal index
            stream.Line  |> Equal line
            stream.LineBegin |> Equal lineBegin
            stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)
        else
            stream.SkipNewlineThenWhitespace(tabStopDistance, true) |> Equal -1
            stream.StateTag |> Equal state0.Tag
            stream.Index |> Equal index0
            stream.Line  |> Equal state0.Line
            stream.LineBegin |> Equal state0.LineBegin
            stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)


    let testFastPath() =
        let cs = Array.create 11 '_'
    #if LOW_TRUST
    #else
        use stream = new CharStream<unit>(cs, 1, 10, 100L)
    #endif
        for c1 in testChars do
            cs.[1] <- c1
            for c2 in testChars do
                cs.[2] <- c2
                for c3 in testChars do
                    cs.[3] <- c3
                    for c4 in testChars do
                        cs.[4] <- c4
                        for c5 in testChars do
                            cs.[5] <- c5
                            for c6 in testChars do
                                cs.[6] <- c6
                                for c7 in testChars do
                                    cs.[7] <- c7
                                #if LOW_TRUST
                                    let stream = new CharStream<unit>(new string(cs), 1, 10, 100L)
                                #endif
                                    checkSkipWhitespace cs 1 stream

        // check end of block/stream handling
        for c7 in testChars do
            cs.[7] <- c7
            for c8 in testChars do
                cs.[8] <- c8
                for c9 in testChars do
                    cs.[9] <- c9
                    for c10 in testChars do
                        cs.[10] <- c10
                    #if LOW_TRUST
                        let stream = new CharStream<unit>(new string(cs), 1, 10, 100L)
                    #else
                        stream.Seek(stream.IndexOfFirstChar)
                    #endif
                        stream.Skip(6)
                        checkSkipWhitespace cs 7  stream
                        stream.Skip(1)
                        checkSkipWhitespace cs 8  stream
                        stream.Skip(1)
                        checkSkipWhitespace cs 9  stream
                        stream.Skip(1)
                        checkSkipWhitespace cs 10 stream

    #if LOW_TRUST
        let stream = new CharStream<unit>(new string(cs), 1, 10, 100L)
    #else
        stream.Seek(stream.IndexOfFirstChar)
    #endif
        stream.Skip(10)
        let tag = stream.StateTag
        stream.SkipWhitespace() |> False
        stream.StateTag |> Equal tag
        stream.Index |> Equal (stream.IndexOfFirstChar + 10L)
        stream.Line  |> Equal 1L
        stream.LineBegin  |> Equal stream.IndexOfFirstChar


    let testSlowPath() =
        let cs =  Array.create 17 '_'
        // check end of block handling with multi-block CharStream (blockSize = 8, blockOverlap = 3)
        for c6 in testChars do
            cs.[6] <- c6
            for c7 in testChars do
                cs.[7] <- c7
                for c8 in testChars do
                    cs.[8] <- c8
                    for c9 in testChars do
                        cs.[9] <- c9
                        for c10 in testChars do
                            cs.[10] <- c10
                            use stream = createMultiBlockUtf8TestStream cs
                            stream.Skip(6)
                            checkSkipWhitespace cs 6 stream // will start in the fast path
                            stream.Skip(1)
                            checkSkipWhitespace cs 7 stream // will start in the slow path

    testFastPath()
    testSlowPath()



let testSkipUnicodeWhitespace() =
    // We've already tested the the basic skipping logic in testSkipWhitespace.
    // Here we only test that the  additional newline chars are correctly recognized.

    let testChars = [|'\u0008'; '\t'; '\n'; '\u000B'; '\f'; '\r'; ' '; '\u0021'; '\u0085'; '\u200a'; '\u2028'; '\u2029'; '\u205f'|]

    let checkSkipUnicodeWhitespace (cs: char[]) iBegin (stream: CharStream<_>) =
        let mutable line = 1
        let mutable lineBegin = 0
        let mutable i = iBegin
        while i < cs.Length
              && (match cs.[i] with
                  | '\r' | '\u0085' | '\u2028' | '\u2029' ->
                      line <- line + 1; lineBegin <- i + 1; true
                  | '\n' ->
                      if i = iBegin || cs.[i - 1] <> '\r' then
                          line <- line + 1
                      lineBegin <- i + 1
                      true
                  | c ->
                      System.Char.IsWhiteSpace(c))
           do i <- i + 1

        let state0 = stream.State
        let index0 = stream.Index
        let indexOffset = int32 index0 - iBegin

        let tag  = if  i <> iBegin then state0.Tag + _1 else state0.Tag
        let index  = int64 (i + indexOffset)
        let lineBegin  = if line <> 1 then int64 (lineBegin + indexOffset) else state0.LineBegin
        let line  = int64 line + (state0.Line - 1L)

        stream.SkipUnicodeWhitespace() |> Equal (i <> iBegin)
        stream.StateTag |> Equal tag
        stream.Index |> Equal index
        stream.Line  |> Equal line
        stream.LineBegin |> Equal lineBegin
        stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)

        let isNewline c = match c with
                          |'\n' | '\r' | '\u0085' | '\u2028' | '\u2029' -> true
                          | _ -> false

        if iBegin + 2 >= i then
            if iBegin < cs.Length && isNewline cs.[iBegin] then
                let index = if cs.[iBegin] = '\r' && iBegin + 1 < cs.Length && cs.[iBegin + 1] = '\n' then
                                index0 + 2L
                            else
                                index0 + 1L
                stream.SkipUnicodeNewline() |> True
                stream.Index |> Equal index
                stream.StateTag |> Equal tag
                stream.Line |> Equal (state0.Line + 1L)
                stream.LineBegin |> Equal index
                stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)
            else
                stream.SkipUnicodeNewline() |> False
                stream.Index |> Equal index0
                stream.StateTag |> Equal state0.Tag
                stream.Line |> Equal state0.Line
                stream.LineBegin |> Equal state0.LineBegin

    let testFastPath() =
        let cs = Array.create 11 '_'
    #if LOW_TRUST
    #else
        use stream = new CharStream<unit>(cs, 1, 10, 100L)
    #endif
        for c1 in testChars do
            cs.[1] <- c1
            for c2 in testChars do
                cs.[2] <- c2
                for c3 in testChars do
                    cs.[3] <- c3
                    for c4 in testChars do
                        cs.[4] <- c4
                        for c5 in testChars do
                            cs.[5] <- c5
                        #if LOW_TRUST
                            let stream = new CharStream<unit>(new string(cs), 1, 10, 100L)
                        #endif
                            checkSkipUnicodeWhitespace cs 1 stream

    let testSlowPath() =
        let cs =  Array.create 17 '_'
        for c7 in testChars do
            cs.[7] <- c7
            for c8 in testChars do
                cs.[8] <- c8
                for c9 in testChars do
                    cs.[9] <- c9
                    for c10 in testChars do
                        cs.[10] <- c10
                        use stream = createMultiBlockUtf8TestStream cs
                        stream.Skip(7)
                        checkSkipUnicodeWhitespace cs 7 stream

    testFastPath()
    testSlowPath()


let testSkipNewlineWhitespace() =
    // Most of the testing for SkipNewlineWhitespace is done in testSkipWhitespace,
    // here we mostly test the error handling.

    let testArgumentChecking() =
        use stream = new CharStream<unit>("\r\n  ")
        let state0 = stream.State
        let mutable indentation = 0
        try stream.SkipNewlineThenWhitespace(0, true) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()
        try stream.SkipNewlineThenWhitespace(-1, true) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()
        try stream.SkipNewlineThenWhitespace(-8, true) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()
        try stream.SkipNewlineThenWhitespace(System.Int32.MinValue, true) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()
        try stream.SkipNewlineThenWhitespace(7, true) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()

    let testIndentationComputation() =
        // some additional checks complementing those in testSkipWhitespace
        use stream = new CharStream<unit>("\r\n\t \t  \t   \t ")
        let state0 = stream.State
        stream.SkipNewlineThenWhitespace(1, true) |> Equal 11
        stream.BacktrackTo(state0)
        stream.SkipNewlineThenWhitespace(2, true) |> Equal 13
        stream.BacktrackTo(state0)
        stream.SkipNewlineThenWhitespace(4, true) |> Equal 17

    let testIndentationOverflowHandling() =
        let mutable indentation = 0

        let test (str: string) (tabStopDistance: int) (index: int) (indentation: int) =
            use stream = new CharStream<unit>(str)

            stream.SkipNewlineThenWhitespace(tabStopDistance, true) |> Equal indentation
            stream.Index |> Equal (int64 index)

        test "\r\n\t" (1 <<< 30) 3  (1 <<< 30)
        test "\r\n\t " (1 <<< 30) 4 ((1 <<< 30) + 1)
        test "\r\n\t\t" (1 <<< 30) 3  (1 <<< 30)
        test "\r\n\t \t" (1 <<< 30) 4 ((1 <<< 30) + 1)
        test "\r\n\t\t " (1 <<< 30) 3  (1 <<< 30)
        test "\r\n\t \t " (1 <<< 30) 4 ((1 <<< 30) + 1)

        let cs = Array.zeroCreate (1 + (1 <<< 15) + (1 <<< 16)) // 1 + 2^15 + 2^16
        cs.[0] <- '\n'
        // (2^15 - 1)*2^16 + 2^16 = 2^31
        for i = 1 to (1 <<< 15) - 1 do
            cs.[i] <- '\t'
        for i = (1 <<< 15) to ((1 <<< 15)) + (1 <<< 16) do
            cs.[i] <- ' '

        use stream = new CharStream<unit>(new string(cs))
        stream.SkipNewlineThenWhitespace((1 <<< 16), true) |> Equal System.Int32.MaxValue
        stream.Index |> Equal (int64 (cs.Length - 2))
        stream.Read(3) |> Equal "  "

        // test slow path
        let e = System.Text.Encoding.Unicode
        let bs = e.GetBytes(cs)
        use stream = new CharStream<unit>(new System.IO.MemoryStream(bs, false), false, e, false,
                                                                     #if LOW_TRUST
                                                                     #else
                                                                         6144, 2048,
                                                                     #endif
                                                                         2048)
        stream.SkipNewlineThenWhitespace((1 <<< 16), true) |> Equal System.Int32.MaxValue
        stream.Index |> Equal (int64 (cs.Length - 2))
        stream.Read(3) |> Equal "  "

    testArgumentChecking()
    testIndentationComputation()
    testIndentationOverflowHandling()

let testSkipRestOfLine() =
    let testChars = [|'\n'; '\r'; '\t'; '\u000C'; '\u000E'|]

    let checkSkipRestOfLine (cs: char[]) iBegin (stream: CharStream<_>) =
        let state0 = stream.State

        let indexOffset = stream.Index - int64 iBegin
        let mutable i = iBegin
        while i < cs.Length  && (cs.[i] <> '\r' && cs.[i] <> '\n') do i <- i + 1

        let index = indexOffset + int64 i
        let tag = if i <> iBegin then state0.Tag + _1 else state0.Tag
        let str =  new string(cs, iBegin, i - iBegin)

        stream.SkipRestOfLine(false)
        stream.Index |> Equal index
        stream.StateTag |> Equal tag
        stream.Line |> Equal state0.Line
        stream.LineBegin |> Equal state0.LineBegin
        stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)

        stream.ReadRestOfLine(false) |> Equal str
        stream.Index |> Equal index
        stream.StateTag |> Equal tag
        stream.Line |> Equal state0.Line
        stream.LineBegin |> Equal state0.LineBegin
        stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)

        let mutable line2 = state0.Line
        if i < cs.Length then
            let c = cs.[i]
            if c = '\r' || c = '\n' then
                i <- i + if c = '\r' && i + 1 < cs.Length && cs.[i + 1] = '\n' then 2 else 1
                line2 <- line2 + 1L

        let index2 = indexOffset + int64 i
        let tag2 = if i <> iBegin then state0.Tag + _1 else state0.Tag
        let lineBegin2 = if line2 <> state0.Line then index2 else state0.LineBegin

        try
            stream.SkipRestOfLine(true)
            stream.Index |> Equal index2
            stream.StateTag |> Equal tag2
            stream.Line |> Equal line2
            stream.LineBegin |> Equal lineBegin2
            stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)
        with TestFailed str ->
            stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)
            stream.SkipRestOfLine(true)
            stream.Index |> Equal index2

        stream.ReadRestOfLine(true) |> Equal str
        stream.Index |> Equal index2
        stream.StateTag |> Equal tag2
        stream.Line |> Equal line2
        stream.LineBegin |> Equal lineBegin2
        stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)

    let testFastPath() =
        let cs = Array.create 8 '_'
    #if LOW_TRUST
    #else
        use stream = new CharStream<unit>(cs, 1, 7, 100L)
    #endif
        for c1 in testChars do
            cs.[1] <- c1
            for c2 in testChars do
                cs.[2] <- c2
                for c3 in testChars do
                    cs.[3] <- c3
                    for c4 in testChars do
                        cs.[4] <- c4
                        for c5 in testChars do
                            cs.[5] <- c5
                        #if LOW_TRUST
                            use stream = new CharStream<unit>(new string(cs), 1, 7, 100L)
                        #endif
                            checkSkipRestOfLine cs 1 stream

        // check end of block/stream handling
        for c5 in testChars do
            cs.[5] <- c5
            for c6 in testChars do
                cs.[6] <- c6
                for c7 in testChars do
                    cs.[7] <- c7
                #if LOW_TRUST
                    use stream = new CharStream<unit>(new string(cs), 1, 7, 100L)
                #else
                    stream.Seek(stream.IndexOfFirstChar)
                #endif
                    stream.Skip(4)
                    checkSkipRestOfLine cs 5 stream
                    stream.Skip(1)
                    checkSkipRestOfLine cs 6 stream
                    stream.Skip(1)
                    checkSkipRestOfLine cs 7 stream

    #if LOW_TRUST
        use stream = new CharStream<unit>(new string(cs), 1, 7, 100L)
    #else
        stream.Seek(stream.IndexOfFirstChar)
    #endif
        stream.Skip(7)
        checkSkipRestOfLine cs 8 stream

    let testSlowPath() =
        let cs =  Array.create 17 '_'
        // check end of block handling with multi-block CharStream (blockSize = 8, blockOverlap = 3)
        for c5 in testChars do
            cs.[5] <- c5
            for c6 in testChars do
                cs.[6] <- c6
                for c7 in testChars do
                    cs.[7] <- c7
                    for c8 in testChars do
                        cs.[8] <- c8
                        use stream = createMultiBlockUtf8TestStream cs
                        let s5 = stream.Skip(5)
                        checkSkipRestOfLine cs 5 stream // will start in the fast path
                        stream.Skip()
                        checkSkipRestOfLine cs 6 stream // will start in the slow path

    testFastPath()
    testSlowPath()

let testSkipCharsOrNewlines() =
    let counter = ref 0

    let check (stream: CharStream<_>) (cs: char[]) iBegin nMax =
        let state0 = stream.State
        let tag0 = state0.Tag
        let line0 = state0.Line
        let lineBegin0 = state0.LineBegin
        let index0 = stream.Index
        let indexOffset = int32 index0 - iBegin
        let lineOffset = int32 state0.Line - 1
        let alwaysTrue  = fun (c: char) -> true
        let alwaysFalse = fun (c: char) -> false
        let nTrueN = ref 0
        let nTrue = fun (c: char) -> if !nTrueN > 0 then decr nTrueN; true else false
        for n = 0 to nMax do
            incr counter
            let mutable line = 1
            let mutable lineBegin = 0
            let mutable i = iBegin
            let mutable c = 0
            while c < n && i < cs.Length do
                match cs.[i] with
                | '\r' | '\n' ->
                    i <- i + if cs.[i] = '\r' && i + 1 < cs.Length && cs.[i + 1] = '\n' then 2 else 1
                    line <- line + 1
                    lineBegin <- i
                | _ -> i <- i + 1
                c <- c + 1

            let consumed = c <> 0
            let tag = if consumed then tag0 + _1 else tag0
            let index = int64 (i + indexOffset)
            let containsNewline = line <> 1
            let lineBegin = if containsNewline then int64 (lineBegin + indexOffset) else lineBegin0
            let line = int64 (line + lineOffset)

            let checkStreamAndReset() = // this function needs to be fast
                stream.Index |> Equal index
                stream.StateTag |> Equal tag
                stream.Line |> Equal line
                stream.LineBegin |> Equal lineBegin
                stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)

            let str = new string(cs, iBegin, i - iBegin)
            let normalizedStr = FParsec.Text.NormalizeNewlines(str)

            if n = 1 then
                stream.ReadCharOrNewline() |> Equal (if c = 0 then EOS else normalizedStr.[0])
                checkStreamAndReset()

                stream.SkipNewline() |> Equal containsNewline
                if containsNewline then
                    checkStreamAndReset()
                else
                    stream.Index |> Equal index0
                    stream.StateTag |> Equal tag0
                    stream.Line |> Equal line0
                    stream.LineBegin |> Equal lineBegin0

                stream.SkipUnicodeNewline() |> Equal containsNewline
                if containsNewline then
                    checkStreamAndReset()
                else
                    stream.Index |> Equal index0
                    stream.StateTag |> Equal tag0
                    stream.Line |> Equal line0
                    stream.LineBegin |> Equal lineBegin0

                stream.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysFalse) |> Equal c
                checkStreamAndReset()
                stream.ReadCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, false) |> Equal str
                checkStreamAndReset()
                stream.ReadCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, true) |> Equal normalizedStr
                checkStreamAndReset()

                stream.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, 0, System.Int32.MaxValue) |> Equal c
                checkStreamAndReset()
                stream.ReadCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, 0, System.Int32.MaxValue, false) |> Equal str
                checkStreamAndReset()
                stream.ReadCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, 0, System.Int32.MaxValue, true) |> Equal normalizedStr
                checkStreamAndReset()

            stream.SkipCharsOrNewlines(n) |> Equal c
            checkStreamAndReset()
            stream.ReadCharsOrNewlines(n, false) |> Equal str
            stream.ReadFrom(state0, false) |> Equal str
            checkStreamAndReset()
            stream.ReadCharsOrNewlines(n, true) |> Equal normalizedStr
            stream.ReadFrom(state0, true) |> Equal normalizedStr
            checkStreamAndReset()

            try
                nTrueN:= n
                stream.SkipCharsOrNewlinesWhile(nTrue) |> Equal c
                checkStreamAndReset()
            with TestFailed _ ->
                stream.Seek(stream.IndexOfFirstChar); stream.BacktrackTo(state0)
                nTrueN:= n
                stream.SkipCharsOrNewlinesWhile(nTrue) |> Equal c

            nTrueN:= n
            stream.ReadCharsOrNewlinesWhile(nTrue, false) |> Equal str
            checkStreamAndReset()
            nTrueN:= n
            stream.ReadCharsOrNewlinesWhile(nTrue, true) |> Equal normalizedStr
            checkStreamAndReset()

            nTrueN:= n
            stream.SkipCharsOrNewlinesWhile(nTrue, 0, System.Int32.MaxValue) |> Equal c
            checkStreamAndReset()
            nTrueN:= n
            stream.ReadCharsOrNewlinesWhile(nTrue, 0, System.Int32.MaxValue, false) |> Equal str
            checkStreamAndReset()
            nTrueN:= n
            stream.ReadCharsOrNewlinesWhile(nTrue, 0, System.Int32.MaxValue, true) |> Equal normalizedStr
            checkStreamAndReset()

            nTrueN:= n
            stream.SkipCharsOrNewlinesWhile(alwaysTrue, 0, n) |> Equal c
            checkStreamAndReset()
            nTrueN:= n
            stream.ReadCharsOrNewlinesWhile(alwaysTrue, 0, n, false) |> Equal str
            checkStreamAndReset()
            nTrueN:= n
            stream.ReadCharsOrNewlinesWhile(alwaysTrue, 0, n, true) |> Equal normalizedStr
            checkStreamAndReset()

            let mutable foundString = false;
            stream.SkipCharsOrNewlinesUntilString("\u0000", n, &foundString) |> Equal c // the stream contains no '\u0000'
            foundString |> Equal false
            checkStreamAndReset()

            stream.SkipCharsOrNewlinesUntilCaseFoldedString("\u0000", n, &foundString) |> Equal c
            foundString |> Equal false
            checkStreamAndReset()

            let mutable str2 = null : string
            stream.SkipCharsOrNewlinesUntilString("\u0000", n, false, &str2) |> Equal c
            str2 |> IsNull
            checkStreamAndReset()

            stream.SkipCharsOrNewlinesUntilString("\u0000", n, true, &str2) |> Equal c
            str2 |> IsNull
            checkStreamAndReset()

            stream.SkipCharsOrNewlinesUntilCaseFoldedString("\u0000", n, false, &str2) |> Equal c
            str2 |> IsNull
            checkStreamAndReset()

            stream.SkipCharsOrNewlinesUntilCaseFoldedString("\u0000", n, true, &str2) |> Equal c
            str2 |> IsNull
            checkStreamAndReset()

            if i < cs.Length && str.IndexOf(cs.[i]) = -1 then
                let cis = string cs.[i]

                stream.SkipCharsOrNewlinesUntilString(cis, n, &foundString) |> Equal c // the stream contains no '\u0000'
                foundString |> Equal true
                checkStreamAndReset()

                stream.SkipCharsOrNewlinesUntilCaseFoldedString(cis, n, &foundString) |> Equal c
                foundString |> Equal true
                checkStreamAndReset()

                let mutable str2 = null : string
                stream.SkipCharsOrNewlinesUntilString(cis, n, false, &str2) |> Equal c
                str2 |> Equal str
                checkStreamAndReset()

                stream.SkipCharsOrNewlinesUntilString(cis, n, true, &str2) |> Equal c
                str2 |> Equal normalizedStr
                checkStreamAndReset()

                stream.SkipCharsOrNewlinesUntilCaseFoldedString(cis, n, false, &str2) |> Equal c
                str2 |> Equal str
                checkStreamAndReset()

                stream.SkipCharsOrNewlinesUntilCaseFoldedString(cis, n, true, &str2) |> Equal c
                str2 |> Equal normalizedStr
                checkStreamAndReset()

    let testChars = [|'\n'; '\r'; '\t'; '\u000C'; '\u000E'|]

    let testFastPath() =
        let cs = Array.create 11 '_'
    #if LOW_TRUST
    #else
        use stream = new CharStream<unit>(cs, 1, 10, 100L)
    #endif
        for c1 in testChars do
            cs.[1] <- c1
            for c2 in testChars do
                cs.[2] <- c2
                for c3 in testChars do
                    cs.[3] <- c3
                    for c4 in testChars do
                        cs.[4] <- c4
                        for c5 in testChars do
                            cs.[5] <- c5
                            for c6 in testChars do
                                cs.[6] <- c6
                                for c7 in testChars do
                                    cs.[7] <- c7
                                #if LOW_TRUST
                                    use stream = new CharStream<unit>(new string(cs), 1, 10, 100L)
                                #endif
                                    check stream cs 1 7

        // check end of block/stream handling
        for c7 in testChars do
            cs.[7] <- c7
            for c8 in testChars do
                cs.[8] <- c8
                for c9 in testChars do
                    cs.[9] <- c9
                    for c10 in testChars do
                        cs.[10] <- c10
                    #if LOW_TRUST
                        use stream = new CharStream<unit>(new string(cs), 1, 10, 100L)
                    #else
                        stream.Seek(stream.IndexOfFirstChar)
                    #endif
                        stream.Skip(6)
                        check stream cs  7 5
                        stream.Skip()
                        check stream cs  8 4
                        stream.Skip()
                        check stream cs  9 3
                        stream.Skip()
                        check stream cs 10 2

    #if LOW_TRUST
        use stream = new CharStream<unit>(new string(cs), 1, 10, 100L)
    #else
        stream.Seek(stream.IndexOfFirstChar)
    #endif
        stream.Skip(10)
        check stream cs 11 1

    let testSlowPath() =
        let cs =  Array.create 17 '_'
        // check end of block handling with multi-block CharStream (blockSize = 8, blockOverlap = 3)
        for c6 in testChars do
            cs.[6] <- c6
            for c7 in testChars do
                cs.[7] <- c7
                for c8 in testChars do
                    cs.[8] <- c8
                    for c9 in testChars do
                        cs.[9] <- c9
                        for c10 in testChars do
                            cs.[10] <- c10
                            use stream = createMultiBlockUtf8TestStream cs
                            stream.Skip(6)
                            check stream cs 6 4
                            stream.Skip()
                            check stream cs 7 4
                            stream.Skip()
                            check stream cs 8 4

    let testArgumentChecking() =
        let N = 10
        let cs =  Array.create N '_'
        let css = new string(cs)
        use stream = new CharStream<unit>(css, 0, cs.Length)

        let alwaysTrue = fun c -> true

        for i in [0; 1; N - 1; N] do
            let str = if i < N then css.[i..] else ""
            let n = N - i
            stream.Seek(int64 i)
            stream.SkipCharsOrNewlines(System.Int32.MaxValue) |> Equal n
            stream.Index |> Equal (int64 N)

            stream.Seek(int64 i)
            stream.ReadCharsOrNewlines(System.Int32.MaxValue, true) |> Equal str
            stream.Index |> Equal (int64 N)

            stream.Seek(int64 i)
            try stream.SkipCharsOrNewlines(-1) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlines(System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.ReadCharsOrNewlines(-1, true) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.ReadCharsOrNewlines(System.Int32.MinValue, true) |> ignore; Fail()
            with :? System.ArgumentException -> ()

            stream.Seek(int64 i)
            stream.SkipCharsOrNewlinesWhile(alwaysTrue, -1, System.Int32.MaxValue) |> Equal n
            stream.Index |> Equal (int64 N)

            stream.Seek(int64 i)
            stream.ReadCharsOrNewlinesWhile(alwaysTrue, -1, System.Int32.MaxValue, true) |> Equal str
            stream.Index |> Equal (int64 N)

            stream.Seek(int64 i)
            try stream.SkipCharsOrNewlinesWhile(alwaysTrue, -1, -1) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesWhile(alwaysTrue, -1, System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.ReadCharsOrNewlinesWhile(alwaysTrue, -1, -1, true) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.ReadCharsOrNewlinesWhile(alwaysTrue, -1, System.Int32.MinValue, true) |> ignore; Fail()
            with :? System.ArgumentException -> ()

            let mutable found = false
            let mutable str = null
            try stream.SkipCharsOrNewlinesUntilString(null, 10, &found) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try stream.SkipCharsOrNewlinesUntilString(null, 10, false, &str) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try stream.SkipCharsOrNewlinesUntilString("", 10, &found) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilString("", 10, false, &str) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilString("_", -1, &found) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilString("_", -1, false, &str) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilString("_", System.Int32.MinValue, &found) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilString("_", System.Int32.MinValue, false, &str) |> ignore; Fail()
            with :? System.ArgumentException -> ()

            try stream.SkipCharsOrNewlinesUntilCaseFoldedString(null, 10, &found) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try stream.SkipCharsOrNewlinesUntilCaseFoldedString(null, 10, false, &str) |> ignore; Fail()
            with :? System.NullReferenceException -> ()
            try stream.SkipCharsOrNewlinesUntilCaseFoldedString("", 10, &found) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilCaseFoldedString("", 10, false, &str) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilCaseFoldedString("_", -1, &found) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilCaseFoldedString("_", -1, false, &str) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilCaseFoldedString("_", System.Int32.MinValue, &found) |> ignore; Fail()
            with :? System.ArgumentException -> ()
            try stream.SkipCharsOrNewlinesUntilCaseFoldedString("_", System.Int32.MinValue, false, &str) |> ignore; Fail()
            with :? System.ArgumentException -> ()


    let SkipCharsOrNewlinesWhileMinChars() =
        let cs = "0123456789"
        use stream = new CharStream<unit>(cs, 0, cs.Length)

        let smaller n = fun c -> int c < int '0' + n
        for n = 0 to 10 do
            stream.SkipCharsOrNewlinesWhile(smaller n, n, System.Int32.MaxValue) |> Equal n
            stream.Index |> Equal (int64 n)
            stream.Seek(0L)

            stream.ReadCharsOrNewlinesWhile(smaller n, n, System.Int32.MaxValue, true) |> ignore
            stream.Index |> Equal (int64 n)
            stream.Seek(0L)

            let tag = stream.StateTag
            stream.SkipCharsOrNewlinesWhile(smaller n, n + 1, System.Int32.MaxValue) |> Equal 0
            stream.Index |> Equal 0L
            stream.StateTag |> Equal tag

            stream.ReadCharsOrNewlinesWhile(smaller n, n + 1, System.Int32.MaxValue, true) |> ignore
            stream.Index |> Equal 0L
            stream.StateTag |> Equal tag

            stream.SkipCharsOrNewlinesWhile((fun c -> true), n + 1, n) |> Equal 0
            stream.Index |> Equal 0L
            stream.StateTag |> Equal tag

            stream.ReadCharsOrNewlinesWhile((fun c -> true), n + 1, n, true) |> ignore
            stream.Index |> Equal 0L
            stream.StateTag |> Equal tag

            stream.SkipCharsOrNewlinesWhile((fun c -> true), System.Int32.MaxValue, n) |> Equal 0
            stream.Index |> Equal 0L
            stream.StateTag |> Equal tag

            stream.ReadCharsOrNewlinesWhile((fun c -> true), System.Int32.MaxValue, n, true) |> ignore
            stream.Index |> Equal 0L
            stream.StateTag |> Equal tag


    testFastPath()
    testSlowPath()
    testArgumentChecking()
    SkipCharsOrNewlinesWhileMinChars()


let SkipCharsOrNewlinesUntilString() =
    // most of the testing has already been done in testSkipCharsOrNewlines
    let cs = "ABCDEFGHI\tJKLMNOPQRST".ToCharArray()
    use stream = createMultiBlockUtf8TestStream cs // blockSize = 8, blockOverlap = 3
    for i0 = 0 to cs.Length - 1 do
        stream.Seek(0L); stream.Seek(int64 i0)
        for i1 = i0 to cs.Length - 1 do
           for n = 1 to cs.Length - i1 do

               let check strToFind maxChars isPresent =
                   let iEnd = if isPresent then i1
                              elif maxChars < cs.Length - i0 then i0 + maxChars
                              else cs.Length

                   let skippedString = if not isPresent then null
                                       else new string(cs.[i0..i1 - 1])

                   let state0 = stream.State
                   let mutable found = false
                   stream.SkipCharsOrNewlinesUntilString(strToFind, maxChars, &found) |> Equal (iEnd - i0)
                   stream.Index |> Equal (int64 iEnd)
                   found |> Equal isPresent
                   stream.Skip(0L); stream.BacktrackTo(state0)

                   let mutable str = null
                   stream.SkipCharsOrNewlinesUntilString(strToFind, maxChars, false, &str) |> Equal (iEnd - i0)
                   stream.Index |> Equal (int64 iEnd)
                   str |> Equal skippedString
                   stream.Skip(0L); stream.BacktrackTo(state0)

                   let strToFindCI = FParsec.Text.FoldCase(strToFind)

                   stream.SkipCharsOrNewlinesUntilCaseFoldedString(strToFindCI, maxChars, &found) |> Equal (iEnd - i0)
                   stream.Index |> Equal (int64 iEnd)
                   found |> Equal isPresent
                   stream.Skip(0L); stream.BacktrackTo(state0)

                   stream.SkipCharsOrNewlinesUntilCaseFoldedString(strToFindCI, maxChars, false, &str) |> Equal (iEnd - i0)
                   stream.Index |> Equal (int64 iEnd)
                   str |> Equal skippedString
                   stream.Skip(0L); stream.BacktrackTo(state0)

               let strToFind = new string(cs, i1, n)
               check strToFind System.Int32.MaxValue true
               check strToFind (i1 - i0) true
               if i1 - i0 > 0 then
                   check strToFind (i1 - i0 - 1) false
               if n > 1 then
                   let strToNotFind = string (char (int strToFind.[0] + 1)) + (if n > 1 then strToFind.Substring(1) else "")
                   check strToNotFind System.Int32.MaxValue false
                   let strToNotFind2 = strToFind.Substring(0, n - 1) + string (char (int strToFind.[n - 1] + 1))
                   check strToNotFind2 System.Int32.MaxValue false


let testCreateSubstream() =

    let test (stream: CharStream<_>) =
        let off = stream.IndexOfFirstChar
        let state0 = stream.State
        stream.ReadCharsOrNewlines(3, false) |> Equal " \r\n0"
        stream.Index |> Equal (off + 4L)
        stream.Line |> Equal 2L
        stream.LineBegin |> Equal (off + 3L)
        let state1 = stream.State
        stream.ReadCharsOrNewlines(5, false) |> Equal "1\n345"
        use substream = stream.CreateSubstream(state1)
        stream.BacktrackTo(state0)
        let substreamState0 = substream.State
        substream.IndexOfFirstChar |> Equal (off + 4L)
        substream.IndexOfLastCharPlus1 |> Equal (off + 9L)
        substream.Line |> Equal 2L
        substream.LineBegin |> Equal(off + 3L)
        substream.ReadCharsOrNewlines(10, false) |> Equal "1\n345"
        substream.Index |> Equal (off + 9L)
        substream.IsEndOfStream |> True
        substream.Line |> Equal 3L
        substream.LineBegin |> Equal (off + 6L)
        stream.ReadCharsOrNewlines(10, false) |> Equal " \r\n01\n3456"
        stream.Index |> Equal (off + 10L)
        stream.Line |> Equal 3L
        substream.BacktrackTo(substreamState0)
        substream.Read() |> Equal '1'

        let substreamState1 = substream.State
        substream.ReadCharsOrNewlines(3, false) |> Equal "\n34"
        use subSubstream = substream.CreateSubstream(substreamState1)
        subSubstream.IndexOfFirstChar |> Equal (off + 5L)
        subSubstream.IndexOfLastCharPlus1 |> Equal (off + 8L)
        subSubstream.Line |> Equal 2L
        subSubstream.LineBegin |> Equal (off + 3L)
        subSubstream.ReadCharsOrNewlines(10, false) |> Equal "\n34"
        subSubstream.Index |> Equal (off + 8L)
        subSubstream.IsEndOfStream |> True
        subSubstream.Line |> Equal 3L
        subSubstream.LineBegin |> Equal (off + 6L)

        substream.BacktrackTo(substreamState1)
        use subSubstream2 = substream.CreateSubstream(substreamState1)
        subSubstream2.IndexOfFirstChar |> Equal (off + 5L)
        subSubstream2.IndexOfLastCharPlus1 |> Equal (off + 5L)
        subSubstream2.Line |> Equal 2L
        subSubstream2.LineBegin |> Equal (off + 3L)
        subSubstream2.IsEndOfStream |> True
        substream.ReadCharsOrNewlines(10, false) |> Equal "\n345"
        substream.IsEndOfStream |> True

        let substreamStateEnd = substream.State
        use subSubstream3 = substream.CreateSubstream(substreamStateEnd)
        subSubstream3.IndexOfFirstChar |> Equal (off + 9L)
        subSubstream3.IndexOfLastCharPlus1 |> Equal (off + 9L)
        subSubstream3.Line |> Equal 3L
        subSubstream3.LineBegin |> Equal (off + 6L)
        subSubstream3.IsEndOfStream |> True

        substream.BacktrackTo(substreamState0)
        try substream.CreateSubstream(substreamState1) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try substream.CreateSubstream(substreamStateEnd) |> ignore; Fail()
        with :? System.ArgumentException -> ()


        stream.Skip(-1)
        let state2 = stream.State
        stream.Read() |> Equal '6'
        stream.IsEndOfStream |> True

        use substream2 = stream.CreateSubstream(state2)
        substream2.IndexOfFirstChar |> Equal (off + 9L)
        substream2.IndexOfLastCharPlus1 |> Equal (off + 10L)
        substream2.Index |> Equal(off + 9L)
        substream2.Line |> Equal 3L
        substream2.LineBegin |> Equal (off + 6L)
        substream2.Read() |> Equal '6'
        substream2.IsEndOfStream |> True

        let stateEnd = stream.State
        use substream3 = stream.CreateSubstream(stateEnd)
        substream3.IndexOfFirstChar |> Equal (off + 10L)
        substream3.IndexOfLastCharPlus1 |> Equal (off + 10L)
        substream3.Index |> Equal(off + 10L)
        substream3.Line |> Equal 3L
        substream3.LineBegin |> Equal (off + 6L)
        substream3.IsEndOfStream |> True

        stream.BacktrackTo(state1)
        use substream4 = stream.CreateSubstream(state1)
        substream4.IndexOfFirstChar |> Equal (off + 4L)
        substream4.IndexOfLastCharPlus1 |> Equal (off + 4L)
        substream4.Index |> Equal (off + 4L)
        substream4.Line |> Equal 2L
        substream4.LineBegin |> Equal (off + 3L)
        substream4.IsEndOfStream |> True

        stream.BacktrackTo(state0)
        try stream.CreateSubstream(state1) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try stream.CreateSubstream(stateEnd) |> ignore; Fail()
        with :? System.ArgumentException -> ()

    let str = " \r\n01\n3456"
    use stream1 = new CharStream<unit>("!" + str + "!", 1, str.Length, 100L)
    test stream1

#if LOW_TRUST
#else
  #if DEBUG
    let state = stream1.State
    use substream = stream1.CreateSubstream(state)
    try stream1.Dispose(); Fail()
    with :? System.InvalidOperationException -> ()
    substream.Read() |> ignore
  #endif
#endif

#if LOW_TRUST
#else
    use stream2 = new CharStream<unit>(("!" + str + "!").ToCharArray(), 1, str.Length, 100L)
    test stream2
#endif

    use stream3 = createMultiBlockUtf8TestStream (str.ToCharArray()) // blockSize = 8, blockOverlap = 3
    test stream3

let testTwoChars() =
    let cs = new TwoChars('\u0001', '\u0002')
    cs.Char0 |> Equal '\u0001'
    cs.Char1 |> Equal '\u0002'
    cs.Equals(TwoChars('\u0001', '\u0002')) |> True
    (box cs).Equals(TwoChars('\u0001', '\u0002')) |> True
    TwoChars.op_Equality(cs, TwoChars((2u <<< 16) ||| 1u)) |> True
    TwoChars.op_Inequality(cs, TwoChars('\u0001', '\u0002')) |> False
    cs.GetHashCode() |> Equal ((2 <<< 16) ||| 1)
    let cs2 = new TwoChars('\uffff', '\uffff')
    cs2.Char0 |> Equal '\uffff'
    cs2.Char1 |> Equal '\uffff'


let run() =
#if LOW_TRUST
#else
    setStaticField typeof<FParsec.CharStream> "MinimumByteBufferLength" 10
    setStaticField typeof<FParsec.CharStream> "DoNotRoundUpBlockSizeToSimplifyTesting" true
#endif

    testNonStreamConstructors()
    testStreamConstructorArgumentChecking()
    testEncodingDetection()

#if LOW_TRUST
#else
    testNonSeekableCharStreamHandling()
#endif
    testDecoderFallbackExceptionHandling()

    let testStreams() =
        let refString = "1234567890ABCDEF"
        use stringStream = new CharStream<int>(" " + refString, 1, refString.Length, 100L)
        testBasicCharStreamMethods stringStream refString refString.Length 0 0

        let be = new System.Text.UTF32Encoding(true, true)
        let bs = Array.append (be.GetPreamble()) (be.GetBytes(refString))
        use fileStream = createMultiBlockTestStream (new System.IO.MemoryStream(bs, false)) System.Text.Encoding.Unicode
        fileStream.MinRegexSpace <- 3
        testBasicCharStreamMethods fileStream refString 8 3 3

        let refString2 = "1234567890ABCDEFGH" // exactly three blocks + 1 overlap
        let bs2 = System.Text.Encoding.Unicode.GetBytes(refString2)
        use fileStream = createMultiBlockTestStream (new System.IO.MemoryStream(bs2, false)) System.Text.Encoding.Unicode
        fileStream.MinRegexSpace <- 3
        testBasicCharStreamMethods fileStream refString2 8 3 3

        use emptyStringStream = new CharStream<unit>("x", 1, 0, 1000L)
        testEmptyStream emptyStringStream

        use emptyStringStream2 = new CharStream<unit>("")
        testEmptyStream emptyStringStream2

        use emptyFileStream = createMultiBlockTestStream (new System.IO.MemoryStream(be.GetPreamble(), false)) System.Text.Encoding.Unicode
        testEmptyStream emptyFileStream

        use emptyFileStream2 = createMultiBlockTestStream (new System.IO.MemoryStream([||], false)) System.Text.Encoding.Unicode
        testEmptyStream emptyFileStream2

    testStreams()
    xTest()
    testSkipWhitespace()
    testSkipUnicodeWhitespace()
    testSkipNewlineWhitespace()
    testSkipRestOfLine()
    testSkipCharsOrNewlines()
    SkipCharsOrNewlinesUntilString()
    testCreateSubstream()
    testTwoChars()
