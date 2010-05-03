// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.CharStreamTests

#nowarn "9" //"Uses of this construct may result in the generation of unverifiable .NET IL code."
#nowarn "51" //"The address-of operator may result in non-verifiable code."

open System.Text.RegularExpressions

open Microsoft.FSharp.NativeInterop

open FParsec
open FParsec.Test.Test

exception ArgumentOutOfRange = System.ArgumentOutOfRangeException
exception OutOfMemory = System.OutOfMemoryException
exception NullReference = System.NullReferenceException
exception ArgumentNull = System.ArgumentNullException
exception NotSupported = System.NotSupportedException

let EOS = CharStream.Iterator.EndOfStreamChar

let testNonStreamConstructors() =
     let s = "1234567890"
     let cs = s.ToCharArray()

     let regex = new System.Text.RegularExpressions.Regex(".*")

     let testStream (stream: CharStream) (index: int) (length: int) (indexOffset: int64) (supportsRegex: bool) =
        stream.BeginIndex |> Equal indexOffset
        let iter = stream.Begin
        iter.Index |> Equal indexOffset
        if length > 0 then
            iter.Read() |> Equal s.[index]
            iter.Read(s.Length) |> Equal (s.Substring(index, length))
            iter.Advance(s.Length).Index |> Equal (indexOffset + int64 length)
            if supportsRegex then
                iter.Match(regex).Value |> Equal (s.Substring(index, length))
            else
                try iter.Match(regex).Value |> ignore; Fail()
                with NotSupported -> ()
        else
            iter.Read() |> Equal EOS

     let testStringStream() =
         use stream = new CharStream(s)
         testStream stream 0 s.Length 0L true

         use stream = new CharStream(s, 0, s.Length)
         testStream stream 0 s.Length 0L true
         use stream = new CharStream(s, 0, s.Length, 1000L)
         testStream stream 0 s.Length 1000L true
         use stream = new CharStream(s, 1, s.Length - 1)
         testStream stream 1 (s.Length - 1) 0L true
         use stream = new CharStream(s, 1, 1, 1000L)
         testStream stream 1 1 1000L true
         use stream = new CharStream(s, 1, 0, 1000L)
         testStream stream 1 0 1000L true

         try new CharStream((null: string), 1, 10) |> ignore; Fail()
         with ArgumentNull -> ()
         try new CharStream(s, -1, 1) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(s, 11, 0) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(s, 1, 10) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(s, 0, 10, -1L) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(s, 0, 10, (1L <<< 60)) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
     testStringStream()

 #if LOW_TRUST
 #else
     let testCharArrayStream() =
         use stream = new CharStream(cs, 0, s.Length)
         testStream stream 0 s.Length 0L false
         use stream = new CharStream(cs, 0, s.Length, 1000L)
         testStream stream 0 s.Length 1000L false
         use stream = new CharStream(cs, 1, s.Length - 1)
         testStream stream 1 (s.Length - 1) 0L false
         use stream = new CharStream(cs, 1, 1, 1000L)
         testStream stream 1 1 1000L false
         use stream = new CharStream(cs, 1, 0, 1000L)
         testStream stream 1 0 1000L false

         try new CharStream((null: char[]), 1, 10) |> ignore; Fail()
         with ArgumentNull -> ()
         try new CharStream(cs, -1, 1) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(cs, 11, 0) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(cs, 1, 10) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(cs, 0, 10, -1L) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(cs, 0, 10, (1L <<< 60)) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
     testCharArrayStream()

     let testCharPointerStream() =
         let handle = System.Runtime.InteropServices.GCHandle.Alloc(cs, System.Runtime.InteropServices.GCHandleType.Pinned)
         let cp = &&cs.[0]
         use stream = new CharStream(NativePtr.add cp 0, s.Length)
         testStream stream 0 s.Length 0L false
         use stream = new CharStream(NativePtr.add cp 0, s.Length, 1000L)
         testStream stream 0 s.Length 1000L false
         use stream = new CharStream(NativePtr.add cp 1, s.Length - 1)
         testStream stream 1 (s.Length - 1) 0L false
         use stream = new CharStream(NativePtr.add cp 1, 1, 1000L)
         testStream stream 1 1 1000L false
         use stream = new CharStream(NativePtr.add cp 1, 0, 1000L)
         testStream stream 1 0 1000L false

         try new CharStream(NativePtr.ofNativeInt 0n, 10) |> ignore; Fail()
         with ArgumentNull -> ()
         try new CharStream(cp, -1) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         if sizeof<System.IntPtr> = 4 then
            try new CharStream(cp, System.Int32.MaxValue) |> ignore; Fail()
            with ArgumentOutOfRange -> ()
         try new CharStream(cp, 10, -1L) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         try new CharStream(cp, 10, (1L <<< 60)) |> ignore; Fail()
         with ArgumentOutOfRange -> ()
         handle.Free()
     testCharPointerStream()
    #endif

let testStreamConstructorArgumentChecking() =
    let encoding = System.Text.Encoding.UTF8
    let str = "1234567890"
    let streamBytes = Array.append (encoding.GetPreamble()) (encoding.GetBytes(str))
    use stream = new System.IO.MemoryStream(streamBytes)
    try new CharStream((null: System.IO.Stream), false, encoding) |> ignore
    with :? System.ArgumentNullException -> ()
    try new CharStream(stream, null) |> ignore
    with :? System.ArgumentNullException -> ()

    let tempFilePath = System.IO.Path.GetTempFileName()
    use nonReadableStream = new System.IO.FileStream(tempFilePath, System.IO.FileMode.Open, System.IO.FileAccess.Write)
    try new CharStream(nonReadableStream, encoding) |> ignore
    with :? System.ArgumentException -> ()
    nonReadableStream.Write(streamBytes, 0, streamBytes.Length)
    nonReadableStream.Close()

    try new CharStream((null: string), encoding) |> ignore
    with :? System.ArgumentNullException -> ()
    try new CharStream("", (null: System.Text.Encoding)) |> ignore
    with :? System.ArgumentNullException -> ()

    use charStream = new CharStream(tempFilePath, System.Text.Encoding.ASCII, true)
    charStream.Begin.Read(str.Length + 1) |> Equal str
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
        use cs0 = new CharStream(new System.IO.MemoryStream(bs0, false), gb18030);
        cs0.Encoding.CodePage |> Equal (e.CodePage)

        bs0.[1] <- 33uy
        use cs0 = new CharStream(new System.IO.MemoryStream(bs0, false), gb18030);
        cs0.Encoding|> ReferenceEqual gb18030

        let bs = Array.append (e.GetPreamble()) (e.GetBytes(s))
        use cs = new CharStream(new System.IO.MemoryStream(bs, false), gb18030);
        cs.Encoding.CodePage |> Equal (e.CodePage)
        cs.Begin.Read(s.Length) |> Equal s
        use cs2 = new CharStream(new System.IO.MemoryStream(bs, false), e);
        cs2.Encoding |> ReferenceEqual e
        cs2.Begin.Read(s.Length) |> Equal s
        use cs3 = new CharStream(new System.IO.MemoryStream(bs, false), false, gb18030, false);
        cs3.Encoding |> ReferenceEqual gb18030

    test (System.Text.UTF32Encoding(false, true))
    test (System.Text.UTF32Encoding(true, true))
    test (System.Text.UnicodeEncoding(false, true))
    test (System.Text.UnicodeEncoding(true, true))
    test (System.Text.UTF8Encoding(true))

    let e = CustomPreambleUTF8Encoding([|0uy;1uy;2uy;3uy;4uy|])
    let bs = Array.append (e.GetPreamble()) (e.GetBytes(s))
    use cs = new CharStream(new System.IO.MemoryStream(bs, false), e);
    cs.Encoding.CodePage |> Equal (e.CodePage)
    cs.Begin.Read(s.Length) |> Equal s

/// creates a CharStream with block size 8 and block overlap 3
let createMultiBlockTestStream byteStream encoding =
    new CharStream(byteStream, false,
                   encoding, true,
                   #if LOW_TRUST
                   #else
                      8, 3, 3,
                   #endif
                      16);

type NonSeekableMemoryStream(bytes: byte[]) =
     inherit System.IO.MemoryStream(bytes)
     override t.Seek(offset, origin) = raise (System.NotSupportedException())
     override t.CanSeek = false

#if LOW_TRUST
#else

let testStreamBuffer() =
    let ty = typeof<FParsec.StringBuffer>
    let getStaticField name = getStaticField ty name
    let minChunkSize          = getStaticField "MinChunkSize" : int
    let firstSegmentSmallSize = getStaticField "FirstSegmentSmallSize" : int
    let firstSegmentLargeSize = getStaticField "FirstSegmentLargeSize" : int
    let maxSegmentSize        = getStaticField "MaxSegmentSize" : int

    let testConstructor() =
        let buffer1 = FParsec.StringBuffer.Create(0)
        buffer1.Dispose()
        let buffer1 = FParsec.StringBuffer.Create(firstSegmentSmallSize)
        buffer1.Dispose()
        let buffer2 = FParsec.StringBuffer.Create(maxSegmentSize + 1)
        buffer2.Dispose()
        try FParsec.StringBuffer.Create(System.Int32.MaxValue) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()
        try FParsec.StringBuffer.Create(-1) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()

    testConstructor()

    let rand = System.Random(1054754)
    let maxBufferSize =  196608
    let maxTotalSize = 1 <<< 22
    let buffers = new ResizeArray<_>()
    let mutable allocated = 0
    let mutable maxReached = false
    for i = 1 to 10000 do
        if (not maxReached && rand.Next(2) = 0) || buffers.Count = 0 then
            let maxSize = rand.Next(maxBufferSize + 1)
            let n = rand.Next(1, 11)
            for i = 1 to n do
                let size = rand.Next(maxSize + 1)
                if allocated + size < maxTotalSize then
                    let buffer = FParsec.StringBuffer.Create(size)
                    allocated <- allocated + buffer.Length
                    buffers.Add(buffer)
                else
                    maxReached <- true
        else
            maxReached <- false
            let n = rand.Next(1, buffers.Count + 1)
            for i = 1 to n do
                let idx = rand.Next(buffers.Count)
                let buffer = buffers.[idx]
                allocated <- allocated - buffer.Length
                buffer.Dispose()
                buffers.RemoveAt(idx)
    buffers.Reverse()
    for b in buffers do b.Dispose()

// currently unused because ...
type NonSerializableUTF8Decoder() =
    inherit System.Text.Decoder()
    let decoder = System.Text.Encoding.UTF8.GetDecoder()
    override t.GetCharCount(bytes: byte[], index: int, count: int) : int =
        raise (System.NotImplementedException())
    override t.GetChars(bytes: byte[], byteIndex: int, byteCount: int, chars: char[], charIndex: int): int =
        raise (System.NotImplementedException())
    // ... the following method is not correctly compiled
    override t.Convert(bytes, byteCount, chars, charCount, flush,  bytesUsed: byref<int>, charsUsed: byref<int>, completed: byref<bool>) =
        decoder.Convert(bytes, byteCount, chars, charCount, flush, &bytesUsed, &charsUsed, &completed)

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
        let mutable iter = stream.Begin
        iter.Read(9) |> ignore
        try iter.Read() |> ignore
            Fail()
        with :? System.NotSupportedException as e -> ()
    testNonSeekableByteStream()

    let testNonSerializableEncoding() =
        let nsEncoding = FParsec.Helper.UTF8EncodingWithNonSerializableDecoder()
        use byteStream = new System.IO.MemoryStream(streamBytes)
        use stream = createMultiBlockTestStream byteStream nsEncoding
        let mutable iter0 = stream.Begin
        let mutable iter1 = iter0
        iter1._Increment(8u) |> Equal str.[8]
        let mutable iter2 = iter0
        iter2._Increment(uint32 (str.Length - 1)) |> Equal str.[str.Length - 1]
        // backtracking to the first block should work
        iter0.Read() |> Equal str.[0]
        // seeking forward should work too
        iter2.Read() |> Equal str.[str.Length - 1]
        try // but backtracking to a block other than the first should fail
            iter1.Read() |> ignore
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
            use stream = if not multiBlock then new CharStream(byteStream, encoding)
                         else createMultiBlockTestStream byteStream encoding
            let iter = stream.Begin
            iter.Read(int position + 4) |> ignore
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

let testEmptyStream (stream: CharStream) =
    let iter0 = stream.Begin
    let mutable iter = iter0
    iter.IsBeginOfStream |> True
    iter.IsEndOfStream |> True
    stream.EndIndex |> Equal 0L
    iter.Next |> Equal iter0
    iter.Advance(1) |> Equal iter0
    iter.Advance(System.Int32.MaxValue) |> Equal iter0
    try iter.Advance(-1) |> ignore; Fail()
    with ArgumentOutOfRange -> ()
    iter.Advance(1L) |> Equal iter0
    iter.Advance(System.Int64.MaxValue) |> Equal iter0
    try iter.Advance(-1L) |> ignore; Fail()
    with ArgumentOutOfRange -> ()
    try iter.Advance(System.Int64.MinValue) |> ignore; Fail()
    with ArgumentOutOfRange -> ()
    iter._Increment() |> Equal EOS
    iter |> Equal iter0
    iter._Increment(0u) |> Equal EOS
    iter |> Equal iter0
    iter._Increment(1u) |> Equal EOS
    iter |> Equal iter0
    iter._Increment((uint32)System.Int32.MaxValue) |> Equal EOS
    iter |> Equal iter0
    iter._Increment((uint32)System.Int32.MinValue) |> Equal EOS
    iter |> Equal iter0
    iter._Increment((uint32)System.UInt32.MaxValue) |> Equal EOS
    iter |> Equal iter0
    iter._Decrement() |> Equal EOS
    iter |> Equal iter0
    iter._Decrement(0u) |> Equal EOS
    iter |> Equal iter0
    iter._Decrement(1u) |> Equal EOS
    iter |> Equal iter0
    iter._Decrement((uint32)System.Int32.MaxValue) |> Equal EOS
    iter |> Equal iter0
    iter._Decrement((uint32)System.Int32.MinValue) |> Equal EOS
    iter |> Equal iter0
    iter._Decrement((uint32)System.UInt32.MaxValue) |> Equal EOS
    iter |> Equal iter0
    iter.Peek(0) |> Equal EOS
    iter.Peek(1) |> Equal EOS
    iter.Peek(System.Int32.MaxValue) |> Equal EOS
    iter.Peek(-1) |> Equal EOS
    iter.Peek(System.Int32.MinValue) |> Equal EOS
    iter.Peek(0u) |> Equal EOS
    iter.Peek(1u) |> Equal EOS
    iter.Peek((uint32)System.Int32.MaxValue) |> Equal EOS
    iter.Peek((uint32)System.Int32.MinValue) |> Equal EOS
    iter.Peek((uint32)System.UInt32.MaxValue) |> Equal EOS
    iter.Match("")  |> True
    iter.Match("x") |> False
    iter.MatchCaseFolded("")  |> True
    iter.MatchCaseFolded("x") |> False
    iter.Match([||],0,0) |> True
    iter.Match([|'x'|],0,1) |> False
    #if LOW_TRUST
    #else
    let mutable c = 'x'
    iter.Match(&&c, 0) |> True
    iter.Match(&&c, 1) |> False
    iter.MatchCaseFolded(&&c, 0) |> True
    iter.MatchCaseFolded(&&c, 1) |> False
    #endif
    iter.Match(Regex("x")).Success |> False
    iter.Read(0) |> Equal ""
    iter.Read(0, false) |> Equal ""
    iter.Read(0, true)  |> Equal ""
    iter.Read(1) |> Equal ""
    iter.Read(1, false) |> Equal ""
    iter.Read(1, true)  |> Equal ""
    iter.Read(System.Int32.MaxValue) |> Equal ""
    iter.Read(System.Int32.MaxValue, false) |> Equal ""
    iter.Read(System.Int32.MaxValue, true)  |> Equal ""
    let c = [|'x'|]
    iter.Read(c, 0, 1) |> Equal 0
    c.[0] |> Equal 'x'
    #if LOW_TRUST
    #else
    let mutable c = 'x'
    iter.Read(&&c, 1) |> Equal 0
    #endif
    let cs = iter.Read2()
    cs.Char0 |> Equal EOS
    cs.Char1 |> Equal EOS
    iter.ReadUntil(iter) |> ReferenceEqual ""

/// Tries to systematically test all code branches in CharStream.Iterator methods.
let testStream (stream: CharStream) (refString: string) blockSize blockOverlap minRegexSpace =
    let beginIndex = stream.Begin.Index
    let dollarString = new string('$', refString.Length)
    let N = refString.Length

    let testMove i j =
        let c0 = refString.[0]
        let ii = int64 (min i N) + beginIndex
        let ci = if i < N then refString.[i] else EOS
        let jj = int64 (min j N) + beginIndex
        let cj = if j < N then refString.[j] else EOS
        let d = j - min i N

        let iteri = stream.Seek(int64 i + beginIndex)
        iteri.Index  |> Equal ii
        iteri.Read() |> Equal ci
        let iterj = stream.Seek(int64 j + beginIndex)
        iterj.Index  |> Equal jj
        iterj.Read() |> Equal cj
        iterj.IsBeginOfStream |> Equal (j = 0)
        iterj.IsEndOfStream |> Equal (j >= N)

        let iter0 = stream.Begin
        iter0.Read() |> Equal c0
        let iteri1 = iter0.Advance(i)
        iteri1.Index  |> Equal ii
        iteri1.Read() |> Equal ci
        if d = 1 then
            let itern = iteri1.Next
            itern.Index  |> Equal jj
            itern.Read() |> Equal cj
            iter0.Read() |> Equal c0; iteri1.Read() |> Equal ci; // restore state of stream before branch

        let iterj1 = iteri1.Advance(d)
        iterj1.Index  |> Equal jj
        iterj1.Read() |> Equal cj

        iter0.Read() |> Equal c0
        let iteri2 = iter0.Advance(int64 i)
        iteri2.Index  |> Equal ii
        iteri2.Read() |> Equal ci
        if d >= 0 then
            let iterj2b = iteri2.Advance(uint32 d)
            iterj2b.Index  |> Equal jj
            iterj2b.Read() |> Equal cj
            iter0.Read() |> Equal c0; iteri2.Read() |> Equal ci; // restore state of stream before branch

        let iterj2 = iteri2.Advance(int64 d)
        iterj2.Index  |> Equal jj
        iterj2.Read() |> Equal cj

        iter0.Read() |> Equal c0
        let mutable iter3 = iter0
        iter3._Increment(uint32 i) |> Equal ci
        iter3.Index  |> Equal ii
        iter3.Read() |> Equal ci
        if j = 0 then
            let mutable iter4 = iter3
            iter4._Decrement(uint32 -d + 1u) |> Equal EOS
            iter4 |> Equal iter0
            iter4 <- iter3
            iter4._Decrement(System.UInt32.MaxValue) |> Equal EOS
            iter4 |> Equal iter0
        if d >= 0 then
            if d = 1 then
                let mutable iter4 = iter3
                iter4._Increment() |> Equal cj
                iter4.Index  |> Equal jj
                iter4.Read() |> Equal cj
                iter0.Read() |> Equal c0; iter3.Read() |> Equal ci; // restore state of stream before branch
            iter3._Increment(uint32 d) |> Equal cj
            iter3.Index  |> Equal jj
            iter3.Read() |> Equal cj
            if d = 0 then
                iter3._Decrement(0u) |> Equal cj
                iter3.Index  |> Equal jj
                iter3.Read() |> Equal cj
        else
            if d = -1 then
                let mutable iter4  = iter3
                iter4._Decrement() |> Equal cj
                iter4.Index  |> Equal jj
                iter4.Read() |> Equal cj
                iter0.Read() |> Equal c0; iter3.Read() |> Equal ci; // restore state of stream before branch
            iter3._Decrement(uint32 -d) |> Equal cj
            iter3.Index  |> Equal jj
            iter3.Read() |> Equal cj

    for i = 0 to N + 2 do
        for j = 0 to N + 2 do
            testMove i j

    let getIter (i: int) =
        let iter = stream.Begin
        iter.Read() |> ignore// move stream back to first block
        iter.Advance(i)

    let testMoveException() =
        let endIndex = beginIndex + int64 N
        stream.Seek(System.Int64.MaxValue).Index |> Equal endIndex
        try  stream.Seek(-1L) |> ignore; Fail ()
        with ArgumentOutOfRange -> ()
        try  stream.Seek(System.Int64.MinValue) |> ignore; Fail ()
        with ArgumentOutOfRange -> ()

        getIter(0).Advance(System.Int32.MaxValue).Index |> Equal endIndex
        getIter(0).Advance(System.UInt32.MaxValue).Index |> Equal endIndex
        getIter(0).Advance(System.Int64.MaxValue).Index |> Equal endIndex

        for i = 0 to N do
            try  getIter(i).Advance(-i - 1) |> ignore; Fail ()
            with ArgumentOutOfRange -> ()
            try  getIter(i).Advance(int64 (-i - 1)) |> ignore; Fail ()
            with ArgumentOutOfRange -> ()
            try  getIter(i).Advance(System.Int32.MinValue) |> ignore; Fail ()
            with ArgumentOutOfRange -> ()
            try  getIter(i).Advance(System.Int64.MinValue) |> ignore; Fail ()
            with ArgumentOutOfRange -> ()

            let mutable iter = getIter(i)
            iter._Decrement(uint32 (i + 1)) |> Equal EOS
            iter |> Equal stream.Begin

            iter <- getIter(i)
            iter._Decrement(System.UInt32.MaxValue) |> Equal EOS
            iter |> Equal stream.Begin

            let mutable iter0 = getIter(0)
            iter0._Decrement() |> Equal EOS
            iter0 |> Equal stream.Begin

    testMoveException()

    let regex = new Regex(".*", RegexOptions.Singleline)

    let testMatch i n =
    #if LOW_TRUST
    #else
        let matchCaseFoldedArray (iter: CharStream.Iterator) (cs: char[]) i n =
            if n > 0 then
                let handle = System.Runtime.InteropServices.GCHandle.Alloc(cs, System.Runtime.InteropServices.GCHandleType.Pinned)
                let result = iter.MatchCaseFolded(&&cs.[0], n)
                handle.Free()
                result
            else
                let mutable c = '$'
                iter.MatchCaseFolded(&&c, 0)
    #endif

        let test (str: string) (result: bool) =
            let n = str.Length
            let strA = str.ToCharArray()
            let cfStr = CharStream.FoldCase(str)
            let cfStrA = cfStr.ToCharArray()

            getIter(i).Match(str) |> Equal result
            getIter(i).Match(str, 0, n) |> Equal result
            getIter(i).Match(strA, 0, n) |> Equal result
            if n > 1 then
                let restIsEqual = getIter(i + n - 1).Match(str.[n - 1]) // str only differs in first or last char
                getIter(i + 1).Match(str, 1, n - 1)  |> Equal restIsEqual
                getIter(i + 1).Match(strA, 1, n - 1) |> Equal restIsEqual

            getIter(i).MatchCaseFolded(cfStr) |> Equal result
        #if LOW_TRUST
        #else
            matchCaseFoldedArray (getIter(i)) cfStrA 0 n |> Equal result
        #endif

        if n = 0 then
            test "" true
        elif i < N then
            let ci  = refString.[i]
            let ci1 = char (int ci + 1)
            if n = 1 then
                getIter(i).Match(ci) |> Equal true
                getIter(i).Match(ci1) |> Equal false
            if n > 0 && i + n <= N then
                test (refString.Substring(i, n)) true
                if n = 1 then
                    test (ci1.ToString()) false
                else
                    test (ci1.ToString() + refString.Substring(i + 1, n - 1)) false
                    test (refString.Substring(i, n - 1) + ((char (int (refString.[i + n - 1]) + 1)).ToString())) false
            else
                test (refString.Substring(i, N - i) + (new string(refString.[N - 1], n - (N - i)))) false
            let iteri = getIter(i)
            let mutable iter = iteri
            let mstr = iter.Match(regex).Value // might modify iter...
            iter.Equals(iteri) |> True // ...but iter should still point to the same char
            let minLength = if blockOverlap = 0 then N - i else min minRegexSpace (N - i)
            (mstr.Length >= minLength) |> True
            mstr |> Equal (refString.Substring(i, mstr.Length))
        else
            let str = new string(refString.[N - 1], n)
            test str false
            let iteri = getIter(i)
            let mutable iter = iteri
            iter.Match(regex).Value |> Equal ""
            iter.Equals(iteri) |> True

    for i = 0 to N do
        for n = 0 to N + 15 - i do
            testMatch i n

    let testMatchException() =
        let str = "$$$"
        let a = str.ToCharArray()

        let iter0 = getIter(0)

        try  iter0.Match(null: string) |> ignore; Fail()
        with NullReference -> ()

        try  iter0.MatchCaseFolded(null) |> ignore; Fail()
        with NullReference -> ()

        try  iter0.Match((null: string), 0, 0) |> ignore; Fail()
        with NullReference -> ()
        try  iter0.Match(str, 0, 4) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, 2, 2) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, 3, 1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, -1, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, 0, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, 0, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, System.Int32.MinValue, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(str, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try  iter0.Match((null: char[]), 0, 0) |> ignore; Fail()
        with NullReference -> ()
        try  iter0.Match(a, 0, 4) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, 2, 2) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, 3, 1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, -1, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, 0, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, 0, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, System.Int32.MinValue, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(a, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
    #if LOW_TRUST
    #else
        let mutable c = '$'
        try  iter0.Match(NativePtr.ofNativeInt 0n, 1) |> ignore; Fail()
        with NullReference -> ()
        try  iter0.Match(&&c, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Match(&&c, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try  iter0.MatchCaseFolded(NativePtr.ofNativeInt 0n, 1) |> ignore; Fail()
        with NullReference -> ()
        try  iter0.MatchCaseFolded(&&c, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.MatchCaseFolded(&&c, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
    #endif

        try iter0.Match(null: Regex) |> ignore; Fail()
        with NullReference -> ()

        let iterN = getIter(N)

        try  iterN.Match(str, 0, 4) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, 2, 2) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, 3, 1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, -1, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, 0, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, 0, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, System.Int32.MinValue, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(str, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try  iterN.Match(a, 0, 4) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, 2, 2) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, 3, 1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, -1, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, 0, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, 0, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, System.Int32.MinValue, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(a, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

    #if LOW_TRUST
    #else
        try  iterN.Match(&&c, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Match(&&c, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try  iterN.MatchCaseFolded(&&c, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.MatchCaseFolded(&&c, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
    #endif

        try getIter(N).Match(null: Regex) |> ignore; Fail()
        with NullReference -> ()


    testMatchException()

    // reading
    //=========

    let testRead i n =
        if i < N && n > 0 then
            let str = refString.Substring(i, min n (N - i))
            getIter(i).Read() |> Equal str.[0]
            getIter(i).Read(n)|> Equal str
            if n = 2 then
                let chars = getIter(i).Read2()
                chars.Char0 |> Equal str.[0]
                chars.Char1 |> Equal (if str.Length = 2 then str.[1] else EOS)

            let endIter = getIter(i).Advance(n)
            getIter(i).ReadUntil(endIter) |> Equal str
            getIter(i).Read(n, false) |> Equal str
            if str.Length = n then
                getIter(i).Read(n, true) |> Equal str
            else
                getIter(i).Read(n, true) |> Equal ""
            let cs = Array.create (N + 2) '$'
            getIter(i).Read(cs, i%2, min n N) |> Equal str.Length
            let cs2 = ((if i%2 = 1 then "$" else "") + str + new string('$', N + 2 - str.Length - i%2))
            new string(cs) |> Equal cs2
        #if LOW_TRUST
        #else
            Array.fill cs 0 cs.Length '$'
            let handle = System.Runtime.InteropServices.GCHandle.Alloc(cs, System.Runtime.InteropServices.GCHandleType.Pinned)
            let ptr = NativePtr.ofNativeInt<char> (handle.AddrOfPinnedObject())
            let count = getIter(i).Read(NativePtr.add ptr (i%2), min n N)
            handle.Free()
            count |> Equal str.Length
            new string(cs) |> Equal cs2
        #endif

        else
            if i >= N then getIter(i).Read() |> Equal EOS
            getIter(i).Read(n) |> Equal ""
            getIter(i).Read(n, true) |> Equal ""
            getIter(i).Read(n, false) |> Equal ""
            let endIter = getIter(i)
            getIter(i).ReadUntil(endIter) |> ReferenceEqual ""
            let cs = dollarString.ToCharArray()
            getIter(i).Read(cs, i%2, min n N) |> Equal 0
            new string(cs) |> Equal dollarString
        #if LOW_TRUST
        #else
            Array.fill cs 0 cs.Length '$'
            let handle = System.Runtime.InteropServices.GCHandle.Alloc(cs, System.Runtime.InteropServices.GCHandleType.Pinned)
            let ptr = NativePtr.ofNativeInt<char> (handle.AddrOfPinnedObject())
            let count = getIter(i).Read(NativePtr.add ptr (i%2), min n N)
            handle.Free()
            count |> Equal 0
            new string(cs) |> Equal dollarString
        #endif

    for i = 0 to N do
        for n = 0 to N + 15 - i do
            testRead i n

    let testReadException() =
        let iter0 = getIter(0)

        try
            let str = iter0.Read(System.Int32.MaxValue)
            str |> Equal refString
        with OutOfMemory -> ()
        try  iter0.Read(-1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try
            let str = iter0.Read(System.Int32.MaxValue, false)
            str |> Equal refString
        with OutOfMemory -> ()
        try  iter0.Read(-1, false) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(System.Int32.MinValue, false) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try
            iter0.Read(System.Int32.MaxValue, true) |> Equal ""
        with OutOfMemory -> ()
        try  iter0.Read(-1, true) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(System.Int32.MinValue, true) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        let a = Array.create 3 '$'

        try  iter0.Read(null, 0, 1) |> ignore; Fail()
        with NullReference -> ()
        try  iter0.Read(a, 0, 4) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, 2, 2) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, 3, 1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, -1, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, 0, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, 0, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, System.Int32.MinValue, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(a, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

    #if LOW_TRUST
    #else
        let mutable c = '_'
        try  iter0.Read(NativePtr.ofNativeInt 0n, 1) |> ignore; Fail()
        with NullReference -> ()
        try  iter0.Read(&&c, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iter0.Read(&&c, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
    #endif

        iter0.Next.ReadUntil(iter0) |> ReferenceEqual ""

        use cs2 = new CharStream("another stream", 0, 5)
        try iter0.ReadUntil(cs2.Begin) |> ignore; Fail()
        with :? System.ArgumentException -> ()

        let iterN = getIter(N)

        try  iterN.Read(System.Int32.MaxValue) |> Equal ""
        with OutOfMemory -> ()
        try  iterN.Read(-1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try  iterN.Read(System.Int32.MaxValue, false) |> Equal ""
        with OutOfMemory -> ()
        try  iterN.Read(-1, false) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(System.Int32.MinValue, false) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        try  iterN.Read(System.Int32.MaxValue, true) |> Equal ""
        with OutOfMemory -> ()
        try  iterN.Read(-1, true) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(System.Int32.MinValue, true) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

        let a = Array.create 3 '$'

        try  iterN.Read(null, 0, 1) |> ignore; Fail()
        with NullReference -> ()
        try  iterN.Read(a, 0, 4) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, 2, 2) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, 3, 1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, -1, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, 0, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, 0, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, System.Int32.MinValue, 0) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, System.Int32.MinValue, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(a, System.Int32.MinValue, System.Int32.MaxValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()

    #if LOW_TRUST
    #else
        let mutable c = '_'
        try  iterN.Read(NativePtr.ofNativeInt 0n, 1) |> Equal 0
        with NullReference -> ()
        try  iterN.Read(&&c, -1) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
        try  iterN.Read(&&c, System.Int32.MinValue) |> ignore; Fail()
        with ArgumentOutOfRange -> ()
    #endif

    testReadException()

    let testPeek i (j: int) =
        let d = j - min i N
        let c = if j < N then refString.[j] else EOS
        getIter(i).Peek(d) |> Equal c
        if d = 1 then
            getIter(i).Peek() |> Equal c
        if d > 0 then
            let iter = getIter(i)
            let cc = iter.Peek(uint32 d)
            Equal c cc

    for i = 0 to N + 2 do
        for j = 0 to N + 2 do
            testPeek i j

    let testPeekException() =
        let iter0 = getIter(0)

        getIter(0).Peek(System.Int32.MaxValue)  |> Equal EOS
        getIter(0).Peek(System.UInt32.MaxValue) |> Equal EOS
        getIter(0).Peek(-1) |> Equal EOS
        getIter(0).Peek(System.Int32.MinValue)  |> Equal EOS

        getIter(N - 1).Peek(System.Int32.MaxValue)  |> Equal EOS
        getIter(N - 1).Peek(System.UInt32.MaxValue) |> Equal EOS
        getIter(N - 1).Peek(-N)  |> Equal EOS
        getIter(N - 1).Peek(System.Int32.MinValue)  |> Equal EOS

        let iterN = getIter(N)
        getIter(N).Peek(System.Int32.MaxValue)  |> Equal EOS
        getIter(N).Peek(System.UInt32.MaxValue) |> Equal EOS
        getIter(N).Peek(-N - 1)  |> Equal EOS
        getIter(N).Peek(System.Int32.MinValue)  |> Equal EOS

    testPeekException()

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
    let EOS = CharStream.Iterator.EndOfStreamChar
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
                let encoding = if r = 1 then utf16 else gb18030
                let chars = generateRandomUnicodeChars (1 <<< 17)
                let bytes = encoding.GetBytes(chars)
                encoding, bytes, chars

    #if LOW_TRUST
        use stringStream = new CharStream(new string(chars), 0, chars.Length)
    #else
        use stringStream = new CharStream(chars, 0, chars.Length)
    #endif

        let blockSize = 16 + rand.Next(maxBlockSize - 16)
        let maxCharsForOneByte = encoding.GetMaxCharCount(1)
        let blockOverlap = maxCharsForOneByte + rand.Next(blockSize/2 - maxCharsForOneByte)
        let byteBufferLength = 8 + rand.Next(maxBlockSize)
        let blockSizeMinusOverlap = blockSize - blockOverlap
        use charStream  = new CharStream(new System.IO.MemoryStream(bytes), false,
                                         encoding, true,
                                     #if LOW_TRUST
                                     #else
                                         blockSize, blockOverlap, 0,
                                     #endif
                                         byteBufferLength)

        if j%10 = 1 then
            let mutable csIter = charStream.Begin
            let mutable ssIter = stringStream.Begin

            for i = 0 to chars.Length - 1 do
                Equal (csIter.Read()) (ssIter.Read())
                if i%blockSizeMinusOverlap = blockOverlap && i >= blockSize then
                    Equal (csIter.Advance(-blockSize).Read(blockSize)) (ssIter.Advance(-blockSize).Read(blockSize))
                csIter <- csIter.Next; ssIter <- ssIter.Next


        let mutable index = int32 (rand.Next(chars.Length))
        let mutable csIter = charStream.Seek(int64 index)
        let mutable ssIter = stringStream.Seek(int64 index)
        // a random walk with jumps...
        let mutable i = 0;
        let mutable resetCounter = 0
        while i < 10000 do
                                                  // biased towards jumping backwards
            let jumpSize = rand.Next(maxJumpSize) * if rand.Next(4) = 0 then 1 else -1
            index <- index + jumpSize
            if 0 < index && index < chars.Length then
                csIter <- csIter.Advance(jumpSize)
                ssIter <- ssIter.Advance(int64 jumpSize)
            else
                resetCounter <- resetCounter + 1
                index <- rand.Next(chars.Length)
                csIter <- charStream.Seek(int64 index)
                ssIter <- stringStream.Seek(int64 index)

            let mutable doContinue = true
            while doContinue do
               if i % 500 = 0 then
                   index <- rand.Next(chars.Length)
                   csIter <- charStream.Seek(int64 index)
                   ssIter <- stringStream.Seek(int64 index)
                   csIter.Read(3) |> Equal (ssIter.Read(3))
               ssIter.Index |> Equal (int64 index)
               csIter.Index |> Equal (int64 index)
               match rand.Next(4) with
                | 0 -> let c = ssIter.Read()
                       if c <> EOS || not ssIter.IsEndOfStream then
                           csIter.Match(c) |> True
                       elif index >= 7 then
                           csIter.Advance(-7).Read() |> Equal (ssIter.Peek(-7))
                       index <- index + 1; csIter <- csIter.Next; ssIter <- ssIter.Next

                | 1 -> let n = 1 + rand.Next(maxReadSize)
                       let str = ssIter.Read(n)
                       let nc = str.Length
                       ssIter.Read(readBuffer, 0, n) |> Equal nc
                       new string(readBuffer, 0, nc) |> Equal str
                       csIter.Match(str) |> True
                       csIter.Match(readBuffer, 0, str.Length) |> True
                       let cfStr = CharStream.FoldCase(str)
                       csIter.MatchCaseFolded(cfStr) |> True

                       if nc = 0 then csIter.IsEndOfStream |> True
                       index <- index + nc; csIter <- csIter.Advance(uint32 nc); ssIter <- ssIter.Advance(nc)

                | 2 -> let c = csIter.Read()
                       if c <> EOS || not csIter.IsEndOfStream then
                           ssIter.Match(c) |> True
                       elif index >= 1 then
                           ssIter.Advance(-1).Read() |> Equal (csIter.Peek(-1))
                       index <- index + 1; csIter <- csIter.Next; ssIter <- ssIter.Next

                | _ -> let n = 1 + rand.Next(maxReadSize)
                       let str = csIter.Read(n)
                       let nc = str.Length
                       csIter.Read(readBuffer, 0, n) |> Equal nc
                       new string(readBuffer, 0, nc) |> Equal str

                       ssIter.Match(str) |> True
                       ssIter.Match(readBuffer, 0, str.Length) |> True

                       let cfStr = CharStream.FoldCase(str)
                       ssIter.MatchCaseFolded(cfStr) |> True

                       if nc = 0 then ssIter.IsEndOfStream |> True
                       index <- index + nc; csIter <- csIter.Advance(int64 n); ssIter <- ssIter.Advance(n)

               if index > chars.Length then index <- chars.Length
               doContinue <- rand.Next(3) <> 0 // jump every 4th iteration on average
               i <- i + 1

let testNormalizeNewlines() =
    let normalize = CharStream.NormalizeNewlines
    normalize null |> Equal null
    normalize ""   |> ReferenceEqual ""
    normalize "ab" |> ReferenceEqual "ab"

    let check (cs: char[]) n =
        let str = new string(cs, 0, n)
        let nstr = str.Replace("\r\n", "\n").Replace("\r", "\n")
        let nstr2 = normalize str
        Equal nstr nstr2

    let rec test (cs: char[]) n i =
        if i < n then
            cs.[i] <- '\r'
            test cs n (i + 1)
            cs.[i] <- '\n'
            test cs n (i + 1)
            cs.[i] <- '_'
            test cs n (i + 1)
        else
            check cs n

    let N = 10
    let cs = Array.zeroCreate N

    for n = 1 to 8 do
        // test all possible character sequences of length n consisting of '\r','\n' or '_' chars
        test cs n 0

    // make sure there is no size-specific copying problem
    for n = 1 to 24 do
        let s = new string('_', n)
        normalize ("\r" + s) |> Equal ("\n" + s)
        normalize ("\r\n" + s) |> Equal ("\n" + s)

    normalize "_\n_\r\n_\r\r_\r\n_\r\n\n\r_" |> Equal "_\n_\n_\n\n_\n_\n\n\n_"

let testFoldCase() =
    let foldCase = CharStream.FoldCase
    foldCase null   |> Equal null
    for s in [""; "a"; "aa"; "aaa"] do
        foldCase s |> ReferenceEqual s

    foldCase "A"    |> Equal "a"
    foldCase "aA"   |> Equal "aa"
    foldCase "aaA"  |> Equal "aaa"
    foldCase "abcAOUÄÖÜdef" |> Equal "abcaouäöüdef"

    let oneToOneMappings = getStaticField (typeof<FParsec.CharStream>.Assembly.GetType("FParsec.CaseFoldTable")) "oneToOneMappings" : string

    let mutable j = 0
    for i in 0..2..(oneToOneMappings.Length - 2) do
        let c = int oneToOneMappings.[i]
        for k = j to c - 1 do
            CharStream.FoldCase((char k).ToString()).[0] |> Equal (char k)
        CharStream.FoldCase((char c).ToString()).[0] |> Equal oneToOneMappings.[i + 1]
        j <- c + 1
    j |> Equal 0xff3b


let run() =
#if LOW_TRUST
#else
    testStreamBuffer()
    setStaticField typeof<FParsec.CharStream> "MinimumByteBufferLength" 10
    setStaticField typeof<FParsec.CharStream> "DoNotRoundUpBlockSizeToSimplifyTesting" true
    testNonSeekableCharStreamHandling()
#endif
    testNonStreamConstructors()
    testStreamConstructorArgumentChecking()
    testEncodingDetection()
    testDecoderFallbackExceptionHandling()

    let testStreams() =
        let refString = "1234567890ABCDEF"
        use stringStream = new CharStream(" " + refString, 1, refString.Length, 100L)
        testStream stringStream refString refString.Length 0 0

        let be = new System.Text.UTF32Encoding(true, true)
        let bs = Array.append (be.GetPreamble()) (be.GetBytes(refString))
        use fileStream = createMultiBlockTestStream (new System.IO.MemoryStream(bs, false)) System.Text.Encoding.Unicode
        testStream fileStream refString 8 3 3

        let refString2 = "1234567890ABCDEFGH" // exactly three blocks + 1 overlap
        let bs2 = System.Text.Encoding.Unicode.GetBytes(refString2)
        use fileStream = createMultiBlockTestStream (new System.IO.MemoryStream(bs2, false)) System.Text.Encoding.Unicode
        testStream fileStream refString2 8 3 3

        use emptyStringStream = new CharStream("x", 1, 0)
        testEmptyStream emptyStringStream

        use emptyStringStream2 = new CharStream("")
        testEmptyStream emptyStringStream2

        use emptyFileStream = createMultiBlockTestStream (new System.IO.MemoryStream(be.GetPreamble(), false)) System.Text.Encoding.Unicode
        testEmptyStream emptyFileStream

        use emptyFileStream2 = createMultiBlockTestStream (new System.IO.MemoryStream([||], false)) System.Text.Encoding.Unicode
        testEmptyStream emptyFileStream2

    testStreams()
    xTest()
    testNormalizeNewlines()
    testFoldCase()

