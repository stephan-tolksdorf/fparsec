// Copyright (c) Stephan Tolksdorf 2009-2010
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.StringBufferTests

#if LOW_TRUST

let run() = ()

#else

open FParsec.Test.Test

type StringBuffer = FParsec.StringBuffer

// This test relies on the internal assert checks in StringBuffer,
// hence this is only really a proper test in Debug builds or
// if you compile FParsecCS with the DEBUG_STRINGBUFFER define.

let test() =
    let ty = typeof<StringBuffer>
    let getStaticField name   = getStaticField ty name
    let minChunkSize          = getStaticField "MinChunkSize" : int
    let firstSegmentSmallSize = getStaticField "FirstSegmentSmallSize" : int
    let firstSegmentLargeSize = getStaticField "FirstSegmentLargeSize" : int
    let maxSegmentSize        = getStaticField "MaxSegmentSize" : int

    let testConstructor() =
        let buffer1 = StringBuffer.Create(0)
        buffer1.Dispose()
        let buffer1 = StringBuffer.Create(firstSegmentSmallSize)
        buffer1.Dispose()
        let buffer2 = StringBuffer.Create(maxSegmentSize + 1)
        buffer2.Dispose()
        try StringBuffer.Create(System.Int32.MaxValue) |> ignore; Fail()
        with :? System.ArgumentOutOfRangeException -> ()
        try StringBuffer.Create(-1) |> ignore; Fail()
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
                    let buffer = StringBuffer.Create(size)
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

let run() =
    test()

#endif
