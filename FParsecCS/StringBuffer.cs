// Copyright (c) Stephan Tolksdorf 2009
// License: Simplified BSD License. See accompanying documentation.

#if !LOW_TRUST

#if DEBUG
    #define DEBUG_STRINGBUFFER
#endif

using System;
using System.Runtime.InteropServices;
using System.Diagnostics;

namespace FParsec {

/// <summary>A substring of a pinned string on the large object heap.
/// StringBuffers are cached in a pool and hence need to be properly disposed.</summary>
internal unsafe sealed class StringBuffer : IDisposable {
    private PoolSegment Segment;
    public string String { get { return Segment == null ? "" : Segment.String; } }
    public char* StringPointer { get { return Segment == null ? null : Segment.StringPointer; } }
    public int Index { get; private set; }
    public int Length { get; private set; }

    private StringBuffer(PoolSegment segment, int index, int length) {
        Segment = segment;
        Index = index;
        Length = length;
    }

    private sealed class FreeChunk {
        public PoolSegment Segment;

        // free chunks in each segment form a doubly-linked list ordered by index
        public FreeChunk PrevInSegment;
        public FreeChunk NextInSegment;

        public static FreeChunk Smallest;
        public static FreeChunk Largest;

        // all free chunks together form a doubly-linked list ordered by size
        public FreeChunk PrevInSize;
        public FreeChunk NextInSize;

        public int Index;
        public int Size;

        public FreeChunk(PoolSegment segment, int index, int size) {
            Debug.Assert(segment.FirstFreeChunk == null && index >= 0 && size > 0 && index + size <= segment.Size);
            Segment = segment;
            Index = index;
            Size = size;
            segment.FirstFreeChunk = this;
            InsertIntoSizeList();
        }

        public FreeChunk(PoolSegment segment, FreeChunk prevInSegment, FreeChunk nextInSegment, int index, int size) {
            Debug.Assert(index >= 0 && size > 0 && index + size <= segment.Size);
            Segment = segment;
            Index = index;
            Size = size;
            PrevInSegment = prevInSegment;
            NextInSegment = nextInSegment;
            if (prevInSegment != null) {
                Debug.Assert(prevInSegment.Index + prevInSegment.Size < index);
                prevInSegment.NextInSegment = this;
            } else {
                Debug.Assert(segment.FirstFreeChunk == nextInSegment);
                segment.FirstFreeChunk = this;
            }
            if (nextInSegment != null) {
                Debug.Assert(index + size < nextInSegment.Index);
                nextInSegment.PrevInSegment = this;
            }
            InsertIntoSizeList();
        }

        private void InsertIntoSizeList() {
            var largest = FreeChunk.Largest;
            if (largest != null) {
                if (largest.Size <= Size) {
                    largest.NextInSize = this;
                    PrevInSize = largest;
                    FreeChunk.Largest = this;
                } else {
                    NextInSize = largest;
                    var prev = largest.PrevInSize;
                    largest.PrevInSize = this;
                    if (prev != null) {
                        PrevInSize = prev;
                        prev.NextInSize = this;
                        if (Size < prev.Size) MoveAfterSizeHasDecreased();
                    } else FreeChunk.Smallest = this;
                }
            } else {
                FreeChunk.Smallest = this;
                FreeChunk.Largest = this;
            }
        }

        public void Remove() {
            var prev = PrevInSegment;
            var next = NextInSegment;
            if (prev != null) prev.NextInSegment = next;
            else Segment.FirstFreeChunk = next;
            if (next != null) next.PrevInSegment = prev;

            prev = PrevInSize;
            next = NextInSize;
            if (prev != null) prev.NextInSize = next;
            else Smallest = next;
            if (next != null) next.PrevInSize = prev;
            else Largest = prev;
        }

        // the following two methods are dual to each other,
        // i.e. one can be transformed into the other by way of simple search & replace
        public void MoveAfterSizeHasDecreased() {
            Debug.Assert(Size < PrevInSize.Size);
            var prev = PrevInSize;
            var next = NextInSize;
            if (next != null) next.PrevInSize = prev;
            else Largest = prev;
            prev.NextInSize = next;
            next = prev;
            prev = prev.PrevInSize;
            while (prev != null && prev.Size > Size) {
                next = prev;
                prev = prev.PrevInSize;
            }
            NextInSize = next;
            next.PrevInSize = this;
            PrevInSize = prev;
            if (prev != null) prev.NextInSize = this;
            else Smallest = this;
        }

        public void MoveAfterSizeHasIncreased() {
            Debug.Assert(Size > NextInSize.Size);
            var next = NextInSize;
            var prev = PrevInSize;
            if (prev != null) prev.NextInSize = next;
            else Smallest = next;
            next.PrevInSize = prev;
            prev = next;
            next = next.NextInSize;
            while (next != null && next.Size < Size) {
                prev = next;
                next = next.NextInSize;
            }
            PrevInSize = prev;
            prev.NextInSize = this;
            NextInSize = next;
            if (next != null) next.PrevInSize = this;
            else Largest = this;
        }
    }

    private const int MinChunkSize = 1536; // 3 * 2^9
    // segment sizes must be multiple of MinChunkSize and large enough to allocated on the LargeObjectHeap
    private const int FirstSegmentSmallSize = 42  * MinChunkSize; // 64 512
    private const int FirstSegmentLargeSize = 128 * MinChunkSize; // 3 * 2^16 = 196 608 (default CharStream block size)
    private const int MaxSegmentSize = 640 * MinChunkSize; // 983 040

    private static int MaxNumberOfUnusedSegments = 3;

    private static int NumberOfUnusedSegments;

    private sealed class PoolSegment : IDisposable {
        // segments form a doubly-linked list in the order they were constructed

        /// <summary>the last allocated segment</summary>
        private static PoolSegment Last;

        private PoolSegment Next;
        private PoolSegment Prev;

        public string String { get; private set; }
        /// <summary>String.Length - x, where x > 0</summary>
        public int Size { get; private set; }
        public char* StringPointer { get; private set; }
        private GCHandle StringHandle;

        public FreeChunk FirstFreeChunk;

        public PoolSegment(int size, int firstBufferSize) {
            Debug.Assert(firstBufferSize > 0 &&  firstBufferSize <= size && (size <= MaxSegmentSize || firstBufferSize == size));
            // + 1, so that no chunk can span the full string, which helps avoiding accidentally passing a reference to the internal buffer string to the "outside world"
            String = new String('\u0000', size + 1);
            Size = size;
            StringHandle = GCHandle.Alloc(String, GCHandleType.Pinned);
            StringPointer = (char*)StringHandle.AddrOfPinnedObject();
            if (Last != null) {
                Last.Next = this;
                Prev = Last;
            }
            Last = this;
            if (firstBufferSize < size)
                new FreeChunk(this, firstBufferSize, size - firstBufferSize); // inserts itself into the lists
        }

        public void Dispose() {
            if (StringPointer != null) {
                Debug.Assert(FirstFreeChunk == null);
                if (FirstFreeChunk != null) throw new InvalidOperationException();
                if (Prev != null) Prev.Next = Next;
                if (Next != null) Next.Prev = Prev;
                else Last = Prev;
                StringPointer = null;
                StringHandle.Free();
            }
        }

        public static StringBuffer AllocateStringBufferInNewSegment(int length) {
            int segmentSize = length > MaxSegmentSize
                              ? length
                              : (Last == null && length <= FirstSegmentLargeSize)
                                ? (length <= FirstSegmentSmallSize ? FirstSegmentSmallSize : FirstSegmentLargeSize)
                                : MaxSegmentSize;
            return new StringBuffer(new PoolSegment(segmentSize, length), 0, length);
        }

        [Conditional("DEBUG_STRINGBUFFER")]
        public void AssertIntegrity() {
            Debug.Assert(StringPointer != null);
            int sumOfSegmentSizes = 0;
            {   // check list of segments
                var segment = Last;
                Debug.Assert(segment.Next == null);
                var prev = segment.Prev;
                sumOfSegmentSizes += segment.Size;
                bool visitedThis = segment == this;
                while (prev != null) {
                    Debug.Assert(segment == prev.Next);
                    segment = prev;
                    prev  = prev.Prev;
                    sumOfSegmentSizes += segment.Size;
                    visitedThis = visitedThis || segment == this;
                }
                Debug.Assert(visitedThis);
            }
            {   // check segment list of free chunks ordered by index
                var chunk = FirstFreeChunk;
                if (chunk != null) {
                    Debug.Assert(   chunk.Index >= 0 && chunk.Size > 0
                                 && (chunk.PrevInSize != null ? chunk.Size >= chunk.PrevInSize.Size : chunk == FreeChunk.Smallest)
                                 && (chunk.NextInSize != null ? chunk.Size <= chunk.NextInSize.Size : chunk == FreeChunk.Largest));
                    int chunkEnd = chunk.Index + chunk.Size;
                    var next = chunk.NextInSegment;
                    while (next != null) {
                        Debug.Assert(   (chunk == next.PrevInSegment && chunkEnd < next.Index && next.Size > 0)
                                     && (next.PrevInSize != null ? next.Size >= next.PrevInSize.Size : next == FreeChunk.Smallest)
                                     && (next.NextInSize != null ? next.Size <= next.NextInSize.Size : next == FreeChunk.Largest));
                        chunk = next;
                        chunkEnd = chunk.Index + chunk.Size;
                        next = chunk.NextInSegment;
                    }
                    Debug.Assert(chunkEnd <= Size);
                }
            }
            {   // check global list of free chunks ordered by size
                int free = 0;
                var chunk = FreeChunk.Smallest;
                if (chunk == null) Debug.Assert(FreeChunk.Largest == null);
                else {
                    Debug.Assert(chunk.Size > 0 && chunk.PrevInSize == null);
                    free += chunk.Size;
                    var next = chunk.NextInSize;
                    while (next != null) {
                        Debug.Assert(chunk == next.PrevInSize && chunk.Size <= next.Size);
                        chunk = next;
                        free += chunk.Size;
                        next  = chunk.NextInSize;
                    }
                    Debug.Assert(chunk == FreeChunk.Largest);
                }
                Debug.Assert(Allocated == sumOfSegmentSizes - free);
            }
        }
    }


    /// <summary>Sum of the lengths of all currently allocated StringBuffers</summary>
    private static int Allocated = 0;
    private static object SyncRoot = new Object();

    public static StringBuffer Create(int minLength) {
        int size = unchecked(minLength + (MinChunkSize - 1));
        if (size > (MinChunkSize - 1)) { // minLength > 0 && minLength <= System.Int32.MaxValue - (MinChunkSize - 1)
            size -= (int)((uint)size%(uint)MinChunkSize); // round down to multiple of MinChunkSize
            lock (SyncRoot) {
                Allocated += size;
                FreeChunk chunk = FreeChunk.Largest;
                if (chunk != null) { // find smallest free chunk that is large enough to hold the buffer
                    if (size > 10*MinChunkSize) {
                        var prev = chunk.PrevInSize;
                        while (prev != null && prev.Size >= size) {
                            chunk = prev;
                            prev  = prev.PrevInSize;
                        }
                    } else {
                        chunk = FreeChunk.Smallest;
                        var next = chunk.NextInSize;
                        while (chunk.Size < size && next != null) {
                            chunk = next;
                            next  = next.NextInSize;
                        }
                    }
                    if (size <= chunk.Size) {
                        int index = chunk.Index;
                        if (index == 0 && chunk.Size == chunk.Segment.Size) --NumberOfUnusedSegments;
                        if (size != chunk.Size) {
                            chunk.Index += size;
                            chunk.Size  -= size;
                            var prev = chunk.PrevInSize;
                            if (prev != null && chunk.Size < prev.Size) chunk.MoveAfterSizeHasDecreased();
                        } else chunk.Remove();
                        chunk.Segment.AssertIntegrity();
                        return new StringBuffer(chunk.Segment, index, size);
                    }
                }
                return PoolSegment.AllocateStringBufferInNewSegment(size);
            }
        } else {
            if (minLength < 0) throw new ArgumentOutOfRangeException("minLength", "minLength is negative.");
            else if (minLength > 0) throw new ArgumentOutOfRangeException("minLength", "minLength is too large. The maximum string buffer length is approximately 2^30.");
            return new StringBuffer(null, 0, 0);
        }
    }

    public void Dispose() {
        int size = Length;
        Length = -1;
        if (size > 0) {
            lock (SyncRoot) {
                Allocated -= size;
                if (size <= MaxSegmentSize) {
                    FreeChunk prev = null;
                    FreeChunk next = Segment.FirstFreeChunk;
                    while (next != null && Index > next.Index) {
                        prev = next;
                        next = next.NextInSegment;
                    }
                    if (prev == null || prev.Index + prev.Size != Index) {
                        if (next != null && Index + size == next.Index) {
                            next.Index = Index;
                            next.Size += size;
                            var nextNext = next.NextInSize;
                            if (nextNext != null && next.Size > nextNext.Size) next.MoveAfterSizeHasIncreased();
                        } else {
                            new FreeChunk(Segment, prev, next, Index, size); // inserts itself into the lists
                        }
                    } else {
                        if (next != null && Index + size == next.Index) {
                            prev.Size += size + next.Size;
                            next.Remove();
                        } else {
                            prev.Size += size;
                        }
                        if (prev.NextInSize != null && prev.Size > prev.NextInSize.Size) prev.MoveAfterSizeHasIncreased();
                    }
                    Segment.AssertIntegrity();
                    var first = Segment.FirstFreeChunk;
                    if (first.Size == Segment.Size && ++NumberOfUnusedSegments > MaxNumberOfUnusedSegments) {
                        --NumberOfUnusedSegments;
                        first.Remove();
                        Segment.Dispose();
                    }
                } else { // size > MaxSegmentSize
                    Debug.Assert(size == Segment.Size);
                    Segment.Dispose();
                }
            }
        }
    }
}

}

#endif