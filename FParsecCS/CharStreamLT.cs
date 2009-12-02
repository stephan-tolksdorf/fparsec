// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

#if LOW_TRUST

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Diagnostics;

namespace FParsec {

public sealed class CharStream : IDisposable {
    private const int DefaultByteBufferLength = (1 << 12);
    private static int MinimumByteBufferLength = 128; // must be larger than longest detectable preamble (we can only guess here)
    private const char EOS = '\uFFFF';

    public Encoding Encoding { get; private set; }
    internal String String;
    internal int IndexBegin;
    internal int IndexEnd;
    internal long StreamIndexOffset;

    /// <summary>The index of the first char in the stream, i.e. Begin.Index.
    /// This value is determined by the streamBeginIndex argument of some of the CharStream constructors.
    /// By default this value is 0.</summary>
    public long BeginIndex { get { return StreamIndexOffset; } }

    /// <summary>The index of the last char of the stream plus 1.</summary>
    public long EndIndex   { get { return StreamIndexOffset + (IndexEnd - IndexBegin); } }

    [Obsolete("CharStream.IndexOffset has been renamed to CharStream.BeginIndex.")]
    public long IndexOffset { get { return BeginIndex; } }
    [Obsolete("CharStream.EndOfStream has been renamed to CharStream.EndIndex.")]
    public long EndOfStream { get { return EndIndex; } }

    internal CharStream(string chars) {
        Debug.Assert(chars != null);
        String = chars;
        //IndexBegin = 0;
        IndexEnd = chars.Length;
        //StreamIndexOffset = 0L;
    }

    /// <summary>Constructs a CharStream from the chars in the string argument between the indices index (inclusive) and index + length (exclusive).</summary>
    /// <exception cref="ArgumentNullException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0 and index + length ≤ chars.Length.</exception>
    public CharStream(string chars, int index, int length) : this(chars, index, length, 0) {}

    /// <summary>Constructs a CharStream from the chars in the string argument between the indices index (inclusive) and index + length (exclusive). The first char in the stream is assigned the index streamBeginIndex.</summary>
    /// <exception cref="ArgumentNullException">chars is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">At least one of the following conditions is not satisfied: index ≥ 0, length ≥ 0, index + length ≤ chars.Length and 0 ≤ streamBeginIndex &lt; 2^60.</exception>
    public CharStream(string chars, int index, int length, long streamBeginIndex) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (index < 0) throw new ArgumentOutOfRangeException("index", "The index is negative.");
        if (streamBeginIndex < 0 || streamBeginIndex >= (1L << 60)) throw new ArgumentOutOfRangeException("streamBeginIndex", "streamBeginIndex must be non-negative and less than 2^60.");
        int indexEnd = unchecked (index + length);
        if (indexEnd < index || indexEnd > chars.Length) throw new ArgumentOutOfRangeException("length", "The length is out of range.");

        String = chars;
        IndexBegin = index;
        IndexEnd = indexEnd;
        StreamIndexOffset = streamBeginIndex;
    }


    /// <summary>Constructs a CharStream from the file at the given path.<br/>Is equivalent to CharStream(new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan), false, encoding, true, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(string path, Encoding encoding)
           : this(path, encoding, true, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from the file at the given path.<br/>Is equivalent to CharStream(new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan), false, encoding, detectEncodingFromByteOrderMarks, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(path, encoding, detectEncodingFromByteOrderMarks, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from the file at the given path.<br/>Is equivalent to CharStream(new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan), false, encoding, detectEncodingFromByteOrderMarks, blockSize, blockOverlap, minRegexSpace, byteBufferLength).</summary>
    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength) {
        if (encoding == null) throw new ArgumentNullException("encoding");
        Stream stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, FileOptions.SequentialScan);
        try {
           StreamConstructorContinue(stream, false, encoding, detectEncodingFromByteOrderMarks, byteBufferLength);
        } catch {
            stream.Dispose();
            throw;
        }
    }

    /// <summary>Constructs a CharStream from a byte Stream.<br/>Is equivalent to CharStream(stream, false, encoding, true, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(Stream stream, Encoding encoding)
           : this(stream, false, encoding, true, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from a byte Stream.<br/>Is equivalent to CharStream(stream, leaveOpen, encoding, true, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(Stream stream, bool leaveOpen, Encoding encoding)
           : this(stream, leaveOpen, encoding, true, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from a byte Stream.<br/>Is equivalent to CharStream(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks, defaultBlockSize, defaultBlockSize/3, ((defaultBlockSize/3)*2)/3, defaultByteBufferLength).</summary>
    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks, DefaultByteBufferLength) { }

    /// <summary>Constructs a CharStream from a byte Stream.</summary>
    /// <param name="stream">The byte stream providing the input.</param>
    /// <param name="leaveOpen">Indicates whether the byte Stream should be left open when the CharStream has finished reading it.</param>
    /// <param name="encoding">The (default) Encoding used for decoding the byte Stream into chars.</param>
    /// <param name="detectEncodingFromByteOrderMarks">Indicates whether the constructor should detect the encoding from a unicode byte-order mark at the beginning of the stream. An encoding detected from a byte-order mark overrides the default encoding.</param>
    /// <param name="byteBufferLength">The size of the byte buffer used for decoding purposes. The default is 2^12 = 4KB.</param>
    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength) {
        if (stream == null) throw new ArgumentNullException("stream");
        if (!stream.CanRead) throw new ArgumentException("stream is not readable");
        if (encoding == null) throw new ArgumentNullException("encoding");
        StreamConstructorContinue(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks, byteBufferLength);
    }

    private void StreamConstructorContinue(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength) {
        // the ByteBuffer must be larger than the longest detectable preamble
        if (byteBufferLength < MinimumByteBufferLength) byteBufferLength = MinimumByteBufferLength;

        long streamPosition = 0;
        int bytesInStream = -1;
        if (stream.CanSeek) {
            streamPosition = stream.Position;
            long streamLength = stream.Length - streamPosition;
            if (streamLength <= Int32.MaxValue) {
                bytesInStream = (int) streamLength;
                if (bytesInStream < byteBufferLength) byteBufferLength = bytesInStream;
            }
        }

        byte[] byteBuffer = new byte[byteBufferLength];

        int byteBufferCount = 0;
        do {
            int c = stream.Read(byteBuffer, byteBufferCount, byteBuffer.Length - byteBufferCount);
            if (c > 0) byteBufferCount += c;
            else {
                bytesInStream = byteBufferCount;
                break;
            }
        } while (byteBufferCount < 16);
        int preambleLength = Helper.DetectPreamble(byteBuffer, byteBufferCount, ref encoding, detectEncodingFromByteOrderMarks);
        bytesInStream -= preambleLength;
        streamPosition += preambleLength;
        Encoding = encoding;

        if (bytesInStream == 0) {
            String = "";
            //Index = 0;
            //Length = 0;
            //StreamIndexOffset = 0L;
            return;
        }

        Decoder decoder = encoding.GetDecoder();
        int charBufferLength = encoding.GetMaxCharCount(byteBufferLength); // might throw
        char[] charBuffer = new char[charBufferLength];

        int stringBufferCapacity = 2*charBufferLength;
        if (bytesInStream > 0) {
            try {
                stringBufferCapacity = encoding.GetMaxCharCount(bytesInStream); // might throw
            } catch (ArgumentOutOfRangeException) { }
        }
        StringBuilder sb = new StringBuilder(stringBufferCapacity);
        if (byteBufferCount > preambleLength) {
            int charBufferCount;
            try {
                charBufferCount = decoder.GetChars(byteBuffer, preambleLength, byteBufferCount - preambleLength, charBuffer, 0, false);
                streamPosition += byteBufferCount - preambleLength;
            } catch (DecoderFallbackException e) {
                e.Data.Add("Stream.Position", streamPosition + e.Index);
                throw;
            }
            sb.Append(charBuffer, 0, charBufferCount);
        }
        for (;;) {
            byteBufferCount = stream.Read(byteBuffer, 0, byteBuffer.Length);
            bool flush = byteBufferCount == 0;
            int charBufferCount;
            try {
                charBufferCount = decoder.GetChars(byteBuffer, 0, byteBufferCount, charBuffer, 0, flush);
                streamPosition += byteBufferCount;
            } catch (DecoderFallbackException e) {
                e.Data.Add("Stream.Position", streamPosition + e.Index);
                throw;
            }
            sb.Append(charBuffer, 0, charBufferCount);
            if (flush) break;
        }
        String = sb.ToString();
        //Index = 0;
        IndexEnd = String.Length;
        //StreamIndexOffset = 0L;

        if (!leaveOpen) stream.Close();
    }

    /// <summary>The low trust version of the CharStream class implements the IDisposable
    /// interface only for API compatibility. The Dispose method does not need to be called on
    /// low trust CharStream instances, because the instances hold no resources that need to be disposed.</summary>
    public void Dispose() {}

    /// <summary>An iterator pointing to the beginning of the stream (or to the end if the CharStream is empty).</summary>
    public Iterator Begin { get {
        if (IndexBegin < IndexEnd)
            return new Iterator {Stream = this, Idx = IndexBegin};
        else
            return new Iterator {Stream = this, Idx = Int32.MinValue};
    } }

    /// <summary>Returns an iterator pointing to the given index in the stream,
    /// or to the end of the stream if the indexed position lies beyond the last char in the stream.</summary>
    /// <exception cref="ArgumentOutOfRangeException">The index is negative or less than the BeginIndex.</exception>
    public Iterator Seek(long index) {
        long idx = unchecked((uint)IndexBegin + index - StreamIndexOffset);
        if (idx >= IndexBegin && idx < IndexEnd)
            return new Iterator {Stream = this, Idx = (int)idx};
        if (index < BeginIndex)
            throw (new ArgumentOutOfRangeException("index", "The index is negative or less than the BeginIndex."));
        return new Iterator {Stream = this, Idx = Int32.MinValue};
    }

    /// <summary>The iterator type for CharStreams.</summary>
    public struct Iterator : IEquatable<Iterator>  {
        public CharStream Stream { get; internal set; }
        /// The index in Stream.String, or Int32.MinValue if the Iterator has reached the end of the stream
        internal int Idx;

        /// <summary>Indicates whether the Iterator points to the beginning of the CharStream.
        /// If the CharStream is empty, this property is always true.</summary>
        public bool IsBeginOfStream { get {
            var stream = Stream;
            return Idx == stream.IndexBegin || (Idx < 0 && stream.IndexBegin == stream.IndexEnd);
        } }

        /// <summary>Indicates whether the Iterator points to the end of the CharStream,
        /// i.e. whether it points to one char beyond the last char in the CharStream.</summary>
        public bool IsEndOfStream { get { return Idx < 0; } }

        /// <summary>The char returned by Read() if the iterator has
        /// reached the end of the stream. The value is '\uFFFF'.</summary>
        public const char EndOfStreamChar = EOS;

        /// <summary>The index of the stream char pointed to by the Iterator.</summary>
        public long Index { get {
            if (Idx >= 0)
                return Stream.StreamIndexOffset + (Idx - Stream.IndexBegin);
            else
                return Stream.StreamIndexOffset + (Stream.IndexEnd - Stream.IndexBegin);
        } }

        /// <summary>Returns an Iterator pointing to the next char in the stream. If the Iterator already
        /// has reached the end of the stream, i.e. if it points to one char beyond
        /// the last char, the same Iterator is returned.</summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public Iterator Next { get {
            int idx = Idx + 1;
            if (unchecked((uint)idx) < (uint)Stream.IndexEnd)
                return new Iterator {Stream = Stream, Idx = idx};
            else
                return new Iterator {Stream = Stream, Idx = Int32.MinValue};
        } }

        /// <summary>Returns an Iterator that is advanced by offset chars. The Iterator can't
        /// move past the end of the stream, i.e. any position beyond the last char
        /// in the stream is interpreted as precisely one char beyond the last char.</summary>
        /// <exception cref="ArgumentOutOfRangeException">The new position would lie before the beginning of the `CharStream`.</exception>
        public Iterator Advance(int offset) {
            var stream = Stream;
            int idx = unchecked(Idx + offset);
            if (offset < 0) goto Negative;
            if (unchecked((uint)idx) >= (uint)stream.IndexEnd) goto EndOfStream;
        ReturnIter:
            return new Iterator {Stream = stream, Idx = idx};
        EndOfStream:
            idx = Int32.MinValue;
            goto ReturnIter;
        Negative:
            if (Idx >= 0) {
                if (idx >= stream.IndexBegin) goto ReturnIter;
            } else {
                idx = stream.IndexEnd + offset;
                if (idx >= stream.IndexBegin) goto ReturnIter;
            }
            throw new ArgumentOutOfRangeException("offset");
        }

        /// <summary>Returns an Iterator that is advanced by offset chars. The Iterator can't
        /// move past the end of the stream, i.e. any position beyond the last char
        /// in the stream is interpreted as precisely one char beyond the last char.</summary>
        /// <exception cref="ArgumentOutOfRangeException">The new position would lie before the beginning of the `CharStream`.</exception>
        public Iterator Advance(long offset) {
            if (unchecked((int)offset) != offset) goto LargeNumber;
            int idx = unchecked(Idx + (int)offset);
            if ((int)offset < 0) goto Negative;
            if (unchecked((uint)idx) >= (uint)Stream.IndexEnd) goto EndOfStream;
        ReturnIter:
            return new Iterator {Stream = Stream, Idx = idx};
        EndOfStream:
            idx = Int32.MinValue;
            goto ReturnIter;
        Negative:
            if (Idx >= 0) {
                if (idx >= Stream.IndexBegin) goto ReturnIter;
            } else {
                idx = Stream.IndexEnd + (int)offset;
                if (idx >= Stream.IndexBegin) goto ReturnIter;
            }
        OutOfRange:
            throw new ArgumentOutOfRangeException("offset");
        LargeNumber:
            if (offset >= 0) goto EndOfStream;
            else goto OutOfRange;
        }

        /// <summary>Returns an Iterator that is advanced by offset chars. The Iterator can't
        /// move past the end of the stream, i.e. any position beyond the last char
        /// in the stream is interpreted as precisely one char beyond the last char.</summary>
        public Iterator Advance(uint offset) {
            int indexEnd = Stream.IndexEnd;
            int n = unchecked((int)offset);
            if (n >= 0) { // offset <= Int32.MaxValue
                int idx = unchecked(Idx + n);
                if (unchecked((uint)idx) < (uint)indexEnd) {
                    return new Iterator {Stream = Stream, Idx = idx};
                }
            }
            return new Iterator {Stream = Stream, Idx = Int32.MinValue};
        }

        /// <summary>Advances the Iterator *in-place* by 1 char and returns the char on the new position.
        ///`c &lt;- iter._Increment()` is equivalent to `iter &lt;- iter.Next; c &lt;- iter.Read()`.</summary>
        public char _Increment() {
            int idx = Idx + 1;
            var stream = Stream;
            if (unchecked((uint)idx) < (uint)stream.IndexEnd) {
                Idx = idx;
                return stream.String[idx];
            } else {
                Idx = Int32.MinValue;
                return EOS;
            }
        }

        /// <summary>Advances the Iterator *in-place* by offset chars and returns the char on the new position.
        /// `c &lt;- iter._Increment(offset)` is an optimized implementation of `iter &lt;- iter.Advance(offset); c &lt;- iter.Read()`.</summary>
        public char _Increment(uint offset) {
            var stream = Stream;
            int n = unchecked((int)offset);
            if (n >= 0) { // offset <= Int32.MaxValue
                int idx = unchecked(Idx + n);
                if (unchecked((uint)idx) < (uint)stream.IndexEnd) {
                    Idx = idx;
                    return stream.String[idx];
                }
            }
            Idx = Int32.MinValue;
            return EOS;
        }

        /// <summary>Advances the Iterator *in-place* by -1 char and returns the char on the new position,
        /// except if the Iterator already points to the beginning of the CharStream,
        /// in which case the position does not change and the EndOfStreamChar ('\uFFFF') is returned.</summary>
        public char _Decrement() {
            int idx = Idx;
            var stream = Stream;
            if (idx > stream.IndexBegin) {
                idx -= 1;
                Idx = idx;
                return stream.String[idx];
            } else if (idx < 0) {
                idx = stream.IndexEnd - 1;
                if (idx >= stream.IndexBegin)  {
                    Idx = idx;
                    return stream.String[idx];
                }
            }
            return EOS;
        }

        /// <summary>Advances the Iterator *in-place* by -numberOfChars chars and returns the char on the new position,
        /// except if the new position would lie before the beginning of the CharStream,
        /// in which case the Iterator is advanced to the beginning of the stream and the EndOfStreamChar ('\uFFFF') is returned.</summary>
        public char _Decrement(uint offset) {
            int idx = unchecked(Idx - (int)offset);
            var stream = Stream;
            if (idx < Idx && idx >= stream.IndexBegin) {
                Idx = idx;
                return stream.String[idx];
            } else if (offset != 0) {
                if (Idx < 0) {
                    int indexEnd = stream.IndexEnd;
                    idx = unchecked(indexEnd - (int)offset);
                    if (idx < indexEnd && idx >= stream.IndexBegin) {
                        Idx = idx;
                        return stream.String[idx];
                    }
                }
            } else return Read();
            Idx = stream.IndexBegin;
            return EOS;
        }

        /// <summary>Is an optimized implementation of Next.Read().</summary>
        public char Peek() {
            int idx = Idx + 1;
            var stream = Stream;
            if (unchecked((uint)idx) < (uint)stream.IndexEnd)
                return stream.String[idx];
            else
                return EOS;
        }

        /// <summary>Is an optimized implementation of Advance(offset).Read(),
        /// except that the EndOfStreamChar ('\uFFFF') is returned if Index + offset &lt; 0 (instead of an exception being thrown).</summary>
        public char Peek(int offset) {
            var stream = Stream;
            int idx = unchecked(Idx + offset);
            if (offset < 0) goto Negative;
            if (unchecked((uint)idx) >= (uint)stream.IndexEnd) goto EndOfStream;
        ReturnChar:
            return stream.String[idx];
        Negative:
            if (Idx >= 0) {
                if (idx >= stream.IndexBegin) goto ReturnChar;
            } else {
                idx = stream.IndexEnd + offset;
                if (idx >= stream.IndexBegin) goto ReturnChar;
            }
        EndOfStream:
            return EOS;
        }

        /// <summary>Is an optimized implementation of Advance(offset).Read().</summary>
        public char Peek(uint offset) {
            var stream = Stream;
            int n = unchecked((int)offset);
            if (n >= 0) { // offset <= Int32.MaxValue
                int idx = unchecked(Idx + n);
                if (unchecked((uint)idx) < (uint)stream.IndexEnd)
                    return stream.String[idx];
            }
            return EOS;
        }

        /// <summary>Returns true if and only if the char argument matches the char pointed to by the Iterator.
        /// At the end of the stream Match always returns false.</summary>
        public bool Match(char ch) {
            return Idx >= 0 && Stream.String[Idx] == ch;
        }

        /// <summary>Returns true if chars matches the chars in the stream beginning with the char pointed to by the Iterator.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If chars is empty, true is returned.</summary>
        /// <exception cref="NullReferenceException">chars is null.</exception>
        public bool Match(string chars) {
            if (unchecked((uint)Idx) + (uint)chars.Length <= (uint)Stream.IndexEnd) {
                var s = Stream.String;
                for (int i = 0; i < chars.Length; ++i)
                    if (chars[i] != s[Idx + i]) goto ReturnFalse;
                return true;
            }
            if (chars.Length == 0) return true;
        ReturnFalse:
            return false;
        }

        /// <summary>Returns true if caseFoldedChars matches the chars in the stream
        /// beginning with the char pointed to by the Iterator.
        /// The chars in the stream are case-folded before they are matched,
        /// while the chars in the string argument are assumed to already be case-folded.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If caseFoldedChars is empty, true is returned.</summary>
        /// <exception cref="NullReferenceException">caseFoldedChars is null.</exception>
        public bool MatchCaseFolded(string caseFoldedChars) {
            if (unchecked((uint)Idx) + (uint)caseFoldedChars.Length <= (uint)Stream.IndexEnd) {
                char[] cftable = CaseFoldTable.FoldedChars;
                if (cftable == null) cftable = CaseFoldTable.Initialize();
                var s = Stream.String;
                for (int i = 0; i < caseFoldedChars.Length; ++i)
                    if (caseFoldedChars[i] != cftable[s[Idx + i]]) goto ReturnFalse;
                return true;
            }
            if (caseFoldedChars.Length == 0) return true;
        ReturnFalse:
            return false;
        }

        /// <summary>Returns true if the chars in chars between the indices charsIndex (inclusive) and
        /// charsIndex + length (exclusive) match the chars in the stream beginning with the char pointed to by the Iterator.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If length is 0, true is returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">charsIndex is negative, length is negative or charsIndex + length > chars.Length.</exception>
        /// <exception cref="NullReferenceException">chars is null.</exception>
        public bool Match(string chars, int charsIndex, int length) {
            if (charsIndex < 0)
                throw new ArgumentOutOfRangeException("charsIndex", "charsIndex is negative.");
            if (length < 0 || charsIndex > chars.Length - length)
                throw new ArgumentOutOfRangeException("length", "length is out of range.");
            if (unchecked((uint)Idx) + (uint)length <= (uint)Stream.IndexEnd) {
                var s = Stream.String;
                for (int i = 0; i < length; ++i)
                    if (chars[charsIndex + i] != s[Idx + i]) goto ReturnFalse;
                return true;
            }
            if (length == 0) return true;
        ReturnFalse:
            return false;
        }

        /// <summary>Returns true if the chars in the char array between the indices charsIndex (inclusive) and
        /// charsIndex + length (exclusive) match the chars in the stream beginning with the char pointed to by the Iterator.
        /// If the chars do not match or if there are not enough chars remaining in the stream, false is returned.
        /// If length is 0, true is returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">charsIndex is negative, length is negative or charsIndex + length > chars.Length.</exception>
        /// <exception cref="NullReferenceException">chars is null.</exception>
        public bool Match(char[] chars, int charsIndex, int length) {
            if (charsIndex < 0)
                throw new ArgumentOutOfRangeException("charsIndex", "charsIndex is negative.");
            if (length < 0 || charsIndex > chars.Length - length)
                throw new ArgumentOutOfRangeException("length", "length is out of range.");
            if (unchecked((uint)Idx) + (uint)length <= (uint)Stream.IndexEnd) {
                var s = Stream.String;
                for (int i = 0; i < length; ++i)
                    if (chars[charsIndex + i] != s[Idx + i]) goto ReturnFalse;
                return true;
            }
            if (length == 0) return true;
        ReturnFalse:
            return false;
        }

        /// <summary>Applies the given regular expression to stream chars beginning with the char pointed to by the Iterator.
        /// Returns the resulting Match object.</summary>
        /// <remarks>For performance reasons you should specifiy the regular expression
        /// such that it can only match at the beginning of a string,
        /// for example by prepending "\A".<br/>
        /// For CharStreams constructed from large binary streams the regular expression is not applied
        /// to a string containing all the remaining chars in the stream. The minRegexSpace parameter
        /// of the CharStream constructors determines the minimum number of chars that are guaranteed
        /// to be visible to the regular expression.</remarks>
        /// <exception cref="NullReferenceException">regex is null.</exception>
        public Match Match(Regex regex) {
            int idx = Idx;
            var stream = Stream;
            if (idx >= 0) return regex.Match(stream.String, idx, stream.IndexEnd - idx);
            else return regex.Match("");
        }

        /// <summary>Returns the stream char pointed to by the Iterator,
        /// or the EndOfStreamChar ('\uFFFF') if the Iterator has reached the end of the stream.</summary>
        public char Read() {
            int idx = Idx;
            if (idx >= 0) return Stream.String[idx];
            else return EOS;
        }

        public struct TwoChars : IEquatable<TwoChars> {
            private uint chars;

            internal TwoChars(uint chars) {
                this.chars = chars;
            }
            public TwoChars(char char0, char char1) {
                this.chars = ((uint)char1 << 16) | (uint)char0;
            }

            public char Char0 { get { return unchecked((char)chars); } }
            public char Char1 { get { return (char)(chars >> 16); } }

            public override bool Equals(object obj) { return (obj is TwoChars) && chars == ((TwoChars) obj).chars; }
            public bool Equals(TwoChars other) { return chars == other.chars; }
            public override int GetHashCode()  { return (int)chars; }
            public static bool operator==(TwoChars left, TwoChars right) { return left.chars == right.chars; }
            public static bool operator!=(TwoChars left, TwoChars right) { return left.chars != right.chars; }
        }

        /// <summary>Is an optimized implementation of new TwoChars(Read(), Next.Read()).</summary>
        public TwoChars Read2() {
            int idx = Idx + 1;
            var stream = Stream;
            if (unchecked((uint)idx) < (uint)stream.IndexEnd) {
                var s = stream.String;
                return new TwoChars(s[idx - 1], s[idx]);
            } else if (idx == stream.IndexEnd) {
                return new TwoChars(stream.String[idx - 1], EOS);
            } else {
                return new TwoChars(EOS, EOS);
            }
        }

        /// <summary>Returns a string with the length stream chars beginning with the char pointed to by the Iterator.
        /// If less than length chars are remaining in the stream, only the remaining chars are returned.</summary>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        /// <exception cref="OutOfMemoryException">There is not enough memory for the string or the requested string is too large.</exception>
        public string Read(int length) {
            if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
            int idx = Idx;
            var stream = Stream;
            if (unchecked((uint)idx) + (uint)length <= (uint)stream.IndexEnd)
                return stream.String.Substring(idx, length);
            else
                return idx >= 0 ? stream.String.Substring(idx, stream.IndexEnd - idx) : "";
        }

        /// <summary>Returns a string with the length stream chars beginning with the char pointed to by the Iterator.
        /// If less than length chars are remaining in the stream,
        /// only the remaining chars are returned, or an empty string if allOrEmpty is true.</summary>
        /// <exception cref="ArgumentOutOfRangeException">length is negative.</exception>
        /// <exception cref="OutOfMemoryException">There is not enough memory for the string or the requested string is too large.</exception>
        public string Read(int length, bool allOrEmpty) {
            if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
            if (unchecked((uint)Idx) + (uint)length <= (uint)Stream.IndexEnd)
                return Stream.String.Substring(Idx, length);
            else
                return Idx >= 0 && !allOrEmpty ? Stream.String.Substring(Idx, Stream.IndexEnd - Idx) : "";
        }

        /// <summary>Copies the length stream chars beginning with the char pointed to by the Iterator into buffer.
        /// The chars are written into buffer beginning at the index bufferIndex.
        /// If less than length chars are remaining in the stream, only the remaining chars are copied.
        /// Returns the actual number of chars copied.</summary>
        /// <exception cref="ArgumentOutOfRangeException">bufferIndex is negative, length is negative or bufferIndex + length > buffer.Length.</exception>
        /// <exception cref="NullReferenceException">buffer is null.</exception>
        public int Read(char[] buffer, int bufferIndex, int length) {
            if (bufferIndex < 0)
                throw new ArgumentOutOfRangeException("bufferIndex", "bufferIndex is negative.");
            if (length < 0 || bufferIndex > buffer.Length - length)
                throw new ArgumentOutOfRangeException("length", "length is out of range.");
            if (unchecked((uint)Idx) + (uint)length <= (uint)Stream.IndexEnd) {
                var s = Stream.String;
                for (int i = 0; i < length; ++i)
                    buffer[bufferIndex + i] = s[Idx + i];
                return length;
            } else if (Idx >= 0){
                int n = Stream.IndexEnd - Idx;
                var s = Stream.String;
                for (int i = 0; i < n; ++i)
                    buffer[bufferIndex + i] = s[Idx + i];
                return n;
            } else {
                return 0;
            }
        }

        /// <summary>Returns a string with all the chars in the stream between the position of this Iterator (inclusive)
        /// and the position of the Iterator in the argument (exclusive).
        /// If the Iterator argument does not point to a position after the position of this Iterator, the returned string is empty.</summary>
        /// <exception cref="ArgumentOutOfRangeException">iterToCharAfterLastInString belongs to a different CharStream.</exception>
        /// <exception cref="OutOfMemoryException">There is not enough memory for the string or the requested string is too large.</exception>
        public string ReadUntil(Iterator iterToCharAfterLastInString) {
            var stream = Stream;
            int idx = Idx;
            if (stream != iterToCharAfterLastInString.Stream)
                throw new ArgumentOutOfRangeException("iterToCharAfterLastInString", "The iterator argument belongs to a different CharStream.");
            if (idx >= 0) {
                if (idx <= iterToCharAfterLastInString.Idx)
                    return stream.String.Substring(idx, iterToCharAfterLastInString.Idx - idx);
                if (iterToCharAfterLastInString.Idx < 0)
                    return stream.String.Substring(idx, stream.IndexEnd - idx);
            }
            return "";
        }

        public override bool Equals(object obj) {
            return (obj is Iterator) && Equals((Iterator) obj);
        }

        public bool Equals(Iterator other) {
            return Idx == other.Idx && Stream == other.Stream;
        }

        public override int GetHashCode() {
            return Idx;
        }

        public static bool operator==(Iterator left, Iterator right) { return  left.Equals(right); }
        public static bool operator!=(Iterator left, Iterator right) { return !left.Equals(right); }
    }

    /// <summary>Returns a case-folded copy of the string argument. All chars are mapped
    /// using the (non-Turkic) 1-to-1 case folding mappings (v. 5.1) for Unicode code
    /// points in the Basic Multilingual Plane, i.e. code points below 0x10000.
    /// If the argument is null, null is returned.</summary>
    static public string FoldCase(string str) {
        char[] cftable = CaseFoldTable.FoldedChars;
        if (cftable == null) cftable = CaseFoldTable.Initialize();
        if (str != null) {
            for (int i = 0; i < str.Length; ++i) {
                char c   = str[i];
                char cfc = cftable[c];
                if (c != cfc) {
                    StringBuilder sb = new StringBuilder(str);
                    sb[i++] = cfc;
                    for (; i < str.Length; ++i) {
                        c   = str[i];
                        cfc = cftable[c];
                        if (c != cfc) sb[i] = cfc;
                    }
                    return sb.ToString();
                }
            }
        }
        return str;
    }

    /// <summary>Returns the given string with all occurrences of "\r\n" and "\r" replaced
    /// by "\n". If the argument is null, null is returned.</summary>
    static public string NormalizeNewlines(string str) {
        if (str == null || str.Length == 0) return str;
        int nCR   = 0;
        int nCRLF = 0;
        for (int i = 0; i < str.Length; ++i) {
            if (str[i] == '\r') {
                if (i + 1 < str.Length && str[i + 1] == '\n') ++nCRLF;
                else ++nCR;
            }
        }
        if (nCRLF == 0) {
            return nCR == 0 ? str : str.Replace('\r', '\n');
        } else {
            return CopyWithNormalizedNewlines(str, 0, str.Length, nCRLF, nCR);
        }
    }
    static internal string CopyWithNormalizedNewlines(string src, int index, int length, int nCRLF, int nCR) {
        Debug.Assert(length > 0 && nCRLF >= 0 && nCR >= 0 && (nCRLF | nCR) != 0);
        if (nCRLF != 0) {
            StringBuilder sb = new StringBuilder(length - nCRLF);
            int end = index + length;
            int i0 = index;
            if (nCR == 0) {
                int nn = nCRLF;
                int i = index;
                for (;;) {
                    char c = src[i++];
                    if (c == '\r') {
                        sb.Append(src, i0, i - i0 - 1).Append('\n');
                        ++i; // skip over the '\n' in "\r\n"
                        i0 = i;
                        if (--nn == 0) break;
                    }
                }
            } else {
                int nn = nCRLF + nCR;
                int i = index;
                for (;;) {
                    char c = src[i++];
                    if (c == '\r') {
                        sb.Append(src, i0, i - i0 - 1).Append('\n');
                        if (i < end && src[i] == '\n') ++i; // skip over the '\n' in "\r\n"
                        i0 = i;
                        if (--nn == 0) break;
                    }
                }
            }
            if (i0 < end) sb.Append(src, i0, end - i0);
            return sb.ToString();
        } else {
            return new StringBuilder(src, index, length, length).Replace('\r', '\n').ToString();
        }
    }
} // class CharStream

}

#endif