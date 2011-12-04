// Copyright (c) Stephan Tolksdorf 2007-2011
// License: Simplified BSD License. See accompanying documentation.

#if LOW_TRUST

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.FSharp.Core;

namespace FParsec {


/// <summary>An opaque representation of a CharStream index.</summary>
public struct CharStreamIndexToken {
#if DEBUG
    internal readonly CharStream CharStream;
    private long Index { get { return GetIndex(CharStream); } }
#endif
    private readonly int IdxPlus1;
    /// <summary>Returns -1 if the IndexToken was zero-initialized.</summary>
    internal int Idx { get { return unchecked(IdxPlus1 - 1); } }

    internal CharStreamIndexToken(
                              #if DEBUG
                                  CharStream charStream,
                              #endif
                                  int idx)
    {
    #if DEBUG
        CharStream = charStream;
    #endif
        IdxPlus1 = unchecked(idx + 1);
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void ThrowInvalidIndexToken() {
        throw new InvalidOperationException("The CharStreamIndexToken is invalid.");
    }

    public long GetIndex(CharStream charStreamFromWhichIndexTokenWasRetrieved) {
        int idx = Idx;
        if (idx == -1) ThrowInvalidIndexToken(); // tests for a zero-initialized IndexToken
    #if DEBUG
        Debug.Assert(CharStream == charStreamFromWhichIndexTokenWasRetrieved);
    #endif
        return charStreamFromWhichIndexTokenWasRetrieved.GetIndex(idx);
    }
}

public struct TwoChars : IEquatable<TwoChars> {
    private uint Chars;

    internal TwoChars(uint chars) {
        Chars = chars;
    }
    public TwoChars(char char0, char char1) {
        Chars = ((uint)char1 << 16) | (uint)char0;
    }

    public char Char0 { get { return unchecked((char)Chars); } }
    public char Char1 { get { return (char)(Chars >> 16); } }

    public override bool Equals(object obj) { return (obj is TwoChars) && Chars == ((TwoChars) obj).Chars; }
    public bool Equals(TwoChars other) { return Chars == other.Chars; }
    public override int GetHashCode()  { return unchecked((int)Chars); }
    public static bool operator==(TwoChars left, TwoChars right) { return left.Chars == right.Chars; }
    public static bool operator!=(TwoChars left, TwoChars right) { return left.Chars != right.Chars; }
}

/// <summary>Provides read‐access to a sequence of UTF‐16 chars.</summary>
public class CharStream : IDisposable {
    private const int DefaultByteBufferLength = (1 << 12);
    private static int MinimumByteBufferLength = 128; // must be larger than longest detectable preamble (we can only guess here)
    private const char EOS = '\uFFFF';

    public const char EndOfStreamChar = EOS;

    public int BlockOverlap { get { return 0; } }

    public int MinRegexSpace { get { return 0; }
                               set {           } }

    internal String String;

    /// <summary>The current index in the string, or Int32.MinValue if the end of the stream has been reached.</summary>
    internal int Idx;

    /// <summary>Index of the first char in the string belonging to the stream. Is always non-negative.</summary>
    internal int IndexBegin;
    /// <summary>1 + index of the last char in the string belonging to the stream. Equals IndexBegin if the stream is empty.</summary>
    internal int IndexEnd;

    /// <summary>Any CharStream method or property setter increments this value when it changes the CharStream state.
    /// Backtracking to an old state also restores the old value of the StateTag.</summary>
    public
#if SMALL_STATETAG
           uint
#else
           ulong
#endif
                 StateTag;

    /// <summary>IndexOfFirstChar - IndexBegin</summary>
    internal long StringToStreamIndexOffset;

    public long IndexOfFirstChar     { get { return (uint)IndexBegin + StringToStreamIndexOffset; } }
    public long IndexOfLastCharPlus1 { get { return (uint)IndexEnd + StringToStreamIndexOffset; } }

    public long Index { get {
        // return GetIndex(Idx);
        if (Idx >= 0) {
            Debug.Assert(Idx >= IndexBegin && Idx < IndexEnd);
            return (uint)Idx + StringToStreamIndexOffset;
        } else {
            Debug.Assert(Idx == Int32.MinValue);
            return (uint)IndexEnd + StringToStreamIndexOffset;
        }
    } }
    internal long GetIndex(int idx) {
        if (idx >= 0) {
            Debug.Assert(idx >= IndexBegin && idx < IndexEnd);
            return (uint)idx + StringToStreamIndexOffset;
        } else {
            Debug.Assert(idx == Int32.MinValue);
            return (uint)IndexEnd + StringToStreamIndexOffset;
        }
    }

    /// <summary>Indicates whether the Iterator points to the beginning of the CharStream.
    /// If the CharStream is empty, this property is always true.</summary>
    public bool IsBeginOfStream { get {
        return Idx == IndexBegin || (Idx < 0 && IndexBegin == IndexEnd);
    } }

    /// <summary>Indicates whether the Iterator points to the end of the CharStream,
    /// i.e. whether it points to one char beyond the last char in the CharStream.</summary>
    public bool IsEndOfStream { get { return Idx < 0; } }

    internal long _Line;
    public long Line { get { return _Line; } }
    public void SetLine_WithoutCheckAndWithoutIncrementingTheStateTag(long line) {
        _Line = line;
    }

    internal long _LineBegin;
    public long LineBegin { get { return _LineBegin; } }
    public void SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag(long lineBegin) {
        _LineBegin = lineBegin;
    }

    /// <summary>The UTF‐16 column number of the next char, i.e. Index ‐ LineBegin  + 1.</summary>
    public long Column { get { return Index - LineBegin + 1; } }

    internal string _Name;
    public string Name {
        get { return _Name; }
        set { _Name = value; ++StateTag; }
    }

    public Encoding Encoding { get; private set; }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public Position Position { get {
        long index = Index;
        return new Position(_Name, index, Line, index - LineBegin + 1);
    } }

    internal CharStream(string chars) {
        Debug.Assert(chars != null);
        String = chars;
        Encoding = Encoding.Unicode;
        _Line = 1;
        var length = chars.Length;
        if (length != 0) {
            // Idx = 0
            IndexEnd = length;
        } else {
            Idx = Int32.MinValue;
            // IndexEnd = 0
        }
    }

    public CharStream(string chars, int index, int length) : this(chars, index, length, 0) {}

    public CharStream(string chars, int index, int length, long streamBeginIndex) {
        if (chars == null) throw new ArgumentNullException("chars");
        if (index < 0) throw new ArgumentOutOfRangeException("index", "index is negative.");
        if (streamBeginIndex < 0 || streamBeginIndex >= (1L << 60)) throw new ArgumentOutOfRangeException("streamBeginIndex", "streamBeginIndex must be non-negative and less than 2^60.");
        int indexEnd = unchecked(index + length);
        if (indexEnd < index || indexEnd > chars.Length) throw new ArgumentOutOfRangeException("length", "index or length is out of range.");
        String = chars;
        Encoding = Encoding.Unicode;
        _Line = 1;
        Idx = length == 0 ? Int32.MinValue : index;
        IndexBegin = index;
        IndexEnd = indexEnd;
        _LineBegin = streamBeginIndex;
        StringToStreamIndexOffset = streamBeginIndex - index;
    }

    public CharStream(string path, Encoding encoding)
           : this(path, encoding, true, DefaultByteBufferLength) { }

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(path, encoding, detectEncodingFromByteOrderMarks, DefaultByteBufferLength) { }

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength) {
        if (encoding == null) throw new ArgumentNullException("encoding");
        Stream stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096
                                  #if !SILVERLIGHT
                                      , FileOptions.SequentialScan
                                  #endif
                                      );
        try {
           StreamConstructorContinue(stream, false, encoding, detectEncodingFromByteOrderMarks, byteBufferLength);
        } catch {
            stream.Dispose();
            throw;
        }
    }

    public CharStream(Stream stream, Encoding encoding)
           : this(stream, false, encoding, true, DefaultByteBufferLength) { }

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding)
           : this(stream, leaveOpen, encoding, true, DefaultByteBufferLength) { }

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : this(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks, DefaultByteBufferLength) { }

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength) {
        if (stream == null) throw new ArgumentNullException("stream");
        if (!stream.CanRead) throw new ArgumentException("stream is not readable");
        if (encoding == null) throw new ArgumentNullException("encoding");
        StreamConstructorContinue(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks, byteBufferLength);
    }

    private void StreamConstructorContinue(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength) {
        // the ByteBuffer must be larger than the longest detectable preamble
        if (byteBufferLength < MinimumByteBufferLength) byteBufferLength = MinimumByteBufferLength;

        int remainingBytesCount = -1;
        long streamPosition;
        if (stream.CanSeek) {
            streamPosition = stream.Position;
            long remainingBytesCount64 = stream.Length - streamPosition;
            if (remainingBytesCount64 <= Int32.MaxValue) {
                remainingBytesCount = (int)remainingBytesCount64;
                if (remainingBytesCount < byteBufferLength) byteBufferLength = remainingBytesCount;
            }
        } else {
            streamPosition = 0;
        }

        // byteBufferLength should be larger than the longest detectable preamble
        byte[] byteBuffer = new byte[byteBufferLength];
        int byteBufferCount = 0;
        bool flush = false;
        do {
            int n = stream.Read(byteBuffer, byteBufferCount, byteBuffer.Length - byteBufferCount);
            if (n == 0) {
                remainingBytesCount = byteBufferCount;
                flush = true;
                break;
            }
            byteBufferCount += n;
        } while (byteBufferCount < MinimumByteBufferLength);
        streamPosition += byteBufferCount;

        int preambleLength = Text.DetectPreamble(byteBuffer, byteBufferCount, ref encoding, detectEncodingFromByteOrderMarks);
        remainingBytesCount -= preambleLength;
        Encoding = encoding;
        _Line = 1;
        if (remainingBytesCount != 0) {
            int charBufferLength = encoding.GetMaxCharCount(byteBufferLength); // might throw
            char[] charBuffer = new char[charBufferLength];
            int stringBufferCapacity = 2*charBufferLength;
            if (remainingBytesCount > 0) {
                try {
                    stringBufferCapacity = encoding.GetMaxCharCount(remainingBytesCount); // might throw
                } catch (ArgumentOutOfRangeException) { }
            }
            var sb = new StringBuilder(stringBufferCapacity);
            var decoder = encoding.GetDecoder();
            Debug.Assert(preambleLength < byteBufferCount);
            int byteBufferIndex = preambleLength;
            for (;;) {
                try {
                    int charBufferCount = decoder.GetChars(byteBuffer, byteBufferIndex, byteBufferCount - byteBufferIndex, charBuffer, 0, flush);
                    sb.Append(charBuffer, 0, charBufferCount);
                } catch (DecoderFallbackException e) {
                    e.Data.Add("Stream.Position", streamPosition - (byteBufferCount - byteBufferIndex) + e.Index);
                    throw;
                }
                if (flush) break;
                byteBufferIndex = 0;
                byteBufferCount = stream.Read(byteBuffer, 0, byteBuffer.Length);
                streamPosition += byteBufferCount;
                flush = byteBufferCount == 0;
            }
            String = sb.ToString();
            if (!leaveOpen) stream.Close();
        } else {
            String = "";
        }
        if (String.Length != 0) {
            // Idx = 0
            IndexEnd = String.Length;
        } else {
            Idx = Int32.MinValue;
            // IndexEnd = 0
        }
    }

    /// <summary>The low trust version of the CharStream class implements the IDisposable
    /// interface only for API compatibility. The Dispose method does not need to be called on
    /// low trust CharStream instances, because the instances hold no resources that need to be disposed.</summary>
    public void Dispose() {}

    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:Dispose objects before losing scope", Justification="The CharStream is manually disposed.")]
    public static T ParseString<T,TUserState>(string chars, int index, int length,
                                              FSharpFunc<CharStream<TUserState>,T> parser,
                                              TUserState userState,
                                              string streamName)
    {
        var stream = new CharStream<TUserState>(chars, index, length);
        stream.UserState = userState;
        stream._Name = streamName;
        return parser.Invoke(stream);
    }

    public void Seek(long index) {
        long idx = unchecked(index - StringToStreamIndexOffset);
        if (idx >= IndexBegin && idx < IndexEnd) {
            Idx = (int)idx;
            ++StateTag;
            return;
        }
        if (index < IndexOfFirstChar)
            throw (new ArgumentOutOfRangeException("index", "The index is negative or less than the IndexOfFirstChar."));
        ++StateTag;
        Idx = Int32.MinValue;
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public CharStreamIndexToken IndexToken { get {
        return new CharStreamIndexToken(
                   #if DEBUG
                       this,
                   #endif
                       Idx
                   );
    } }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void ThrowInvalidIndexToken() {
        throw new ArgumentException("The CharStreamIndexToken is invalid.");
    }

    public void Seek(CharStreamIndexToken indexToken) {
        int idx = indexToken.Idx;
        if (idx == -1) ThrowInvalidIndexToken(); // tests for zero-initialized IndexTokens
    #if DEBUG
        Debug.Assert(this == indexToken.CharStream);
    #endif
        Idx = idx;
        Debug.Assert((Idx >= IndexBegin && Idx < IndexEnd) || Idx == Int32.MinValue);
        ++StateTag;
    }

    public string ReadFrom(CharStreamIndexToken indexToken) {
        int idx = indexToken.Idx;
        if (idx == -1) ThrowInvalidIndexToken(); // tests for zero-initialized IndexTokens
    #if DEBUG
        Debug.Assert(this == indexToken.CharStream);
    #endif
        return ReadFrom(idx);
    }

    internal string ReadFrom(int idx0) {
        if (idx0 >= 0) {
            Debug.Assert(idx0 >= IndexBegin && idx0 < IndexEnd);
            if (idx0 <= Idx)
                return String.Substring(idx0, Idx - idx0);
            if (Idx < 0)
                return String.Substring(idx0, IndexEnd - idx0);
        } else {
            Debug.Assert(idx0 == Int32.MinValue);
            if (Idx < 0) return "";
        }
        throw new ArgumentException("The current position of the stream must not lie before the position corresponding to the given CharStreamIndexToken/CharStreamState.");
    }

    public void RegisterNewline() {
        ++_Line;
        var index = Index;
        Debug.Assert(index != _LineBegin);
        _LineBegin = index;
        ++StateTag;
    }

    private void RegisterNewLineBegin(int stringLineBegin, int lineOffset) {
        Debug.Assert(lineOffset > 0
                     && ((Idx >= stringLineBegin && Idx < IndexEnd) || Idx == Int32.MinValue)
                     && stringLineBegin >= IndexBegin && stringLineBegin <= IndexEnd);
        _Line += lineOffset;
        long newLineBegin = (uint)stringLineBegin + StringToStreamIndexOffset;
        Debug.Assert(newLineBegin > _LineBegin);
        _LineBegin = newLineBegin;
        ++StateTag;
    }

    public void RegisterNewlines(int lineOffset, int newColumnMinus1) {
        _Line += lineOffset;
        Debug.Assert(_Line > 0 && newColumnMinus1 >= 0);
        var newLineBegin = Index - newColumnMinus1;
        Debug.Assert(lineOffset != 0 && newLineBegin != _LineBegin);
        _LineBegin = Index - newColumnMinus1;
        ++StateTag;
    }

    public void RegisterNewlines(long lineOffset, long newColumnMinus1) {
        _Line += lineOffset;
        Debug.Assert(_Line > 0 && newColumnMinus1 >= 0);
        var newLineBegin = Index - newColumnMinus1;
        Debug.Assert(lineOffset != 0 && newLineBegin != _LineBegin);
        _LineBegin = Index - newColumnMinus1;
        ++StateTag;
    }


    public char Peek() {
        int idx = Idx;
        if (idx >= 0) return String[idx];
        return EOS;
    }

    public void Skip() {
        int idx = Idx + 1;
        if (unchecked((uint)idx) < (uint)IndexEnd) {
            Idx = idx;
            ++StateTag;
        } else if (idx == IndexEnd) {
            Idx = Int32.MinValue;
            ++StateTag;
        }
    }

    public char Read() {
        int idx = Idx;
        if (idx >= 0)  {
            char c = String[idx];
            ++idx;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            ++StateTag;
            return c;
        }
        return EOS;
    }

    public char SkipAndPeek() {
        int idx = Idx + 1;
        if (unchecked((uint)idx) < (uint)IndexEnd) {
            Idx = idx;
            ++StateTag;
            return String[idx];
        } else if (idx == IndexEnd) {
            Idx = Int32.MinValue;
            ++StateTag;
        }
        return EOS;
    }

    public TwoChars Peek2() {
        int idx = Idx + 1;
        if (unchecked((uint)idx) < (uint)IndexEnd)
            return new TwoChars(String[idx - 1], String[idx]);
        else if (idx == IndexEnd)
            return new TwoChars(String[idx - 1], EOS);
        else
            return new TwoChars(EOS, EOS);
    }

    public char Peek(uint utf16Offset) {
        int n = unchecked((int)utf16Offset);
        if (n >= 0) { // utf16Offset <= Int32.MaxValue
            int idx = unchecked(Idx + n);
            if (unchecked((uint)idx) < (uint)IndexEnd)
                return String[idx];
        }
        return EOS;
    }

    public void Skip(uint utf16Offset) {
        ++StateTag;
        int n = unchecked((int)utf16Offset);
        if (n >= 0) { // utf16Offset <= Int32.MaxValue
            int idx = unchecked(Idx + n);
            if (unchecked((uint)idx) < (uint)IndexEnd) {
                Idx = idx;
                return;
            }
        }
        Idx = Int32.MinValue;
        return;
    }

    public char SkipAndPeek(uint utf16Offset) {
        ++StateTag;
        int n = unchecked((int)utf16Offset);
        if (n >= 0) { // utf16Offset <= Int32.MaxValue
            int idx = unchecked(Idx + n);
            if (unchecked((uint)idx) < (uint)IndexEnd) {
                Idx = idx;
                return String[idx];
            }
        }
        Idx = Int32.MinValue;
        return EOS;
    }

    public char Peek(int utf16Offset) {
        int idx = unchecked(Idx + utf16Offset);
        if (utf16Offset < 0) goto Negative;
        if (unchecked((uint)idx) >= (uint)IndexEnd) goto EndOfStream;
    ReturnChar:
        return String[idx];
    Negative:
        if (Idx >= 0) {
            if (idx >= IndexBegin) goto ReturnChar;
        } else {
            idx = IndexEnd + utf16Offset;
            if (idx >= IndexBegin) goto ReturnChar;
        }
    EndOfStream:
        return EOS;
    }

    public void Skip(int utf16Offset) {
        ++StateTag;
        int idx = unchecked(Idx + utf16Offset);
        if (utf16Offset < 0) goto Negative;
        if (unchecked((uint)idx) >= (uint)IndexEnd) goto EndOfStream;
    Return:
        Idx = idx;
        return;
    Negative:
        if (Idx >= 0) {
            if (idx >= IndexBegin) goto Return;
        } else {
            idx = IndexEnd + utf16Offset;
            if (idx >= IndexBegin) goto Return;
        }
        --StateTag;
        throw new ArgumentOutOfRangeException("utf16Offset", "Index + utf16Offset is negative or less than the index of the first char in the CharStream.");
    EndOfStream:
        idx = Int32.MinValue;
        goto Return;
    }

    public void Skip(long utf16Offset) {
        if (unchecked((int)utf16Offset) == utf16Offset) {
            Skip((int)utf16Offset);
        } else {
            if (utf16Offset < 0) throw new ArgumentOutOfRangeException("utf16Offset", "Index + utf16Offset is negative or less than the index of the first char in the CharStream.");
            ++StateTag;
            Idx = Int32.MinValue;
        }
    }

    public char SkipAndPeek(int utf16Offset) {
        ++StateTag;
        int idx = unchecked(Idx + utf16Offset);
        if (utf16Offset < 0) goto Negative;
        if (unchecked((uint)idx) >= (uint)IndexEnd) goto EndOfStream;
    ReturnChar:
        Idx = idx;
        return String[idx];
    Negative:
        if (Idx >= 0) {
            if (idx >= IndexBegin) goto ReturnChar;
        } else {
            idx = IndexEnd + utf16Offset;
            if (idx >= IndexBegin) goto ReturnChar;
            if (IndexBegin == IndexEnd) goto EndOfStream;
        }
        Idx = IndexBegin;
        return EOS;
    EndOfStream:
        Idx = Int32.MinValue;
        return EOS;
    }

    public string PeekString(int length) {
        if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
        int idx = Idx;
        if (unchecked((uint)idx) + (uint)length <= (uint)IndexEnd)
            return String.Substring(idx, length);
        else
            return idx < 0 ? "" : String.Substring(idx, IndexEnd - idx);
    }

    public string Read(int length) {
        if (length < 0) throw new ArgumentOutOfRangeException("length", "length is negative.");
        ++StateTag;
        var idx = Idx;
        int newIdx = unchecked(idx + length);
        if (unchecked((uint)newIdx) < (uint)IndexEnd) {
            Idx = newIdx;
            return String.Substring(idx, length);
        } else {
            Idx = Int32.MinValue;
            return idx < 0 ? "" : String.Substring(idx, IndexEnd - idx);
        }
    }

    public int PeekString(char[] buffer, int bufferIndex, int length) {
        return Read(buffer, bufferIndex, length, true);
    }
    public int Read(char[] buffer, int bufferIndex, int length) {
        return Read(buffer, bufferIndex, length, false);
    }
    private int Read(char[] buffer, int bufferIndex, int length, bool backtrack) {
        if (bufferIndex < 0)
            throw new ArgumentOutOfRangeException("bufferIndex", "bufferIndex is negative.");
        if (length < 0 || bufferIndex > buffer.Length - length)
            throw new ArgumentOutOfRangeException("length", "bufferIndex or length is out of range.");
        if (unchecked((uint)Idx) + (uint)length < (uint)IndexEnd) {
            for (int i = 0; i < length; ++i)
                buffer[bufferIndex + i] = String[Idx + i];
            if (!backtrack) {
                Idx += length;
                ++StateTag;
            }
            return length;
        } else if (Idx >= 0) {
            int n = IndexEnd - Idx;
            for (int i = 0; i < n; ++i)
                buffer[bufferIndex + i] = String[Idx + i];
            if (!backtrack) {
                Idx = Int32.MinValue;
                ++StateTag;
            }
            return n;
        } else {
            return 0;
        }
    }

    public bool Match(char ch) {
        return Idx >= 0 && String[Idx] == ch;
    }

    public bool MatchCaseFolded(char caseFoldedChar) {
        return Idx >= 0 &&  CaseFoldTable.FoldedChars[String[Idx]] == caseFoldedChar;
    }

    public bool Skip(char ch) {
        int idx = Idx;
        if (idx >= 0 && String[idx] == ch)  {
            ++idx;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            ++StateTag;
            return true;
        }
        return false;
    }

    public bool SkipCaseFolded(char caseFoldedChar) {
        int idx = Idx;
        if (idx >= 0 && CaseFoldTable.FoldedChars[String[idx]] == caseFoldedChar)  {
            ++idx;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            ++StateTag;
            return true;
        }
        return false;
    }

    public bool Skip(TwoChars twoChars) {
        int idx2 = unchecked(Idx + 2);
        if (unchecked((uint)idx2) < (uint)IndexEnd) {
            if (String[Idx] == twoChars.Char0 && String[Idx + 1] == twoChars.Char1) {
                ++StateTag;
                Idx = idx2;
                return true;
            }
        } else if (idx2 == IndexEnd && String[Idx] == twoChars.Char0 && String[Idx + 1] == twoChars.Char1) {
            ++StateTag;
            Idx = Int32.MinValue;
            return true;
        }
        return false;
    }

    public bool Match(string chars) {
        if (unchecked((uint)Idx) + (uint)chars.Length <= (uint)IndexEnd) {
            for (int i = 0; i < chars.Length; ++i)
                if (chars[i] != String[Idx + i]) goto ReturnFalse;
            return true;
        }
        return chars.Length == 0;
    ReturnFalse:
        return false;
    }

    public bool Skip(string chars) {
        int newIdx = unchecked(Idx + chars.Length);
        if (unchecked((uint)newIdx) <= (uint)IndexEnd) {
            for (int i = 0; i < chars.Length; ++i)
                if (chars[i] != String[Idx + i]) goto ReturnFalse;
            if (newIdx == IndexEnd) newIdx = Int32.MinValue;
            Idx = newIdx;
            ++StateTag;
            return true;
        }
        return chars.Length == 0;
    ReturnFalse:
        return false;
    }

    public bool MatchCaseFolded(string caseFoldedChars) {
        if (unchecked((uint)Idx) + (uint)caseFoldedChars.Length <= (uint)IndexEnd) {
            for (int i = 0; i < caseFoldedChars.Length; ++i)
                if (caseFoldedChars[i] != CaseFoldTable.FoldedChars[String[Idx + i]]) goto ReturnFalse;
            return true;
        }
        return caseFoldedChars.Length == 0;
    ReturnFalse:
        return false;
    }

    public bool SkipCaseFolded(string caseFoldedChars) {
        int newIdx = unchecked(Idx + caseFoldedChars.Length);
        if (unchecked((uint)newIdx) <= (uint)IndexEnd) {
            for (int i = 0; i < caseFoldedChars.Length; ++i)
                if (caseFoldedChars[i] != CaseFoldTable.FoldedChars[String[Idx + i]]) goto ReturnFalse;
            if (newIdx == IndexEnd) newIdx = Int32.MinValue;
            Idx = newIdx;
            ++StateTag;
            return true;
        }
        return caseFoldedChars.Length == 0;
    ReturnFalse:
        return false;
    }

    public bool Match(char[] chars, int charsIndex, int length) {
        return Skip(chars, charsIndex, length, true);
    }
    public bool Skip(char[] chars, int charsIndex, int length) {
        return Skip(chars, charsIndex, length, false);
    }
    private bool Skip(char[] chars, int charsIndex, int length, bool backtrackEvenIfCharsMatch) {
        if (charsIndex < 0)
            throw new ArgumentOutOfRangeException("charsIndex", "charsIndex is negative.");
        if (length < 0 || charsIndex > chars.Length - length)
            throw new ArgumentOutOfRangeException("length", "charsIndex or length is out of range.");
        int newIdx = unchecked(Idx + length);
        if (unchecked((uint)newIdx) <= (uint)IndexEnd) {
            for (int i = 0; i < length; ++i)
                if (chars[charsIndex + i] != String[Idx + i]) goto ReturnFalse;
            if (!backtrackEvenIfCharsMatch) {
                if (newIdx == IndexEnd) newIdx = Int32.MinValue;
                Idx = newIdx;
                ++StateTag;
                return true;
            }
            return true;
        }
        return length == 0;
    ReturnFalse:
        return false;
    }

    public Match Match(Regex regex) {
        if (Idx >= 0) return regex.Match(String, Idx, IndexEnd - Idx);
        return regex.Match("");
    }

    public bool SkipWhitespace() {
        int lineBegin = 0;
        int lineOffset = 0;
        int idx = Idx;
        int end = IndexEnd;
        if (idx >= 0) {
            char c = String[idx];
            ++idx;
            if (c > ' ') goto ReturnFalse;
            if (c == ' ') {
                if (idx != end && String[idx] > ' ') {
                    Idx = idx;
                    ++StateTag;
                    return true;
                }
                goto Loop;
            } else {
                if (c == '\r') {
                    if (idx != end && String[idx] == '\n') ++idx;
                } else if (c != '\n') goto CheckTab;
                if (idx != end && String[idx] > ' ') {
                    Idx = idx;
                    RegisterNewline();
                    return true;
                }
                goto Newline;
            CheckTab:
                if (c != '\t') goto ReturnFalse;
                goto Loop;
            }
        Newline:
            lineBegin = idx;
            ++lineOffset;
        Loop:
            for (;;) {
                if (idx != end) {
                    c = String[idx];
                    ++idx;
                    if (c != ' ') {
                        if (c != '\t') {
                            if (c == '\r') {
                                if (idx != end && String[idx] == '\n') ++idx;
                                goto Newline;
                            }
                            if (c == '\n') goto Newline;
                            // end of whitespace
                            --idx;
                            break;
                        }
                    }
                } else { // end of stream,
                    idx = Int32.MinValue;
                    break;
                }
            }
            Idx = idx;
            if (lineOffset == 0) {
                ++StateTag;
                return true;
            } else {
                RegisterNewLineBegin(lineBegin, lineOffset);
                return true;
            }
        }
    ReturnFalse:
        return false;
    }

    public bool SkipUnicodeWhitespace() {
        int lineBegin = 0;
        int lineOffset = 0;
        int idx = Idx;
        int end = IndexEnd;
        if (idx >= 0) {
            char c = String[idx];
            ++idx;
            if (c == ' ') goto Loop;
            if (!Text.IsWhitespace(c)) goto ReturnFalse;
            if (c <= '\r') {
                if (c == '\r') {
                    if (idx != end && String[idx] == '\n') ++idx;
                } else if (c != '\n') goto Loop;
            } else if (c < '\u2028' ? c != '\u0085' : c > '\u2029') goto Loop;
        Newline:
            lineBegin = idx;
            ++lineOffset;
        Loop:
            for (;;) {
                if (idx != end) {
                    c = String[idx];
                    ++idx;
                    if (c != ' ') {
                        if (Text.IsWhitespace(c)) {
                            if (c <= '\r') {
                                if (c == '\r') {
                                    if (idx != end && String[idx] == '\n') ++idx;
                                    goto Newline;
                                }
                                if (c == '\n') goto Newline;
                            } else if (c < '\u2028' ? c == '\u0085' : c <= '\u2029') goto Newline;
                        } else { // end of whitespace
                            --idx;
                            break;
                        }
                    }
                } else { // end of stream
                    idx = Int32.MinValue;
                    break;
                }
            }
            Idx = idx;
            if (lineOffset == 0) {
                ++StateTag;
                return true;
            } else {
                RegisterNewLineBegin(lineBegin, lineOffset);
                return true;
            }
        }
    ReturnFalse:
        return false;
    }

     public bool SkipNewline() {
        int idx = Idx;
        if (idx >= 0) {
            char c = String[idx];
            ++idx;
            if (c == '\r') {
                if (idx != IndexEnd && String[idx] == '\n') ++idx;
            } else if (c != '\n') goto ReturnFalse;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            RegisterNewline();
            return true;
        }
ReturnFalse:
        return false;
    }

    public bool SkipUnicodeNewline() {
        int idx = Idx;
        if (idx >= 0) {
            char c = String[idx];
            ++idx;
            if (c < '\u0085') {
                if (c == '\r') {
                    if (idx != IndexEnd && String[idx] == '\n') ++idx;
                } else if (c != '\n') goto ReturnFalse;
            } else if (c >= '\u2028' ? c > '\u2029' : c != '\u0085') goto ReturnFalse;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            RegisterNewline();
            return true;
        }
    ReturnFalse:
        return false;
    }

    public int SkipNewlineThenWhitespace(int powerOf2TabStopDistance, bool allowFormFeed) {
        int tabStopDistanceMinus1 = unchecked(powerOf2TabStopDistance - 1);
        if (powerOf2TabStopDistance <= 0 || (powerOf2TabStopDistance & tabStopDistanceMinus1) != 0)
            throw new ArgumentOutOfRangeException("powerOf2TabStopDistance", "powerOf2TabStopDistance must be a positive power of 2.");

        int lineBegin = 0;
        int lineOffset = 0;
        int idx = Idx;
        int indexEnd = IndexEnd;
        char c = '\u0000';
        if (idx >= 0) c = String[idx];
        ++idx;
        if (c == '\r') {
            if (idx != indexEnd && String[idx] == '\n') ++idx;
        } else if (c != '\n') {
            return -1;
        }
    Newline:
        lineBegin = idx;
        ++lineOffset;
        int ind = 0;
        for (;;) {
            if (idx != indexEnd) {
                c = String[idx];
                ++idx;
                if (c == ' ') {
                    ind = unchecked(ind + 1);
                    if (ind >= 0) continue;
                    // indentation has overflown, so put back ' ' and return
                    ind = unchecked(ind - 1);
                } else if (c <= '\r') {
                    if (c == '\r') {
                        if (idx != indexEnd && String[idx] == '\n') ++idx;
                        goto Newline;
                    }
                    if (c == '\n') goto Newline;
                    if (c == '\t') {
                        // ind = ind + tabStopDistance - ind%tabStopDistance
                        int d = tabStopDistanceMinus1 - (ind & tabStopDistanceMinus1);
                        ind = unchecked(ind + d + 1);
                        if (ind >= 0) continue;
                        // indentation has overflown, so put back '\t' and return
                        ind = unchecked(ind - d - 1);
                    } else if (c == '\f' && allowFormFeed) {
                        ind = 0;
                        continue;
                    }
                }
                // end of indentation
                --idx;
                break;
            } else {
                // end of stream;
                idx = Int32.MinValue;
                break;
            }
        }
        Idx = idx;
        RegisterNewLineBegin(lineBegin, lineOffset);
        return ind;
    }

    public void SkipRestOfLine(bool skipNewline) {
        int idx = Idx;
        int indexEnd = IndexEnd;
        if (idx >= 0) {
            for (;;) {
                char c = String[idx];
                if (c > '\r') {
                    if (++idx == indexEnd) break;
                } else if (c != '\r' && c != '\n') {
                    if (++idx == indexEnd) break;
                } else {
                    if (!skipNewline) {
                        if (idx != Idx) {
                            Idx = idx;
                            ++StateTag;
                        }
                        return;
                    } else {
                        ++idx;
                        if (c == '\r' && idx != indexEnd && String[idx] == '\n') ++idx;
                        if (idx == indexEnd) idx = Int32.MinValue;
                        Idx = idx;
                        RegisterNewline();
                        return;
                    }
                }
            }
            // idx == indexEnd
            {
                Idx = Int32.MinValue;
                ++StateTag;
            }
        }
    }

    public string ReadRestOfLine(bool skipNewline) {
        int idx = Idx;
        int indexEnd = IndexEnd;
        if (idx >= 0) {
            for (;;) {
                char c = String[idx];
                if (c > '\r') {
                    if (++idx == indexEnd) break;
                } else if (c != '\r' && c != '\n') {
                    if (++idx == indexEnd) break;
                } else {
                    int idx0 = Idx;
                    if (!skipNewline) {
                        if (idx != idx0) {
                            Idx = idx;
                            ++StateTag;
                            return String.Substring(idx0, idx - idx0);
                        } else {
                            return "";
                        }
                    } else {
                        var skippedString = idx == idx0 ? "" : String.Substring(idx0, idx - idx0);
                        ++idx;
                        if (c == '\r' && idx != indexEnd && String[idx] == '\n') ++idx;
                        if (idx == indexEnd) idx = Int32.MinValue;
                        Idx = idx;
                        RegisterNewline();
                        return skippedString;
                    }
                }
            }
            // idx == indexEnd
            {
                int idx0 = Idx;
                Idx = Int32.MinValue;
                ++StateTag;
                return String.Substring(idx0, indexEnd - idx0);
            }
        }
        return "";
    }

    public char ReadCharOrNewline() {
        int idx = Idx;
        if (idx >= 0) {
            char c = String[idx];
            ++idx;
            if (c != '\r') {
                if (c != '\n') {
                    if (idx == IndexEnd) idx = Int32.MinValue;
                    Idx = idx;
                    ++StateTag;
                    return c;
                }
            } else if (idx != IndexEnd && String[idx] == '\n') ++idx;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            RegisterNewline();
            return '\n';
        }
        return EOS;
    }

    public int SkipCharsOrNewlines(int maxCharsOrNewlines) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int idx = Idx;
        if (idx >= 0 && maxCharsOrNewlines > 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)IndexEnd ? IndexEnd : (int)end2;
            for (;;) {
                if (idx >= end) break;
                char c = String[idx];
                ++idx;
                if (c <= '\r') {
                    if (c == '\r') {
                        if (idx != IndexEnd && String[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end != IndexEnd) ++end;
                        }
                    } else if (c != '\n') continue;
                    lineBegin = idx;
                    ++lineOffset;
                }
            }
            int count = idx - Idx - nCRLF;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            if (lineOffset == 0)
                ++StateTag;
            else
                RegisterNewLineBegin(lineBegin, lineOffset);
            return count;
        }
        return 0;
    }

    public string ReadCharsOrNewlines(int maxCharsOrNewlines, bool normalizeNewlinesInReturnString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Idx;
        int indexEnd = IndexEnd;
        if (idx >= 0 && maxCharsOrNewlines > 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)indexEnd ? indexEnd : (int)end2;
            for (;;) {
                if (idx >= end) break;
                char c = String[idx];
                ++idx;
                if (c <= '\r') {
                    if (c == '\r') {
                        if (idx != indexEnd && String[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end != indexEnd) ++end;
                        } else {
                            ++nCR;
                        }
                    } else if (c != '\n') continue;
                    lineBegin = idx;
                    ++lineOffset;
                }
            }
            int idx0 = Idx;
            int length = idx - idx0;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            if (lineOffset == 0) {
                ++StateTag;
                return String.Substring(idx0, length);
            } else {
                RegisterNewLineBegin(lineBegin, lineOffset);
                return   !normalizeNewlinesInReturnString || (nCR | nCRLF) == 0
                       ? String.Substring(idx0, length)
                       : Text.CopyWithNormalizedNewlines(String, idx0, length, nCRLF, nCR);
            }
        }
        return "";
    }

    public int SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f) {
        return SkipCharsOrNewlinesWhile(f, f);
    }
    public int SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f) {
        int lineOffset = 0;
        int nCRLF = 0;
        int lineBegin = 0;
        int idx = Idx;
        int end = IndexEnd;
        if (idx >= 0) {
            char c = String[idx];
            ++idx;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx != end && String[idx] == '\n') {
                    ++idx;
                    ++nCRLF;
                }
                lineBegin = idx;
                ++lineOffset;
            } else {
                if (!f1.Invoke(c)) goto ReturnEmpty;
                if (c == '\n') {
                    lineBegin = idx;
                    ++lineOffset;
                }
            }
            for (;;) {
                if (idx == end) goto ReturnCount;
                c = String[idx];
                ++idx;
                if (c > '\r') {
                    if (!f.Invoke(c)) break;
                } else if (c == '\r') {
                    if (!f.Invoke('\n')) break;
                    if (idx != end && String[idx] == '\n') {
                        ++idx;
                        ++nCRLF;
                    }
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    if (!f.Invoke(c)) break;
                    if (c == '\n') {
                        lineBegin = idx;
                        ++lineOffset;
                    }
                }
            }
            --idx;
        ReturnCount:
            int count = idx - Idx - nCRLF;
            if (idx == IndexEnd) idx = Int32.MinValue;
            Idx = idx;
            if (lineOffset == 0)
                ++StateTag;
            else
                RegisterNewLineBegin(lineBegin, lineOffset);
            return count;
        }
    ReturnEmpty:
        return 0;
    }

    public string ReadCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, bool normalizeNewlines) {
        return ReadCharsOrNewlinesWhile(f, f, normalizeNewlines);
    }
    public string ReadCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, bool normalizeNewlinesInReturnString) {
        int lineOffset = 0;
        int nCR = 0;
        int nCRLF = 0;
        int lineBegin = 0;
        int idx = Idx;
        int indexEnd = IndexEnd;
        if (idx >= 0) {
            char c = String[idx];
            ++idx;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx != indexEnd && String[idx] == '\n') {
                    ++idx;
                    ++nCRLF;
                } else {
                    ++nCR;
                }
                lineBegin = idx;
                ++lineOffset;
            } else {
                if (!f1.Invoke(c)) goto ReturnEmpty;
                if (c == '\n') {
                    lineBegin = idx;
                    ++lineOffset;
                }
            }
            for (;;) {
                if (idx == indexEnd) goto ReturnString;
                c = String[idx];
                ++idx;
                if (c > '\r') {
                    if (!f.Invoke(c)) break;
                } else if (c == '\r') {
                    if (!f.Invoke('\n')) break;
                    if (idx != indexEnd && String[idx] == '\n') {
                        ++idx;
                        ++nCRLF;
                    } else {
                        ++nCR;
                    }
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    if (!f.Invoke(c)) break;
                    if (c == '\n') {
                        lineBegin = idx;
                        ++lineOffset;
                    }
                }
            }
            --idx;
        ReturnString:
            int idx0 = Idx;
            int length = idx - idx0;
            if (idx == indexEnd) idx = Int32.MinValue;
            Idx = idx;
            if (lineOffset == 0) {
                ++StateTag;
                return String.Substring(idx0, length);
            } else {
                RegisterNewLineBegin(lineBegin, lineOffset);
                return !normalizeNewlinesInReturnString || (nCR | nCRLF) == 0
                       ? String.Substring(idx0, length)
                       : Text.CopyWithNormalizedNewlines(String, idx0, length, nCRLF, nCR);
            }
        }
    ReturnEmpty:
        return "";
    }

    public int SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines) {
        return SkipCharsOrNewlinesWhile(f, f, minCharsOrNewlines, maxCharsOrNewlines);
    }
    public int SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int idx = Idx;
        int indexEnd = IndexEnd;
        if (idx >= 0 && maxCharsOrNewlines > 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)indexEnd ? indexEnd : (int)end2;
            char c = String[idx];
            ++idx;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx != indexEnd && String[idx] == '\n') {
                    ++idx;
                    ++nCRLF;
                    if (end != indexEnd) ++end;
                }
                lineBegin = idx;
                ++lineOffset;
            } else {
                if (!f1.Invoke(c)) goto ReturnEmpty;
                if (c == '\n') {
                    lineBegin = idx;
                    ++lineOffset;
                }
            }
            for (;;) {
                if (idx >= end) goto ReturnCount;
                c = String[idx];
                ++idx;
                if (c > '\r') {
                    if (!f.Invoke(c)) break;
                } else if (c == '\r') {
                    if (!f.Invoke('\n')) break;
                    if (idx != indexEnd && String[idx] == '\n') {
                        ++idx;
                        ++nCRLF;
                        if (end != indexEnd) ++end;
                    }
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    if (!f.Invoke(c)) break;
                    if (c == '\n') {
                        lineBegin = idx;
                        ++lineOffset;
                    }
                }
            }
            --idx;
        ReturnCount:
            int count = idx - Idx - nCRLF;
            if (count >= minCharsOrNewlines) {
                if (idx == indexEnd) idx = Int32.MinValue;
                Idx = idx;
                if (lineOffset == 0)
                    ++StateTag;
                else
                    RegisterNewLineBegin(lineBegin, lineOffset);
                return count;
            }
        }
    ReturnEmpty:
        return 0;
    }

    public string ReadCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, bool normalizeNewlinesInReturnString) {
        return ReadCharsOrNewlinesWhile(f, f, minCharsOrNewlines, maxCharsOrNewlines, normalizeNewlinesInReturnString);
    }
    public string ReadCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, bool normalizeNewlinesInReturnString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Idx;
        int indexEnd = IndexEnd;
        if (idx >= 0 && maxCharsOrNewlines > 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)indexEnd ? indexEnd : (int)end2;
            char c = String[idx];
            ++idx;
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx != indexEnd && String[idx] == '\n') {
                    ++idx;
                    ++nCRLF;
                    if (end != indexEnd) ++end;
                } else {
                    ++nCR;
                }
                lineBegin = idx;
                ++lineOffset;
            } else {
                if (!f1.Invoke(c)) goto ReturnEmpty;
                if (c == '\n') {
                    lineBegin = idx;
                    ++lineOffset;
                }
            }
            for (;;) {
                if (idx >= end) goto ReturnString;
                c = String[idx];
                ++idx;
                if (c > '\r') {
                    if (!f.Invoke(c)) break;
                } else if (c == '\r') {
                    if (!f.Invoke('\n')) break;
                    if (idx != indexEnd && String[idx] == '\n') {
                        ++idx;
                        ++nCRLF;
                        if (end != indexEnd) ++end;
                    } else {
                        ++nCR;
                    }
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    if (!f.Invoke(c)) break;
                    if (c == '\n') {
                        lineBegin = idx;
                        ++lineOffset;
                    }
                }
            }
            --idx;
        ReturnString:
            int idx0 = Idx;
            int length = idx - idx0;
            if (length - nCRLF >= minCharsOrNewlines) {
                if (idx == indexEnd) idx = Int32.MinValue;
                Idx = idx;
                if (lineOffset == 0) {
                    ++StateTag;
                    return String.Substring(idx0, length);
                } else {
                    RegisterNewLineBegin(lineBegin, lineOffset);
                    return !normalizeNewlinesInReturnString || (nCR | nCRLF) == 0
                           ? String.Substring(idx0, length)
                           : Text.CopyWithNormalizedNewlines(String, idx0, length, nCRLF, nCR);
                }
            }
        }
    ReturnEmpty:
        return "";
    }

    private static bool RestOfStringEquals(string str1, int str1Index, string str2) {
        for (int i1 = str1Index + 1, i2 = 1; i2 < str2.Length; ++i1, ++i2) {
            if (str1[i1] != str2[i2]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    private static bool RestOfStringEqualsCI(string str1, int str1Index, string cfStr2) {
        char[] cftable = CaseFoldTable.FoldedChars;
        for (int i1 = str1Index + 1, i2 = 1; i2 < cfStr2.Length; ++i1, ++i2) {
            if (cftable[str1[i1]] != cfStr2[i2]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    public int SkipCharsOrNewlinesUntilString(string str, int maxCharsOrNewlines, out bool foundString) {
        if (str.Length == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        // The .NET 64-bit JIT emits inefficient code in the loop if we declare first as as char variable.
        int first = str[0];
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int idx = Idx;
        int indexEnd = IndexEnd;
        int end1 = indexEnd - str.Length;
        if (idx >= 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)indexEnd ? indexEnd : (int)end2;
            for (;;) {
                if (idx < end) {
                    char c = String[idx];
                    if (c != first) {
                        ++idx;
                        if (c > '\r' || c == '\t') continue;
                    } else {
                        if (idx <= end1 && RestOfStringEquals(String, idx, str)) {
                            foundString = true;
                            break;
                        }
                        ++idx;
                        if (c > '\r') continue;
                    }
                    if (c == '\r') {
                        if (idx != indexEnd && String[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end != indexEnd) ++end;
                        }
                    } else if (c != '\n') continue;
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    foundString = idx <= end1 && String[idx] == first && RestOfStringEquals(String, idx, str);
                    break;
                }
            }
            if (idx != Idx) {
                int count = idx - Idx - nCRLF;
                if (idx == indexEnd) idx = Int32.MinValue;
                Idx = idx;
                if (lineOffset == 0)
                    ++StateTag;
                else
                    RegisterNewLineBegin(lineBegin, lineOffset);
                return count;
            }
        } else {
            foundString = false;
        }
        return 0;
    }

    public int SkipCharsOrNewlinesUntilString(string str, int maxCharsOrNewlines, bool normalizeNewlinesInOutString, out string skippedCharsIfStringFoundOtherwiseNull) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        if (str.Length == 0) throw new ArgumentException("The string argument is empty.");
        // The .NET 64-bit JIT emits inefficient code in the loop if we declare first as as char variable.
        int first = str[0];
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Idx;
        int end1 = IndexEnd - str.Length;
        if (idx >= 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)IndexEnd ? IndexEnd : (int)end2;
            for (;;) {
                if (idx < end) {
                    char c = String[idx];
                    if (c != first) {
                        ++idx;
                        if (c > '\r' || c == '\t') continue;
                    } else {
                        if (idx <= end1 && RestOfStringEquals(String, idx, str)) break;
                        ++idx;
                        if (c > '\r') continue;
                    }
                    if (c == '\r') {
                        if (idx != IndexEnd && String[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end != IndexEnd) ++end;
                        } else {
                            ++nCR;
                        }
                    } else if (c != '\n') continue;
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    if (idx <= end1 && String[idx] == first && RestOfStringEquals(String, idx, str)) break;
                    // string not found
                    skippedCharsIfStringFoundOtherwiseNull = null;
                    if (idx != Idx) {
                        int count = idx - Idx - nCRLF;
                        if (idx == IndexEnd) idx = Int32.MinValue;
                        Idx = idx;
                        if (lineOffset == 0)
                            ++StateTag;
                        else
                            RegisterNewLineBegin(lineBegin, lineOffset);
                        return count;
                    }
                    return 0;
                }
            }
            // found string
            int idx0 = Idx;
            int length = idx - idx0;
            if (length != 0) {
                Idx = idx;
                if (lineOffset == 0) {
                    ++StateTag;
                    skippedCharsIfStringFoundOtherwiseNull = String.Substring(idx0, length);
                    return length;
                } else {
                    RegisterNewLineBegin(lineBegin, lineOffset);
                    skippedCharsIfStringFoundOtherwiseNull =
                        !normalizeNewlinesInOutString || (nCR | nCRLF) == 0
                        ? String.Substring(idx0, length)
                        : Text.CopyWithNormalizedNewlines(String, idx0, length, nCRLF, nCR);
                    return length - nCRLF;
                }
            } else {
                skippedCharsIfStringFoundOtherwiseNull = "";
            }
        } else {
            skippedCharsIfStringFoundOtherwiseNull = null;
        }
        return 0;
    }

    public int SkipCharsOrNewlinesUntilCaseFoldedString(string caseFoldedString, int maxCharsOrNewlines, out bool foundString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        if (caseFoldedString.Length == 0) throw new ArgumentException("The string argument is empty.");
        // The .NET 64-bit JIT emits inefficient code in the loop if we declare first as as char variable.
        int first = caseFoldedString[0];
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int idx = Idx;
        int end1 = IndexEnd - caseFoldedString.Length;
        char[] cftable = CaseFoldTable.FoldedChars;
        if (idx >= 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)IndexEnd ? IndexEnd : (int)end2;
            for (;;) {
                if (idx < end) {
                    char c = cftable[String[idx]];
                    if (c != first) {
                        ++idx;
                        if (c > '\r' || c == '\t') continue;
                    } else {
                        if (idx <= end1 && RestOfStringEqualsCI(String, idx, caseFoldedString)) {
                            foundString = true;
                            break;
                        }
                        ++idx;
                        if (c > '\r') continue;
                    }
                    if (c == '\r') {
                        if (idx != IndexEnd && String[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end != IndexEnd) ++end;
                        }
                    } else if (c != '\n') continue;
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    foundString = idx <= end1 && cftable[String[idx]] == first && RestOfStringEqualsCI(String, idx, caseFoldedString);
                    break;
                }
            }
            if (idx != Idx) {
                int count = idx - Idx - nCRLF;
                if (idx == IndexEnd) idx = Int32.MinValue;
                Idx = idx;
                if (lineOffset == 0)
                    ++StateTag;
                else
                    RegisterNewLineBegin(lineBegin, lineOffset);
                return count;
            }
        } else {
            foundString = false;
        }
        return 0;
    }

    public int SkipCharsOrNewlinesUntilCaseFoldedString(string caseFoldedString, int maxCharsOrNewlines, bool normalizeNewlinesInOutString, out string skippedCharsIfStringFoundOtherwiseNull) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        if (caseFoldedString.Length == 0) throw new ArgumentException("The string argument is empty.");
        // The .NET 64-bit JIT emits inefficient code in the loop if we declare first as as char variable.
        int first = caseFoldedString[0];
        int lineBegin = 0;
        int lineOffset = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Idx;
        int end1 = IndexEnd - caseFoldedString.Length;
        char[] cftable = CaseFoldTable.FoldedChars;
        if (idx >= 0) {
            uint end2 = (uint)idx + (uint)maxCharsOrNewlines;
            int end = end2 > (uint)IndexEnd ? IndexEnd : (int)end2;
            for (;;) {
                if (idx < end) {
                    char c = cftable[String[idx]];
                    if (c != first) {
                        ++idx;
                        if (c > '\r' || c == '\t') continue;
                    } else {
                        if (idx <= end1 && RestOfStringEqualsCI(String, idx, caseFoldedString)) break;
                        ++idx;
                        if (c > '\r') continue;
                    }
                    if (c == '\r') {
                        if (idx != IndexEnd && String[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end != IndexEnd) ++end;
                        } else {
                            ++nCR;
                        }
                    } else if (c != '\n') continue;
                    lineBegin = idx;
                    ++lineOffset;
                } else {
                    if (idx <= end1 && cftable[String[idx]] == first && RestOfStringEqualsCI(String, idx, caseFoldedString)) break;
                    // string not found
                    skippedCharsIfStringFoundOtherwiseNull = null;
                    if (idx != Idx) {
                        int count = idx - Idx - nCRLF;
                        if (idx == IndexEnd) idx = Int32.MinValue;
                        Idx = idx;
                        if (lineOffset == 0)
                            ++StateTag;
                        else
                            RegisterNewLineBegin(lineBegin, lineOffset);
                        return count;
                    }
                    return 0;
                }
            }
            // found string
            int idx0 = Idx;
            int length = idx - idx0;
            if (length != 0) {
                Idx = idx;
                if (lineOffset == 0) {
                    ++StateTag;
                    skippedCharsIfStringFoundOtherwiseNull = String.Substring(idx0, length);
                    return length;
                } else {
                    RegisterNewLineBegin(lineBegin, lineOffset);
                    skippedCharsIfStringFoundOtherwiseNull =
                        !normalizeNewlinesInOutString || (nCR | nCRLF) == 0
                        ? String.Substring(idx0, length)
                        : Text.CopyWithNormalizedNewlines(String, idx0, length, nCRLF, nCR);
                    return length - nCRLF;
                }
            } else {
                skippedCharsIfStringFoundOtherwiseNull = "";
            }
        } else {
            skippedCharsIfStringFoundOtherwiseNull = null;
        }
        return 0;
    }
} // class CharStream


public struct CharStreamState<TUserState> {
#if DEBUG
    internal readonly CharStream<TUserState> CharStream;
    private long Index { get { return GetIndex(CharStream); } }
#endif
    internal readonly int Idx;
#if SMALL_STATETAG
    public   readonly uint Tag;
#else
    public   readonly ulong Tag;
#endif
    public   readonly long Line;
    public   readonly long LineBegin;
    public   readonly TUserState UserState;
    public   readonly string Name;

    public CharStreamState(CharStream<TUserState> charStream) {
    #if DEBUG
        CharStream = charStream;
    #endif
        Idx       = charStream.Idx;
        Tag       = charStream.StateTag;
        Line      = charStream._Line;
        LineBegin = charStream._LineBegin;
        UserState = charStream._UserState;
        Name      = charStream._Name;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void ThrowInvalidState() {
        throw new InvalidOperationException("The CharStreamState is invalid.");
    }

    public CharStreamIndexToken IndexToken { get {
        if (Line <= 0) ThrowInvalidState(); // tests for a zero-initialized state

        return new CharStreamIndexToken(
            #if DEBUG
                CharStream,
            #endif
                Idx);
    } }

    public long GetIndex(CharStream charStreamFromWhichStateWasRetrieved) {
        if (Line <= 0) ThrowInvalidState(); // tests for a zero-initialized state
    #if DEBUG
        Debug.Assert(CharStream == charStreamFromWhichStateWasRetrieved);
    #endif
        return charStreamFromWhichStateWasRetrieved.GetIndex(Idx);
    }

    public Position GetPosition(CharStream charStreamFromWhichStateWasRetrieved) {
        if (Line <= 0) ThrowInvalidState(); // tests for a zero-initialized state
    #if DEBUG
        Debug.Assert(CharStream == charStreamFromWhichStateWasRetrieved);
    #endif
        long index = charStreamFromWhichStateWasRetrieved.GetIndex(Idx);
        return new Position(Name, index, Line, index - LineBegin + 1);
    }
}


/// <summary>Provides read‐access to a sequence of UTF‐16 chars.</summary>
public sealed class CharStream<TUserState> : CharStream {
    internal CharStream(string chars) : base(chars) {}

    public CharStream(string chars, int index, int length) : base(chars, index, length) {}

    public CharStream(string chars, int index, int length, long streamBeginIndex)
           : base(chars, index, length, streamBeginIndex) {}

    public CharStream(string path, Encoding encoding) : base(path, encoding) {}

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : base(path, encoding, detectEncodingFromByteOrderMarks) {}

    public CharStream(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength)
           : base(path, encoding, detectEncodingFromByteOrderMarks, byteBufferLength) {}

    public CharStream(Stream stream, Encoding encoding) : base(stream, encoding) {}

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding)
           : base(stream, leaveOpen, encoding) {}

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks)
           : base(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks) {}

    public CharStream(Stream stream, bool leaveOpen, Encoding encoding, bool detectEncodingFromByteOrderMarks, int byteBufferLength)
           : base(stream, leaveOpen, encoding, detectEncodingFromByteOrderMarks, byteBufferLength) {}


    internal TUserState _UserState;
    public TUserState UserState {
        get { return _UserState; }
        set { _UserState = value; ++StateTag; }
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public CharStreamState<TUserState> State { get {
        return new CharStreamState<TUserState>(this);
    } }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    private void ThrowInvalidState() {
        throw new ArgumentException("The CharStreamState is invalid.");
    }

    public void BacktrackTo(CharStreamState<TUserState> state) {
        BacktrackTo(ref state);
    }
    public void BacktrackTo(ref CharStreamState<TUserState> state) {
        if (state.Line <= 0) ThrowInvalidState(); // tests for zero-initialized states
    #if DEBUG
        Debug.Assert(this == state.CharStream);
    #endif
        Idx = state.Idx;
        Debug.Assert((Idx >= IndexBegin && Idx < IndexEnd) || Idx == Int32.MinValue);
        StateTag = state.Tag;
        _Line = state.Line;
        _LineBegin = state.LineBegin;
        _UserState = state.UserState;
        _Name      = state.Name;
    }

    public string ReadFrom(CharStreamState<TUserState> stateWhereStringBegins, bool normalizeNewlines) {
        return ReadFrom(ref stateWhereStringBegins, normalizeNewlines);
    }
    public string ReadFrom(ref CharStreamState<TUserState> state, bool normalizeNewlines) {
        if (state.Line <= 0) ThrowInvalidState(); // tests for zero-initialized states
    #if DEBUG
        Debug.Assert(this == state.CharStream);
    #endif
        var str = ReadFrom(state.Idx);
        if (!normalizeNewlines || state.Line == _Line) return str;
        return Text.NormalizeNewlines(str);
    }

    public CharStream<TSubStreamUserState> CreateSubstream<TSubStreamUserState>(CharStreamState<TUserState> stateWhereSubstreamBegins) {
        return CreateSubstream<TSubStreamUserState>(ref stateWhereSubstreamBegins);
    }
    public CharStream<TSubStreamUserState> CreateSubstream<TSubStreamUserState>(ref CharStreamState<TUserState> stateWhereSubstreamBegins) {
        if (stateWhereSubstreamBegins.Line <= 0) ThrowInvalidState(); // tests for zero-initialized states
    #if DEBUG
        Debug.Assert(this == stateWhereSubstreamBegins.CharStream);
    #endif
        int idx0 = stateWhereSubstreamBegins.Idx;
        if (unchecked((uint)idx0 > (uint)Idx))
            throw new ArgumentException("The current position of the stream must not lie before the position corresponding to the given CharStreamState.");
        var subStream = new CharStream<TSubStreamUserState>(String);
        subStream._Name = stateWhereSubstreamBegins.Name;
        subStream.Idx = idx0 == Idx ? Int32.MinValue : idx0;
        subStream.IndexBegin = idx0 < 0 ? IndexEnd : idx0;
        subStream.IndexEnd = Idx < 0 ? IndexEnd : Idx;
        subStream.StringToStreamIndexOffset = StringToStreamIndexOffset;
        subStream._Line = stateWhereSubstreamBegins.Line;
        subStream._LineBegin = stateWhereSubstreamBegins.LineBegin;
        return subStream;
    }
}

}

#endif
