// Copyright (c) Stephan Tolksdorf 2007-2009
// License: Simplified BSD License. See accompanying documentation.

#if LOW_TRUST

using System;
using System.Diagnostics;

namespace FParsec {

public sealed class State<TUserState> : IEquatable<State<TUserState>> {
    internal sealed class Data {
        public long       Line;
        public long       LineBegin;
        public TUserState UserState;
        public string     StreamName;

        public Data(long line, long lineBegin, TUserState userState, string streamName) {
            this.Line       = line;
            this.LineBegin  = lineBegin;
            this.UserState  = userState;
            this.StreamName = streamName;
        }
    }

    public CharStream.Iterator Iter;
    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    internal Data data;

    internal State(CharStream.Iterator iter, Data data) {
        this.Iter = iter;
        this.data = data;
    }

    public State(CharStream stream, TUserState userState) : this(stream, userState, "") { }

    public State(CharStream stream, TUserState userState, string streamName) {
        Iter = stream.Begin;
        data = new Data(1, Iter.Stream.StreamIndexOffset, userState, streamName);
    }

    public State(CharStream stream, Pos pos, TUserState userState) {
        Iter = stream.Seek(pos.Index); // throws for index smaller then stream.BeginIndex
        if (Iter.Index != pos.Index) throw new ArgumentOutOfRangeException("Pos.Index", "The index is too large.");
        data = new Data(pos.Line, pos.Index - pos.Column + 1, userState, pos.StreamName);
    }

    public State(CharStream.Iterator iter, long line, long column, TUserState userState)
           : this(iter, line, column, userState, "") { }

    public State(CharStream.Iterator iter, long line, long column, TUserState userState, string streamName) {
        Iter = iter;
        data = new Data(line, iter.Index - column + 1, userState, streamName);
    }

    public CharStream Stream     { get { return Iter.Stream; } }
    public string     StreamName { get { return data.StreamName; } }
    public long       Index      { get { return Iter.Index; } }
    public long       Line       { get { return data.Line; } }
    /// <summary>The stream index of the first char in the line, i.e. Index - Column + 1.</summary>
    public long       LineBegin  { get { return data.LineBegin; } }
    /// <summary>The 1-based index of the UTF16 char in the line, i.e. Index - LineBegin + 1.</summary>
    public long       Column     { get { return Iter.Index - data.LineBegin + 1; } }
    public TUserState UserState  { get { return data.UserState; } }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public State<TUserState> Next { get {
        // inline Iter.Next for the common case
        var iter = Iter;
        int idx = iter.Idx + 1;
        if (unchecked((uint)idx) < (uint)iter.Stream.IndexEnd) {
            iter.Idx = idx;
            return new State<TUserState>(iter, data);
        } else {
            return new State<TUserState>(Iter.Next, data);
        }
    } }

    public State<TUserState> Advance(int charOffset) {
        var iter = Iter;
        int idx = unchecked(iter.Idx + charOffset);
        if (charOffset >= 0 && unchecked((uint)idx) < (uint)iter.Stream.IndexEnd) {
            iter.Idx = idx;
            return new State<TUserState>(iter, data);
        }
        return new State<TUserState>(Iter.Advance(charOffset), data);
    }
    public State<TUserState> Advance(int charOffset, TUserState userState) {
        var newData = new Data(data.Line, data.LineBegin, userState, data.StreamName);
        return new State<TUserState>(Iter.Advance(charOffset), newData);
    }
    public State<TUserState> Advance(int charOffset, int lineOffset, int newColumnMinus1) {
        if (lineOffset > 0) {
            long newLineBegin = Iter.Index + charOffset - newColumnMinus1;
            var newData = new Data(data.Line + lineOffset, newLineBegin, data.UserState, data.StreamName);
            return new State<TUserState>(Iter.Advance(charOffset), newData);
        } else return Advance(charOffset);
    }
    public State<TUserState> Advance(int charOffset, int lineOffset, int newColumnMinus1, TUserState userState) {
        if (lineOffset > 0) {
            long newLineBegin = Iter.Index + charOffset - newColumnMinus1;
            var newData = new Data(data.Line + lineOffset, newLineBegin,      userState, data.StreamName);
            return new State<TUserState>(Iter.Advance(charOffset), newData);
        } else return Advance(charOffset, userState);
    }

    internal State<TUserState> AdvanceTo(int idx) {
        var iter = Iter;
        iter.Idx = idx;
        return new State<TUserState>(iter, data);
    }
    internal State<TUserState> AdvanceTo(int idx, int lineBegin) {
        var iter = Iter;
        iter.Idx = idx;
        long newLineBegin = (uint)(lineBegin - Iter.Stream.IndexBegin) + Iter.Stream.StreamIndexOffset;
        var newData = new Data(data.Line + 1, newLineBegin, data.UserState, data.StreamName);
        return new State<TUserState>(iter, newData);
    }
    internal State<TUserState> AdvanceTo(int idx, int lineBegin, int lineOffset) {
        var iter = Iter;
        iter.Idx = idx;
        long newLineBegin = (uint)(lineBegin - Iter.Stream.IndexBegin) + Iter.Stream.StreamIndexOffset;
        var newData = new Data(data.Line + lineOffset, newLineBegin, data.UserState, data.StreamName);
        return new State<TUserState>(iter, newData);
    }

    public State<TUserState> AdvanceTo(CharStream.Iterator iter) {
        return new State<TUserState>(iter, data);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, TUserState userState) {
        var newData = new Data(data.Line, data.LineBegin, userState, data.StreamName);
        return new State<TUserState>(iter, newData);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, int lineOffset, int newColumnMinus1) {
        if (lineOffset > 0) {
            var newData = new Data(data.Line + lineOffset, iter.Index - newColumnMinus1, data.UserState, data.StreamName);
            return new State<TUserState>(iter, newData);
        } else return AdvanceTo(iter);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, long lineOffset, long newColumnMinus1) {
        if (lineOffset > 0) {
            var newData = new Data(data.Line + lineOffset, iter.Index - newColumnMinus1, data.UserState, data.StreamName);
            return new State<TUserState>(iter, newData);
        } else return AdvanceTo(iter);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, uint lineOffset, uint newColumnMinus1) {
            var newData = new Data(data.Line + lineOffset, iter.Index - newColumnMinus1, data.UserState, data.StreamName);
            return new State<TUserState>(iter, newData);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, int lineOffset, int newColumnMinus1, TUserState userState) {
        if (lineOffset > 0) {
            var newData = new Data(data.Line + lineOffset, iter.Index - newColumnMinus1,      userState, data.StreamName);
            return new State<TUserState>(iter, newData);
        } else return AdvanceTo(iter);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, long lineOffset, long newColumnMinus1, TUserState userState) {
        if (lineOffset > 0) {
            var newData = new Data(data.Line + lineOffset, iter.Index - newColumnMinus1,      userState, data.StreamName);
            return new State<TUserState>(iter, newData);
        } else return AdvanceTo(iter);
    }
    public State<TUserState> AdvanceTo(CharStream.Iterator iter, uint lineOffset, uint newColumnMinus1, TUserState userState) {
            var newData = new Data(data.Line + lineOffset, iter.Index - newColumnMinus1,      userState, data.StreamName);
            return new State<TUserState>(iter, newData);
    }

    public State<TUserState> WithUserState(TUserState userState) {
        var newData = new Data(data.Line, data.LineBegin, userState, data.StreamName);
        return new State<TUserState>(Iter, newData);
    }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public Pos Pos { get {
        long index = Index;
        return new Pos(data.StreamName, index, data.Line, index - data.LineBegin + 1);
    } }

    public override bool Equals(object obj) {
        // most of the time this and obj are equal references ...
        if ((object)this == obj) return true;
        var other = obj as State<TUserState>;
        return    (object)other != null
               // ... or their iterator indices differ
               && Iter.Idx == other.Iter.Idx
               && EqualsHelper(other);
    }

    public bool Equals(State<TUserState> other) {
        return    (object)this == (object)other
               || (   (object)other != null
                   && Iter.Idx ==  other.Iter.Idx
                   && EqualsHelper(other));
    }

    public static bool operator==(State<TUserState> left, State<TUserState> right) {
        return    (object)left == (object)right
               || (   (object)left != null && (object)right != null
                   && left.Iter.Idx == right.Iter.Idx
                   && left.EqualsHelper(right));
    }
    public static bool operator!=(State<TUserState> left, State<TUserState> right) { return !(left == right); }

    private bool EqualsHelper(State<TUserState> other) {
        Data d1 = data, d2 = other.data;
        return    Iter.Stream == other.Iter.Stream
               && Object.Equals(d1.UserState, d2.UserState)
               && d1.Line == d2.Line
               && d1.LineBegin == d2.LineBegin
               && d1.StreamName == d2.StreamName;
    }

    public override int GetHashCode() { // GetHashCode() is not required to return different hash codes for unequal instances
        return Iter.Idx;               // and any change in the data members usually coincides with an iterator movement
    }

    public string ReadUntil(State<TUserState> stateAfterEndOfStream) {
        string str = Iter.ReadUntil(stateAfterEndOfStream.Iter);
        if ((object)data == (object)stateAfterEndOfStream.data) return str;
        return CharStream.NormalizeNewlines(str);
    }

    public State<TUserState> SkipWhitespace() {
        int lineBegin = 0;
        int nLines = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int end = stream.IndexEnd;
        var s = stream.String;
        if (idx >= 0) {
            char c = s[idx++];
            if (c > ' ') goto ReturnThis;
            if (c == ' ') {
                if (idx >= end) goto EndOfStream;
                if (s[idx] > ' ')
                    return AdvanceTo(idx);
            } else {
                if (c == '\r') {
                    if (idx < end && s[idx] == '\n') ++idx;
                } else if (c != '\n') goto CheckTab;
                lineBegin = idx;
                ++nLines;
                if (idx >= end) goto EndOfStream;
                if (s[idx] > ' ') return AdvanceTo(idx, idx);
                goto Loop;
            CheckTab:
                if (c != '\t') goto ReturnThis;
                if (idx >= end) goto EndOfStream;
            }
        Loop:
            for (;;) {
                c = s[idx++];
                if (c == ' ') {
                   if (idx >= end) break;
                } else {
                    if (c == '\r') {
                        if (idx < end && s[idx] == '\n') ++idx;
                    } else if (c != '\n') goto CheckTab;
                    lineBegin = idx;
                    ++nLines;
                    if (idx >= end) break; else continue;
                CheckTab:
                    if (c != '\t') {
                        --idx;
                        if (nLines == 0) return AdvanceTo(idx);
                        return AdvanceTo(idx, lineBegin, nLines);
                    }
                    if (idx >= end) break;
                }
            }
        EndOfStream:
            if (nLines == 0) return AdvanceTo(Int32.MinValue);
            return AdvanceTo(Int32.MinValue, lineBegin, nLines);
        }
    ReturnThis:
        return this;
    }

    public State<TUserState> SkipNewline() {
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int end = stream.IndexEnd;
        var s = stream.String;
        if (idx >= 0) {
            char c = s[idx++];
            if (c == '\r') {
                if (idx >= end) goto EndOfStream;
                if (s[idx] == '\n')
                    if (++idx >= end) goto EndOfStream;
            } else if (c == '\n') {
                if (idx >= end) goto EndOfStream;
            } else goto ReturnThis;
            return AdvanceTo(idx, idx);
        EndOfStream:
            return AdvanceTo(Int32.MinValue, idx);
        }
    ReturnThis:
        return this;
    }

    public State<TUserState> SkipRestOfLine(bool skipNewline) {
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int end = stream.IndexEnd;
        var s = stream.String;
        if (idx >= 0) {
            for (;;) {
                char c = s[idx];
                if (c > '\r') {
                    if (++idx >= end) break;
                } else if (c == '\r' || c == '\n') {
                    if (!skipNewline) {
                        if (idx != Iter.Idx) return AdvanceTo(idx);
                        else return this;
                    } else {
                        if (++idx >= end) goto EndOfStreamNewline;
                        if (c == '\r' && s[idx] == '\n')
                            if (++idx >= end) goto EndOfStreamNewline;
                        return AdvanceTo(idx, idx);
                    EndOfStreamNewline:
                        return AdvanceTo(Int32.MinValue, idx);
                    }
                } else if (++idx >= end) break;
            }
            return AdvanceTo(Int32.MinValue);
        }
        return this;
    }

    public State<TUserState> SkipRestOfLine(bool skipNewline, out string skippedString) {
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int end = stream.IndexEnd;
        var s = stream.String;
        if (idx >= 0) {
            for (;;) {
                char c = s[idx];
                if (c > '\r') {
                    if (++idx >= end) break;
                } else if (c == '\r' || c == '\n') {
                    int idx0 = Iter.Idx;
                    if (!skipNewline) {
                        if (idx != idx0) {
                            skippedString = s.Substring(idx0, idx - idx0);
                            return AdvanceTo(idx);
                        } else {
                            skippedString = "";
                            return this;
                        }
                    } else {
                        if (idx != idx0)
                            skippedString = s.Substring(idx0, idx - idx0);
                        else
                            skippedString = "";
                        if (++idx >= end) goto EndOfStreamNewline;
                        if (c == '\r' && s[idx] == '\n')
                            if (++idx >= end) goto EndOfStreamNewline;
                        return AdvanceTo(idx, idx);
                    EndOfStreamNewline:
                        return AdvanceTo(Int32.MinValue, idx);
                    }
                } else if (++idx >= end) break;
            }
            skippedString = s.Substring(Iter.Idx, end - Iter.Idx);
            return AdvanceTo(Int32.MinValue);
        }
        skippedString = "";
        return this;
    }

    public State<TUserState> SkipCharOrNewline() {
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int end = stream.IndexEnd;
        var s = stream.String;
        if (idx >= 0) {
            char c = s[idx++];
            if (c <= '\r') goto CheckForNewline;
        SkipNormalChar:
            if (idx < end) return AdvanceTo(idx);
            else return AdvanceTo(Int32.MinValue);
        CheckForNewline:
            if (c == '\r') {
                if (idx >= end) goto EndOfStreamNewline;
                if (s[idx] == '\n')
                    if (++idx >= end) goto EndOfStreamNewline;
            } else {
                if (c != '\n') goto SkipNormalChar;
                if (idx >= end) goto EndOfStreamNewline;
            }
            return AdvanceTo(idx, idx);
        EndOfStreamNewline:
            return AdvanceTo(Int32.MinValue, idx);
        }
        return this;
    }

    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines, out string skippedString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int nLines = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        var s = stream.String;
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            for (;;) {
                char c = s[idx++];
                if (c > '\r') {
                    if (idx >= end) break;
                } else {
                    if (c == '\r') {
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end < indexEnd) ++end;
                        } else {
                            ++nCR;
                        }
                    } else if (c != '\n') goto CheckBound;
                    lineBegin = idx;
                    ++nLines;
                CheckBound:
                    if (idx >= end) break;
                }
            }
            if (nLines == 0) {
                skippedString = s.Substring(Iter.Idx, idx - Iter.Idx);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            } else {
                if ((nCR | nCRLF) == 0)
                    skippedString = s.Substring(Iter.Idx, idx - Iter.Idx);
                else
                    skippedString = CharStream.CopyWithNormalizedNewlines(s, Iter.Idx, idx - Iter.Idx, nCRLF, nCR);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
            }
        }
        skippedString = "";
        return this;
    }

    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines) {
        int numberOfSkippedCharsOrNewlines;
        return SkipCharsOrNewlines(maxCharsOrNewlines, out numberOfSkippedCharsOrNewlines);
    }
    public State<TUserState> SkipCharsOrNewlines(int maxCharsOrNewlines, out int numberOfSkippedCharsOrNewlines) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int nLines = 0;
        int nCRLF = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        var s = stream.String;
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            for (;;) {
                char c = s[idx++];
                if (c > '\r') {
                    if (idx >= end) break;
                } else {
                    if (c == '\r') {
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end < indexEnd) ++end;
                        }
                    } else if (c != '\n') goto CheckBound;
                    lineBegin = idx;
                    ++nLines;
                CheckBound:
                    if (idx >= end) break;
                }
            }
            numberOfSkippedCharsOrNewlines = idx - Iter.Idx - nCRLF;
            if (nLines == 0) return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            else return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
        }
        numberOfSkippedCharsOrNewlines = 0;
        return this;
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f) {
        return SkipCharsOrNewlinesWhile(f, f);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f) {
        int nLines = 0;
        int lineBegin = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int end = stream.IndexEnd;
        var s = stream.String;
        if (idx >= 0) {
            char c = s[idx++];
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx < end && s[idx] == '\n') ++idx;
                lineBegin = idx;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = idx;
                ++nLines;
            } else if (!f1.Invoke(c)) goto ReturnEmpty;
            if (idx >= end) goto EndOfStream;
            for (;;) {
                c = s[idx++];
                if (c > '\r') {
                    if (!f.Invoke(c)) break;
                    if (idx >= end) goto EndOfStream;
                } else if (c == '\r') {
                    if (!f.Invoke('\n')) break;
                    if (idx < end && s[idx] == '\n') ++idx;
                    lineBegin = idx;
                    ++nLines;
                    if (idx >= end) goto EndOfStream;
                } else if (c == '\n') {
                    if (!f.Invoke('\n')) break;
                    lineBegin = idx;
                    ++nLines;
                    if (idx >= end) goto EndOfStream;
                } else {
                    if (!f.Invoke(c)) break;
                    if (idx >= end) goto EndOfStream;
                }
            }
            int newIterIdx;
            newIterIdx = --idx;
            goto ReturnState;
        EndOfStream:
            newIterIdx = Int32.MinValue;
        ReturnState:
            if (nLines == 0) return AdvanceTo(newIterIdx);
            else return AdvanceTo(newIterIdx, lineBegin, nLines);
        }
    ReturnEmpty:
        return this;
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, out string skippedString) {
        return SkipCharsOrNewlinesWhile(f, f, out skippedString);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, out string skippedString) {
        int nLines = 0;
        int nCR = 0;
        int nCRLF = 0;
        int lineBegin = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int end = stream.IndexEnd;
        var s = stream.String;
        if (idx >= 0) {
            char c = s[idx++];
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx < end && s[idx] == '\n') {
                    ++idx;
                    ++nCRLF;
                } else {
                    ++nCR;
                }
                lineBegin = idx;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = idx;
                ++nLines;
            } else {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            }
            if (idx >= end) goto EndOfStream;
            for (;;) {
                c = s[idx++];
                if (c > '\r') {
                    if (!f.Invoke(c)) break;
                    if (idx >= end) goto EndOfStream;
                } else if (c == '\r') {
                    if (!f.Invoke('\n')) break;
                    if (idx < end && s[idx] == '\n') {
                        ++idx;
                        ++nCRLF;
                    } else {
                        ++nCR;
                    }
                    lineBegin = idx;
                    ++nLines;
                    if (idx >= end) goto EndOfStream;
                } else if (c == '\n') {
                    if (!f.Invoke('\n')) break;
                    lineBegin = idx;
                    ++nLines;
                    if (idx >= end) goto EndOfStream;
                } else {
                    if (!f.Invoke(c)) break;
                    if (idx >= end) goto EndOfStream;
                }
            }
            int newIterIdx;
            newIterIdx = --idx;
            goto ReturnState;
        EndOfStream:
            newIterIdx = Int32.MinValue;
        ReturnState:
            if (nLines == 0) {
                skippedString = s.Substring(Iter.Idx, idx - Iter.Idx);
                return AdvanceTo(newIterIdx);
            } else {
                if ((nCR | nCRLF) == 0)
                    skippedString = s.Substring(Iter.Idx, idx - Iter.Idx);
                else
                    skippedString = CharStream.CopyWithNormalizedNewlines(s, Iter.Idx, idx - Iter.Idx, nCRLF, nCR);
                return AdvanceTo(newIterIdx, lineBegin, nLines);
            }
        }
    ReturnEmpty:
        skippedString = "";
        return this;
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines) {
        return SkipCharsOrNewlinesWhile(f, f, minCharsOrNewlines, maxCharsOrNewlines);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int nLines = 0;
        int nCRLF = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        var s = stream.String;
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            char c = s[idx++];
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx < indexEnd && s[idx] == '\n') {
                    ++idx;
                    ++nCRLF;
                    if (end < indexEnd) ++end;
                }
                lineBegin = idx;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = idx;
                ++nLines;
            } else if (!f1.Invoke(c)) goto ReturnEmpty;
            if (idx < end) {
                for (;;) {
                    c = s[idx];
                    if (c > '\r') {
                        if (!f.Invoke(c) || ++idx >= end) break;
                    } else if (c == '\r') {
                        if (!f.Invoke('\n')) break;
                        ++idx;
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end < indexEnd) ++end;
                        }
                        lineBegin = idx;
                        ++nLines;
                        if (idx >= end) break;
                    } else if (c == '\n') {
                        if (!f.Invoke('\n')) break;
                        ++idx;
                        lineBegin = idx;
                        ++nLines;
                        if (idx >= end) break;
                    } else {
                        if (!f.Invoke(c) || ++idx >= end) break;
                    }
                }
            }
            int idx0 = Iter.Idx;
            int length = idx - idx0;
            if (length - nCRLF < minCharsOrNewlines) goto ReturnEmpty;
            if (nLines == 0)
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            else
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
        }
    ReturnEmpty:
        return this;
    }

    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, out string skippedString) {
        return SkipCharsOrNewlinesWhile(f, f, minCharsOrNewlines, maxCharsOrNewlines, out skippedString);
    }
    public State<TUserState> SkipCharsOrNewlinesWhile(Microsoft.FSharp.Core.FSharpFunc<char,bool> f1, Microsoft.FSharp.Core.FSharpFunc<char,bool> f, int minCharsOrNewlines, int maxCharsOrNewlines, out string skippedString) {
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        int lineBegin = 0;
        int nLines = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        var s = stream.String;
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            char c = s[idx++];
            if (c > '\r') {
                if (!f1.Invoke(c)) goto ReturnEmpty;
            } else if (c == '\r') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                if (idx < indexEnd && s[idx] == '\n') {
                    ++idx;
                    ++nCRLF;
                    if (end < indexEnd) ++end;
                } else {
                    ++nCR;
                }
                lineBegin = idx;
                ++nLines;
            } else if (c == '\n') {
                if (!f1.Invoke('\n')) goto ReturnEmpty;
                lineBegin = idx;
                ++nLines;
            } else if (!f1.Invoke(c)) goto ReturnEmpty;
            if (idx < end) {
                for (;;) {
                    c = s[idx];
                    if (c > '\r') {
                        if (!f.Invoke(c) || ++idx >= end) break;
                    } else if (c == '\r') {
                        if (!f.Invoke('\n')) break;
                        ++idx;
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end < indexEnd) ++end;
                        } else {
                            ++nCR;
                        }
                        lineBegin = idx;
                        ++nLines;
                        if (idx >= end) break;
                    } else if (c == '\n') {
                        if (!f.Invoke('\n')) break;
                        ++idx;
                        lineBegin = idx;
                        ++nLines;
                        if (idx >= end) break;
                    } else {
                        if (!f.Invoke(c) || ++idx >= end) break;
                    }
                }
            }
            int idx0 = Iter.Idx;
            int length = idx - idx0;
            if (length - nCRLF < minCharsOrNewlines) goto ReturnEmpty;
            if (nLines == 0) {
                skippedString = s.Substring(idx0, length);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            } else {
                if ((nCR | nCRLF) == 0)
                    skippedString = s.Substring(idx0, length);
                else
                    skippedString = CharStream.CopyWithNormalizedNewlines(s, idx0, length, nCRLF, nCR);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
            }
        }
    ReturnEmpty:
        skippedString = "";
        return this;
    }

    private static bool RestOfStringEquals(string str1, int str1Index, string str2) {
        for (int i = 1; i < str2.Length; ++i) {
            if (str1[str1Index + i] != str2[i]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    private static bool RestOfStringEqualsCI(string str1, int str1Index, string cfStr2) {
        char[] cftable = CaseFoldTable.FoldedChars;
        for (int i = 1; i < cfStr2.Length; ++i) {
            if (cftable[str1[str1Index + i]] != cfStr2[i]) goto ReturnFalse;
        }
        return true;
    ReturnFalse:
        return false;
    }

    public State<TUserState> SkipToString(string str, int maxCharsOrNewlines, out bool foundString) {
        if (str.Length == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char first = str[0];
        int lineBegin = 0;
        int nLines = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        int end1 = indexEnd - str.Length;
        var s = stream.String;
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            for (;;) {
                char c = s[idx];
                if (c == first) goto CompareRestOfString;
            StringsNotEqual:
                ++idx;
                if (c > '\r') {
                    if (idx >= end) break;
                } else {
                    if (c == '\r') {
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            if (end < indexEnd) ++end;
                        }
                    } else if (c != '\n') goto CheckBound;
                    lineBegin = idx;
                    ++nLines;
                CheckBound:
                    if (idx >= end) break;
                }
                continue;
            CompareRestOfString:
                if (idx > end1 || !RestOfStringEquals(s, idx, str)) goto StringsNotEqual;
                foundString = true;
                goto ReturnState;
            }
        }
        // end1 might be negative too, so we can't use a single unsigned comparison
        foundString = idx >= 0 && idx <= end1 && s[idx] == first && RestOfStringEquals(s, idx, str);
    ReturnState:
        if (idx != Iter.Idx) {
            if (nLines == 0)
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            else
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
        }
        return this;
    }

    public State<TUserState> SkipToString(string str, int maxCharsOrNewlines, out string skippedString) {
        if (str.Length == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char first = str[0];
        int lineBegin = 0;
        int nLines = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        int end1 = indexEnd - str.Length;
        var s = stream.String;
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            for (;;) {
                char c = s[idx];
                if (c == first) goto CompareRestOfString;
            StringsNotEqual:
                ++idx;
                if (c > '\r') {
                    if (idx >= end) break;
                } else {
                    if (c == '\r') {
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end < indexEnd) ++end;
                        } else {
                            ++nCR;
                        }
                    } else if (c != '\n') goto CheckBound;
                    lineBegin = idx;
                    ++nLines;
                CheckBound:
                    if (idx >= end) break;
                }
                continue;
            CompareRestOfString:
                if (idx > end1 || !RestOfStringEquals(s, idx, str)) goto StringsNotEqual;
                goto FoundString;
            }
        }
        // end1 might be negative too, so we can't use a single unsigned comparison
        if (idx >= 0 && idx <= end1 && s[idx] == first && RestOfStringEquals(s, idx, str)) goto FoundString;
        skippedString = null;
        if (idx != Iter.Idx) {
            if (nLines == 0)
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            else
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
        } else return this;
    FoundString:
        int length = idx - Iter.Idx;
        if (length != 0) {
            if (nLines == 0) {
                skippedString = s.Substring(Iter.Idx, length);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            } else {
                if ((nCR | nCRLF) == 0)
                    skippedString = s.Substring(Iter.Idx, length);
                else
                    skippedString = CharStream.CopyWithNormalizedNewlines(s, Iter.Idx, length, nCRLF, nCR);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
            }
        } else {
            skippedString = "";
            return this;
        }
    }

    public State<TUserState> SkipToStringCI(string caseFoldedString, int maxCharsOrNewlines, out bool foundString) {
        if (caseFoldedString.Length == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char first = caseFoldedString[0];
        int lineBegin = 0;
        int nLines = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        int end1 = indexEnd - caseFoldedString.Length;
        var s = stream.String;
        char[] cftable = CaseFoldTable.FoldedChars;
        if (cftable == null) cftable = CaseFoldTable.Initialize();
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            for (;;) {
                char c = cftable[s[idx]];
                if (c == first) goto CompareRestOfString;
            StringsNotEqual:
                ++idx;
                if (c > '\r') {
                    if (idx >= end) break;
                } else {
                    if (c == '\r') {
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            if (end < indexEnd) ++end;
                        }
                    } else if (c != '\n') goto CheckBound;
                    lineBegin = idx;
                    ++nLines;
                CheckBound:
                    if (idx >= end) break;
                }
                continue;
            CompareRestOfString:
                if (idx > end1 || !RestOfStringEqualsCI(s, idx, caseFoldedString)) goto StringsNotEqual;
                foundString = true;
                goto ReturnState;
            }
        }
        // end1 might be negative too, so we can't use a single unsigned comparison
        foundString = idx >= 0 && idx <= end1 && cftable[s[idx]] == first && RestOfStringEqualsCI(s, idx, caseFoldedString);
    ReturnState:
        if (idx != Iter.Idx) {
            if (nLines == 0) {
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            } else {
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
            }
        }
        return this;
    }

    public State<TUserState> SkipToStringCI(string caseFoldedString, int maxCharsOrNewlines, out string skippedString) {
        if (caseFoldedString.Length == 0) throw new ArgumentException("The string argument is empty.");
        if (maxCharsOrNewlines < 0) throw new ArgumentOutOfRangeException("maxCharsOrNewlines", "maxCharsOrNewlines is negative.");
        char first = caseFoldedString[0];
        int lineBegin = 0;
        int nLines = 0;
        int nCRLF = 0;
        int nCR = 0;
        int idx = Iter.Idx;
        var stream = Iter.Stream;
        int indexEnd = stream.IndexEnd;
        int end1 = indexEnd - caseFoldedString.Length;
        var s = stream.String;
        char[] cftable = CaseFoldTable.FoldedChars;
        if (cftable == null) cftable = CaseFoldTable.Initialize();
        int end2 = unchecked (idx + maxCharsOrNewlines); // is negative if idx == Int32.MinValue
        int end = end2 >= idx && unchecked((uint)end2) <= (uint)indexEnd ? end2 : indexEnd; // is always positive
        if (unchecked((uint)idx) < (uint)end) {
            for (;;) {
                char c = cftable[s[idx]];
                if (c == first) goto CompareRestOfString;
            StringsNotEqual:
                ++idx;
                if (c > '\r') {
                    if (idx >= end) break;
                } else {
                    if (c == '\r') {
                        if (idx < indexEnd && s[idx] == '\n') {
                            ++idx;
                            ++nCRLF;
                            if (end < indexEnd) ++end;
                        } else {
                            ++nCR;
                        }
                    } else if (c != '\n') goto CheckBound;
                    lineBegin = idx;
                    ++nLines;
                CheckBound:
                    if (idx >= end) break;
                }
                continue;
            CompareRestOfString:
                if (idx > end1 || !RestOfStringEqualsCI(s, idx, caseFoldedString)) goto StringsNotEqual;
                goto FoundString;
            }
        }
        // end1 might be negative too, so we can't use a single unsigned comparison
        if (idx >= 0 && idx <= end1 && cftable[s[idx]] == first && RestOfStringEqualsCI(s, idx, caseFoldedString)) goto FoundString;
        skippedString = null;
        if (idx != Iter.Idx) {
            if (nLines == 0)
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            else
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
        } else return this;
    FoundString:
        int length = idx - Iter.Idx;
        if (length != 0) {
            if (nLines == 0) {
                skippedString = s.Substring(Iter.Idx, length);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx);
            } else {
                if ((nCR | nCRLF) == 0)
                    skippedString = s.Substring(Iter.Idx, length);
                else
                    skippedString = CharStream.CopyWithNormalizedNewlines(s, Iter.Idx, length, nCRLF, nCR);
                return AdvanceTo(idx == indexEnd ? Int32.MinValue : idx, lineBegin, nLines);
            }
        } else {
            skippedString = "";
            return this;
        }
    }

} // class State

}

#endif // LOW_TRUST