// Copyright (c) Stephan Tolksdorf 2008-2010
// License: Simplified BSD License. See accompanying documentation.

using System;
using System.Text;
using Microsoft.FSharp.Core;

namespace FParsec {

#if !LOW_TRUST
internal unsafe struct _16CharBuffer {
    public UInt64 UInt64_0;
    public UInt64 UInt64_1;
    public UInt64 UInt64_2;
    public UInt64 UInt64_3;
}
#endif

internal class Many1Chars<TUserState> : FSharpFunc<CharStream<TUserState>, Reply<string>> {
    protected FSharpFunc<CharStream<TUserState>, Reply<char>> CharParser1;
    protected FSharpFunc<CharStream<TUserState>, Reply<char>> CharParser;

    public Many1Chars(FSharpFunc<CharStream<TUserState>, Reply<char>> charParser1,
                      FSharpFunc<CharStream<TUserState>, Reply<char>> charParser)
    {
        CharParser1 = charParser1;
        CharParser = charParser;
    }

    public override Reply<string> Invoke(CharStream<TUserState> stream) {
        var reply = CharParser1.Invoke(stream);
        if (reply.Status == ReplyStatus.Ok)
            return ParseRestOfString(stream, reply.Result, reply.Error);
        else
            return new Reply<string>{Status = reply.Status, Error = reply.Error};
    }

#if !LOW_TRUST
    unsafe
#endif
    protected Reply<string> ParseRestOfString(CharStream<TUserState> stream, char firstChar, ErrorMessageList error) {
    #if LOW_TRUST
        var sb = new StringBuilder(16);
        sb.Append(firstChar);
    #else
        _16CharBuffer buffer_; // produces more efficient code on .NET than stackalloc char[16]
        char* buffer = (char*)(&buffer_);
        buffer[0] = firstChar;
        char[] chars = null;
        uint n = 1;
    #endif
        for (;;) {
            var tag = stream.StateTag;
            var reply = CharParser.Invoke(stream);
            if (reply.Status == ReplyStatus.Ok) {
                if (tag == stream.StateTag)
                    throw Internal.ParserCombinatorInInfiniteLoopHelper.CreateException("manyChars", stream);
                error = reply.Error;
            #if LOW_TRUST
                sb.Append(reply.Result);
            #else
                var i = n%16;
                if (i != 0) {
                    buffer[i] = reply.Result;
                    ++n;
                } else {
                    if (chars == null) chars = new char[32];
                    else if (n == chars.Length) {
                        var newChars = new char[2*chars.Length];
                        Array.Copy(chars, newChars, chars.Length);
                        chars = newChars;
                    }
                    for (i = 0; i < 16; ++i)
                        chars[n - 16 + i] = buffer[i];
                    buffer[0] = reply.Result;
                    ++n;
                }
            #endif
            } else if (reply.Status == ReplyStatus.Error && tag == stream.StateTag) {
                string str;
            #if LOW_TRUST
                str = sb.ToString();
            #else
                if (n <= 16) str = new String(buffer, 0, (int)n);
                else {
                    for (uint i = (n - 1) & 0x7ffffff0u; i < n; ++i)
                        chars[i] = buffer[i%16];
                    str = new string(chars, 0, (int)n);
                }
            #endif
                error = ErrorMessageList.Merge(error, reply.Error);
                return new Reply<string>{Status = ReplyStatus.Ok, Result = str, Error = error};
            } else {
                error = tag == stream.StateTag ? ErrorMessageList.Merge(error, reply.Error) : reply.Error;
                return new Reply<string>{Status = reply.Status, Error = error};
            }
        }
    }

    public FSharpFunc<CharStream<TUserState>, Reply<string>> AsFSharpFunc { get { return this; } }
}


internal class ManyChars<TUserState> : Many1Chars<TUserState> {
    public ManyChars(FSharpFunc<CharStream<TUserState>, Reply<char>> charParser1,
                     FSharpFunc<CharStream<TUserState>, Reply<char>> charParser)
           : base(charParser1, charParser) { }

    public override Reply<string> Invoke(CharStream<TUserState> stream)  {
        var tag = stream.StateTag;
        var reply = CharParser1.Invoke(stream);
        if (reply.Status == ReplyStatus.Ok)
            return ParseRestOfString(stream, reply.Result, reply.Error);
        else if (reply.Status == ReplyStatus.Error && tag == stream.StateTag)
            return new Reply<string>{Status = ReplyStatus.Ok, Result = "", Error = reply.Error};
        else
            return new Reply<string>{Status = reply.Status, Error = reply.Error};
    }
}

internal class Many1CharsTill<TUserState, TEnd, TResult> : FSharpFunc<CharStream<TUserState>, Reply<TResult>> {
    protected FSharpFunc<CharStream<TUserState>, Reply<char>> CharParser1;
    protected FSharpFunc<CharStream<TUserState>, Reply<char>> CharParser;
    protected FSharpFunc<CharStream<TUserState>, Reply<TEnd>> EndParser;
    protected OptimizedClosures.FSharpFunc<string, TEnd, TResult> Mapping;

    public Many1CharsTill(FSharpFunc<CharStream<TUserState>, Reply<char>> charParser1,
                          FSharpFunc<CharStream<TUserState>, Reply<char>> charParser,
                          FSharpFunc<CharStream<TUserState>, Reply<TEnd>> endParser,
                          FSharpFunc<string, FSharpFunc<TEnd, TResult>> mapping)
    {
        CharParser1 = charParser1;
        CharParser = charParser;
        EndParser = endParser;
        Mapping = (OptimizedClosures.FSharpFunc<string, TEnd, TResult>)(object)OptimizedClosures.FSharpFunc<string, TEnd, TResult>.Adapt(mapping);
    }

    public override Reply<TResult> Invoke(CharStream<TUserState> stream)  {
        var reply = CharParser1.Invoke(stream);
        if (reply.Status == ReplyStatus.Ok)
            return ParseRestOfString(stream, reply.Result, reply.Error);
        else
            return new Reply<TResult>{Status = reply.Status, Error = reply.Error};
    }

#if !LOW_TRUST
    unsafe
#endif
    protected Reply<TResult> ParseRestOfString(CharStream<TUserState> stream, char firstChar, ErrorMessageList error) {
    #if LOW_TRUST
        var sb = new StringBuilder(16);
        sb.Append(firstChar);
    #else
        _16CharBuffer buffer_; // produces more efficient code than stackalloc char[16]
        char* buffer = (char*)(&buffer_);
        buffer[0] = firstChar;
        char[] chars = null;
        uint n = 1;
    #endif
        for (;;) {
            var tag = stream.StateTag;
            var eReply = EndParser.Invoke(stream);
            if (eReply.Status == ReplyStatus.Error && tag == stream.StateTag) {
                var reply = CharParser.Invoke(stream);
                if (reply.Status == ReplyStatus.Ok) {
                    if (tag == stream.StateTag)
                        throw Internal.ParserCombinatorInInfiniteLoopHelper.CreateException("manyCharsTill", stream);
                    error = reply.Error;
                #if LOW_TRUST
                    sb.Append(reply.Result);
                #else
                    var i = n%16;
                    if (i != 0) {
                        buffer[i] = reply.Result;
                        ++n;
                    } else {
                        if (chars == null) chars = new char[32];
                        else if (n == chars.Length) {
                            var newChars = new char[2*chars.Length];
                            Array.Copy(chars, newChars, chars.Length);
                            chars = newChars;
                        }
                        for (i = 0; i < 16; ++i)
                            chars[n - 16 + i] = buffer[i];
                        buffer[0] = reply.Result;
                        ++n;
                    }
                #endif
                } else {
                    error = tag == stream.StateTag
                            ? ErrorMessageList.Merge(ErrorMessageList.Merge(error, eReply.Error), reply.Error)
                            : reply.Error;
                    return new Reply<TResult>{Status = reply.Status, Error = error};
                }
            } else if (eReply.Status == ReplyStatus.Ok) {
                string str;
            #if LOW_TRUST
                str = sb.ToString();
            #else
                if (n <= 16) str = new String(buffer, 0, (int)n);
                else {
                    for (uint i = (n - 1) & 0x7ffffff0; i < n; ++i)
                        chars[i] = buffer[i%16];
                    str = new string(chars, 0, (int)n);
                }
            #endif
                var result = Mapping.Invoke(str, eReply.Result);
                error = tag == stream.StateTag
                        ? ErrorMessageList.Merge(error, eReply.Error)
                        : eReply.Error;
                return new Reply<TResult>{Status = ReplyStatus.Ok, Result = result, Error = error};
            } else {
                error = tag == stream.StateTag
                        ? ErrorMessageList.Merge(error, eReply.Error)
                        : eReply.Error;
                return new Reply<TResult>{Status = eReply.Status, Error = error};
            }
        }
    }

    public FSharpFunc<CharStream<TUserState>, Reply<TResult>> AsFSharpFunc { get { return this; } }
}

internal class ManyCharsTill<TUserState, TEnd, TResult> : Many1CharsTill<TUserState, TEnd, TResult> {
    public ManyCharsTill(FSharpFunc<CharStream<TUserState>, Reply<char>> charParser1,
                         FSharpFunc<CharStream<TUserState>, Reply<char>> charParser,
                         FSharpFunc<CharStream<TUserState>, Reply<TEnd>> endParser,
                         FSharpFunc<string, FSharpFunc<TEnd, TResult>> mapping)
           : base(charParser1, charParser, endParser, mapping) { }

    public override Reply<TResult> Invoke(CharStream<TUserState> stream)  {
        var tag = stream.StateTag;
        var eReply = EndParser.Invoke(stream);
        if (eReply.Status == ReplyStatus.Error && tag == stream.StateTag) {
            var reply = CharParser1.Invoke(stream);
            if (reply.Status == ReplyStatus.Ok) {
                return ParseRestOfString(stream, reply.Result, reply.Error);
            } else {
                var error = tag == stream.StateTag
                            ? ErrorMessageList.Merge(eReply.Error, reply.Error)
                            : reply.Error;
                return new Reply<TResult>{Status = reply.Status, Error = error};
            }
        } else if (eReply.Status == ReplyStatus.Ok) {
            var result = Mapping.Invoke("", eReply.Result);
            return new Reply<TResult>{Status = ReplyStatus.Ok, Result = result, Error = eReply.Error};
        } else {
            return new Reply<TResult>{Status = eReply.Status, Error = eReply.Error};
        }
    }
}



}